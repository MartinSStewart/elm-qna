module Backend exposing (app, init, subscriptions, update, updateFromFrontend, updateFromFrontendWithTime)

import AssocList as Dict
import Duration
import Id exposing (CryptographicKey, QnaSessionId, UserId(..))
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Network exposing (ChangeId)
import QnaSession exposing (BackendQnaSession)
import Quantity
import Question exposing (BackendQuestion, QuestionId)
import Set
import Set.Extra as Set
import Task
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = ( init, Cmd.none )
        , update = \msg model -> update msg model |> Tuple.mapSecond effectToCmd
        , updateFromFrontend =
            \sessionId clientId msg model ->
                updateFromFrontend sessionId clientId msg model |> Tuple.mapSecond effectToCmd
        , subscriptions = subscriptions >> backendSubToSub
        }


effectToCmd : BackendEffect -> Cmd BackendMsg
effectToCmd effect =
    case effect of
        Batch backendEffects ->
            List.map effectToCmd backendEffects |> Cmd.batch

        SendToFrontend clientId toFrontend ->
            Lamdera.sendToFrontend clientId toFrontend

        TimeNow msg ->
            Task.perform msg Time.now


backendSubToSub : BackendSub -> Sub BackendMsg
backendSubToSub backendSub =
    case backendSub of
        SubBatch backendSubs ->
            List.map backendSubToSub backendSubs |> Sub.batch

        TimeEvery duration msg ->
            Time.every (Duration.inMilliseconds duration) msg

        ClientDisconnected msg ->
            Lamdera.onDisconnect msg


subscriptions : BackendModel -> BackendSub
subscriptions _ =
    SubBatch
        [ ClientDisconnected UserDisconnected
        , TimeEvery Duration.hour CheckSessions
        ]


init : BackendModel
init =
    { qnaSessions = Dict.empty, keyCounter = 0 }


update : BackendMsg -> BackendModel -> ( BackendModel, BackendEffect )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Batch [] )

        ToBackendWithTime sessionId clientId toBackend currentTime ->
            updateFromFrontendWithTime sessionId clientId toBackend model currentTime

        UserDisconnected _ clientId ->
            ( { model
                | qnaSessions =
                    Dict.map
                        (\_ qnaSession ->
                            { qnaSession | connections = Dict.remove clientId qnaSession.connections }
                        )
                        model.qnaSessions
              }
            , Batch []
            )

        CheckSessions currentTime ->
            --( { model
            --    | qnaSessions =
            --        Dict.filter
            --            (\_ qnaSession ->
            --                Duration.from (QnaSession.lastActivity qnaSession) currentTime
            --                    |> Quantity.lessThan (Duration.days 14)
            --            )
            --            model.qnaSessions
            --  }
            ( model
            , Batch []
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, BackendEffect )
updateFromFrontend sessionId clientId msg model =
    ( model, TimeNow (ToBackendWithTime sessionId clientId msg) )


updateQnaSession :
    CryptographicKey QnaSessionId
    -> ClientId
    -> (UserId -> BackendQnaSession -> ( BackendQnaSession, BackendEffect ))
    -> BackendModel
    -> ( BackendModel, BackendEffect )
updateQnaSession qnaSessionId clientId updateFunc model =
    case Dict.get qnaSessionId model.qnaSessions of
        Just qnaSession ->
            case Dict.get clientId qnaSession.connections of
                Just userId ->
                    updateFunc userId qnaSession
                        |> Tuple.mapFirst
                            (\a -> { model | qnaSessions = Dict.insert qnaSessionId a model.qnaSessions })

                Nothing ->
                    ( model, Batch [] )

        Nothing ->
            ( model, Batch [] )


updateQnaSession_ :
    SessionId
    -> ClientId
    -> Time.Posix
    -> ChangeId
    -> LocalQnaMsg
    -> CryptographicKey QnaSessionId
    -> UserId
    -> BackendQnaSession
    -> ( BackendQnaSession, BackendEffect )
updateQnaSession_ sessionId clientId currentTime changeId localQnaMsg qnaSessionId userId qnaSession =
    case localQnaMsg of
        ToggleUpvote questionId ->
            case Dict.get questionId qnaSession.questions of
                Just question ->
                    let
                        question2 : BackendQuestion
                        question2 =
                            { question | votes = Set.toggle sessionId question.votes }

                        serverMsg : ServerQnaMsg
                        serverMsg =
                            if Set.member sessionId question.votes then
                                VoteRemoved questionId

                            else
                                VoteAdded questionId
                    in
                    ( { qnaSession
                        | questions = Dict.insert questionId question2 qnaSession.questions
                      }
                    , Dict.keys qnaSession.connections
                        |> List.map
                            (\clientId_ ->
                                if clientId == clientId_ then
                                    SendToFrontend
                                        clientId_
                                        (LocalConfirmQnaMsgResponse qnaSessionId changeId ToggleUpvoteResponse)

                                else
                                    SendToFrontend
                                        clientId_
                                        (ServerMsgResponse qnaSessionId serverMsg)
                            )
                        |> Batch
                    )

                Nothing ->
                    ( qnaSession, Batch [] )

        CreateQuestion _ content ->
            let
                questionId : QuestionId
                questionId =
                    Types.getQuestionId qnaSession.questions userId
            in
            ( { qnaSession
                | questions =
                    Dict.insert
                        questionId
                        { creationTime = currentTime
                        , content = content
                        , isPinned = Nothing
                        , votes = Set.empty
                        }
                        qnaSession.questions
              }
            , Dict.keys qnaSession.connections
                |> List.map
                    (\clientId_ ->
                        if clientId == clientId_ then
                            SendToFrontend
                                clientId_
                                (LocalConfirmQnaMsgResponse qnaSessionId changeId (CreateQuestionResponse currentTime))

                        else
                            SendToFrontend
                                clientId_
                                (ServerMsgResponse qnaSessionId (NewQuestion questionId currentTime content))
                    )
                |> Batch
            )

        TogglePin questionId _ ->
            if Set.member sessionId qnaSession.host then
                case Dict.get questionId qnaSession.questions of
                    Just question ->
                        let
                            pinStatus : Maybe Time.Posix
                            pinStatus =
                                case question.isPinned of
                                    Just _ ->
                                        Nothing

                                    Nothing ->
                                        Just currentTime
                        in
                        ( { qnaSession
                            | questions =
                                Dict.insert
                                    questionId
                                    { question | isPinned = pinStatus }
                                    qnaSession.questions
                          }
                        , Dict.keys qnaSession.connections
                            |> List.map
                                (\clientId_ ->
                                    if clientId == clientId_ then
                                        SendToFrontend
                                            clientId_
                                            (LocalConfirmQnaMsgResponse
                                                qnaSessionId
                                                changeId
                                                (PinQuestionResponse currentTime)
                                            )

                                    else
                                        SendToFrontend
                                            clientId_
                                            (ServerMsgResponse qnaSessionId (QuestionPinned questionId pinStatus))
                                )
                            |> Batch
                        )

                    Nothing ->
                        ( qnaSession, Batch [] )

            else
                ( qnaSession, Batch [] )

        DeleteQuestion questionId ->
            if Question.isCreator userId questionId then
                ( { qnaSession
                    | questions =
                        Dict.update questionId
                            (Maybe.andThen
                                (\question ->
                                    if question.isPinned == Nothing then
                                        Nothing

                                    else
                                        Just question
                                )
                            )
                            qnaSession.questions
                  }
                , Dict.keys qnaSession.connections
                    |> List.map
                        (\clientId_ ->
                            if clientId == clientId_ then
                                SendToFrontend
                                    clientId_
                                    (LocalConfirmQnaMsgResponse
                                        qnaSessionId
                                        changeId
                                        DeleteQuestionResponse
                                    )

                            else
                                SendToFrontend
                                    clientId_
                                    (ServerMsgResponse qnaSessionId (QuestionDeleted questionId))
                        )
                    |> Batch
                )

            else
                ( qnaSession, Batch [] )


updateFromFrontendWithTime :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> Time.Posix
    -> ( BackendModel, BackendEffect )
updateFromFrontendWithTime sessionId clientId msg model currentTime =
    case msg of
        LocalMsgRequest qnaSessionId changeId localQnaMsg ->
            updateQnaSession
                qnaSessionId
                clientId
                (updateQnaSession_ sessionId clientId currentTime changeId localQnaMsg qnaSessionId)
                model

        GetQnaSessionWithHostInvite hostSecret ->
            case Dict.toList model.qnaSessions |> List.find (Tuple.second >> .hostSecret >> (==) hostSecret) of
                Just ( qnaSessionId, qnaSession ) ->
                    let
                        userId =
                            UserId qnaSession.connectionCounter
                    in
                    ( { model
                        | qnaSessions =
                            Dict.insert
                                qnaSessionId
                                { qnaSession
                                    | connections = Dict.insert clientId userId qnaSession.connections
                                    , connectionCounter = qnaSession.connectionCounter + 1
                                    , host = Set.insert sessionId qnaSession.host
                                }
                                model.qnaSessions
                      }
                    , SendToFrontend clientId
                        (GetQnaSessionWithHostInviteResponse
                            hostSecret
                            (Ok ( qnaSessionId, QnaSession.backendToFrontend sessionId userId qnaSession ))
                        )
                    )

                Nothing ->
                    ( model
                    , SendToFrontend clientId (GetQnaSessionWithHostInviteResponse hostSecret (Err ()))
                    )

        GetQnaSession qnaSessionId ->
            case Dict.get qnaSessionId model.qnaSessions of
                Just qnaSession ->
                    let
                        userId =
                            UserId qnaSession.connectionCounter
                    in
                    ( { model
                        | qnaSessions =
                            Dict.insert
                                qnaSessionId
                                { qnaSession
                                    | connections = Dict.insert clientId userId qnaSession.connections
                                    , connectionCounter = qnaSession.connectionCounter + 1
                                }
                                model.qnaSessions
                      }
                    , SendToFrontend clientId
                        (GetQnaSessionResponse
                            qnaSessionId
                            (Ok
                                { isHost =
                                    if Set.member sessionId qnaSession.host then
                                        Just qnaSession.hostSecret

                                    else
                                        Nothing
                                , qnaSession = QnaSession.backendToFrontend sessionId userId qnaSession
                                }
                            )
                        )
                    )

                Nothing ->
                    ( model
                    , SendToFrontend clientId (GetQnaSessionResponse qnaSessionId (Err ()))
                    )

        CreateQnaSession qnaSessionName ->
            let
                ( model2, qnaSessionId ) =
                    Id.getShortCryptographicKey model

                ( model3, hostSecret ) =
                    Id.getShortCryptographicKey model2
            in
            ( { model3
                | qnaSessions =
                    Dict.insert
                        qnaSessionId
                        (QnaSession.initBackend sessionId clientId hostSecret currentTime qnaSessionName)
                        model2.qnaSessions
              }
            , SendToFrontend clientId (CreateQnaSessionResponse qnaSessionId hostSecret)
            )

        CheckIfConnectedRequest ->
            ( model, SendToFrontend clientId CheckIfConnectedResponse )
