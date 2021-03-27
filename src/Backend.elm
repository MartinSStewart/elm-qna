module Backend exposing (..)

import AssocList as Dict
import Env
import Lamdera exposing (ClientId, SessionId)
import Network exposing (ChangeId)
import Set
import Set.Extra as Set
import Sha256
import Task
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Lamdera.onDisconnect UserDisconnected
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { qnaSessions = Dict.empty, keyCounter = 0 }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

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
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    ( model, Task.perform (ToBackendWithTime sessionId clientId msg) Time.now )


updateQnaSession :
    CryptographicKey QnaSessionId
    -> ClientId
    -> (UserId -> BackendQnaSession -> ( BackendQnaSession, Cmd BackendMsg ))
    -> BackendModel
    -> ( BackendModel, Cmd BackendMsg )
updateQnaSession qnaSessionId clientId updateFunc model =
    case Dict.get qnaSessionId model.qnaSessions of
        Just qnaSession ->
            case Dict.get clientId qnaSession.connections of
                Just userId ->
                    updateFunc userId qnaSession
                        |> Tuple.mapFirst
                            (\a -> { model | qnaSessions = Dict.insert qnaSessionId a model.qnaSessions })

                Nothing ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


updateQnaSession_ :
    SessionId
    -> ClientId
    -> Time.Posix
    -> ChangeId
    -> LocalQnaMsg
    -> CryptographicKey QnaSessionId
    -> UserId
    -> BackendQnaSession
    -> ( BackendQnaSession, Cmd BackendMsg )
updateQnaSession_ sessionId clientId currentTime changeId localQnaMsg qnaSessionId userId qnaSession =
    case localQnaMsg of
        ToggleUpvote questionId ->
            case Dict.get questionId qnaSession.questions of
                Just question ->
                    let
                        question2 : BackendQuestion
                        question2 =
                            { question | votes = Set.toggle sessionId question.votes }

                        voteCount : Int
                        voteCount =
                            Set.size question2.votes
                    in
                    ( { qnaSession
                        | questions = Dict.insert questionId question2 qnaSession.questions
                      }
                    , Dict.keys qnaSession.connections
                        |> List.map
                            (\clientId_ ->
                                if clientId == clientId_ then
                                    Lamdera.sendToFrontend
                                        clientId_
                                        (LocalConfirmQnaMsgResponse qnaSessionId changeId ToggleUpvoteResponse)

                                else
                                    Lamdera.sendToFrontend
                                        clientId_
                                        (ServerMsgResponse qnaSessionId (VotesChanged questionId voteCount))
                            )
                        |> Cmd.batch
                    )

                Nothing ->
                    ( qnaSession, Cmd.none )

        CreateQuestion content ->
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
                        , isRead = False
                        , votes = Set.empty
                        }
                        qnaSession.questions
              }
            , Dict.keys qnaSession.connections
                |> List.map
                    (\clientId_ ->
                        if clientId == clientId_ then
                            Lamdera.sendToFrontend
                                clientId_
                                (LocalConfirmQnaMsgResponse qnaSessionId changeId (CreateQuestionResponse currentTime))

                        else
                            Lamdera.sendToFrontend
                                clientId_
                                (ServerMsgResponse qnaSessionId (NewQuestion questionId currentTime content))
                    )
                |> Cmd.batch
            )

        PinQuestion questionId ->
            if qnaSession.host == sessionId then
                case Dict.get questionId qnaSession.questions of
                    Just question ->
                        ( { qnaSession
                            | questions =
                                Dict.insert
                                    questionId
                                    { question | isRead = not question.isRead }
                                    qnaSession.questions
                          }
                        , Dict.keys qnaSession.connections
                            |> List.map
                                (\clientId_ ->
                                    if clientId == clientId_ then
                                        Lamdera.sendToFrontend
                                            clientId_
                                            (LocalConfirmQnaMsgResponse
                                                qnaSessionId
                                                changeId
                                                PinQuestionResponse
                                            )

                                    else
                                        Lamdera.sendToFrontend
                                            clientId_
                                            (ServerMsgResponse qnaSessionId (QuestionPinned questionId))
                                )
                            |> Cmd.batch
                        )

                    Nothing ->
                        ( qnaSession, Cmd.none )

            else
                ( qnaSession, Cmd.none )


updateFromFrontendWithTime :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> Time.Posix
    -> ( BackendModel, Cmd BackendMsg )
updateFromFrontendWithTime sessionId clientId msg model currentTime =
    case msg of
        LocalMsgRequest qnaSessionId changeId localQnaMsg ->
            updateQnaSession
                qnaSessionId
                clientId
                (updateQnaSession_ sessionId clientId currentTime changeId localQnaMsg qnaSessionId)
                model

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
                                    | connections =
                                        Dict.insert
                                            clientId
                                            userId
                                            qnaSession.connections
                                    , connectionCounter = qnaSession.connectionCounter + 1
                                }
                                model.qnaSessions
                      }
                    , Lamdera.sendToFrontend clientId
                        (GetQnaSessionResponse
                            qnaSessionId
                            (Ok (Types.backendToFrontendQnaSession sessionId userId qnaSession))
                        )
                    )

                Nothing ->
                    ( model
                    , Lamdera.sendToFrontend clientId (GetQnaSessionResponse qnaSessionId (Err ()))
                    )

        CreateQnaSession qnaSessionName ->
            let
                ( model2, qnaSessionId ) =
                    getCryptographicKey model
            in
            ( { model2
                | qnaSessions =
                    Dict.insert
                        qnaSessionId
                        (Types.initBackendQnaSession sessionId currentTime qnaSessionName)
                        model2.qnaSessions
              }
            , Lamdera.sendToFrontend clientId (CreateQnaSessionResponse qnaSessionId)
            )


getCryptographicKey : BackendModel -> ( BackendModel, CryptographicKey a )
getCryptographicKey model =
    ( { model | keyCounter = model.keyCounter + 1 }
    , Env.secretKey ++ String.fromInt model.keyCounter |> Sha256.sha224 |> CryptographicKey
    )
