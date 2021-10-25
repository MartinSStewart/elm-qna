module Backend exposing (app, app_, init, subscriptions, update, updateFromFrontend, updateFromFrontendWithTime)

import AssocList as Dict
import AssocSet as Set exposing (Set)
import Duration
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time as Time
import Id exposing (CryptographicKey, QnaSessionId, UserId(..))
import Lamdera
import List.Extra as List
import Network exposing (ChangeId)
import QnaSession exposing (BackendQnaSession)
import Quantity
import Question exposing (BackendQuestion, QuestionId)
import Types exposing (..)


app =
    Effect.Lamdera.backend Lamdera.broadcast Lamdera.sendToFrontend app_


app_ =
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }


subscriptions : BackendModel -> Subscription BackendOnly BackendMsg
subscriptions _ =
    Subscription.batch
        [ Effect.Lamdera.onDisconnect UserDisconnected
        , Effect.Lamdera.onConnect UserConnected
        , Time.every Duration.hour CheckSessions
        ]


init : ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
init =
    ( { qnaSessions = Dict.empty, keyCounter = 0 }, Command.none )


update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    --let
    --    _ =
    --        Debug.log "Backendupdate" msg
    --in
    case msg of
        NoOpBackendMsg ->
            ( model, Command.none )

        ToBackendWithTime sessionId clientId toBackend currentTime ->
            updateFromFrontendWithTime sessionId clientId toBackend model currentTime

        UserDisconnected _ clientId ->
            ( { model
                | qnaSessions =
                    Dict.map
                        (\_ qnaSession ->
                            { qnaSession | connections = Set.remove clientId qnaSession.connections }
                        )
                        model.qnaSessions
              }
            , Command.none
            )

        UserConnected _ clientId ->
            ( model, Effect.Lamdera.sendToFrontend clientId NewConnection )

        CheckSessions currentTime ->
            ( { model
                | qnaSessions =
                    Dict.filter
                        (\_ qnaSession ->
                            Duration.from (QnaSession.lastActivity qnaSession) currentTime
                                |> Quantity.lessThan (Duration.days 14)
                        )
                        model.qnaSessions
              }
            , Command.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    ( model, Time.now |> Effect.Task.perform (ToBackendWithTime sessionId clientId msg) )


updateQnaSession :
    CryptographicKey QnaSessionId
    -> SessionId
    -> (UserId -> BackendQnaSession -> ( BackendQnaSession, Command BackendOnly ToFrontend BackendMsg ))
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateQnaSession qnaSessionId sessionId updateFunc model =
    case Dict.get qnaSessionId model.qnaSessions of
        Just qnaSession ->
            case Dict.get sessionId qnaSession.userIds of
                Just userId ->
                    updateFunc userId qnaSession
                        |> Tuple.mapFirst
                            (\a -> { model | qnaSessions = Dict.insert qnaSessionId a model.qnaSessions })

                Nothing ->
                    ( model, Command.none )

        Nothing ->
            ( model, Command.none )


toggle : a -> Set a -> Set a
toggle value set =
    if Set.member value set then
        Set.remove value set

    else
        Set.insert value set


updateQnaSession_ :
    SessionId
    -> ClientId
    -> Time.Posix
    -> ChangeId
    -> LocalQnaMsg
    -> CryptographicKey QnaSessionId
    -> UserId
    -> BackendQnaSession
    -> ( BackendQnaSession, Command BackendOnly ToFrontend BackendMsg )
updateQnaSession_ sessionId clientId currentTime changeId localQnaMsg qnaSessionId userId qnaSession =
    case localQnaMsg of
        ToggleUpvote questionId ->
            case Dict.get questionId qnaSession.questions of
                Just question ->
                    let
                        question2 : BackendQuestion
                        question2 =
                            { question | votes = toggle sessionId question.votes }

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
                    , Set.toList qnaSession.connections
                        |> List.map
                            (\clientId_ ->
                                if clientId == clientId_ then
                                    Effect.Lamdera.sendToFrontend
                                        clientId_
                                        (LocalConfirmQnaMsgResponse qnaSessionId changeId ToggleUpvoteResponse)

                                else
                                    Effect.Lamdera.sendToFrontend
                                        clientId_
                                        (ServerMsgResponse qnaSessionId serverMsg)
                            )
                        |> Command.batch
                    )

                Nothing ->
                    ( qnaSession, Command.none )

        CreateQuestion _ content ->
            let
                questionId : QuestionId
                questionId =
                    Types.getQuestionId qnaSession.questions userId

                isTooLate =
                    case qnaSession.closingTime of
                        Just closingTime ->
                            Time.posixToMillis closingTime < Time.posixToMillis currentTime

                        Nothing ->
                            False
            in
            ( { qnaSession
                | questions =
                    if isTooLate then
                        qnaSession.questions

                    else
                        Dict.insert
                            questionId
                            { creationTime = currentTime
                            , content = content
                            , isPinned = Nothing
                            , votes = Set.empty
                            }
                            qnaSession.questions
              }
            , Set.toList qnaSession.connections
                |> List.map
                    (\clientId_ ->
                        if clientId == clientId_ then
                            Effect.Lamdera.sendToFrontend
                                clientId_
                                (LocalConfirmQnaMsgResponse qnaSessionId changeId (CreateQuestionResponse currentTime))

                        else if not isTooLate then
                            Effect.Lamdera.sendToFrontend
                                clientId_
                                (ServerMsgResponse qnaSessionId (NewQuestion questionId currentTime content))

                        else
                            Command.none
                    )
                |> Command.batch
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
                        , Set.toList qnaSession.connections
                            |> List.map
                                (\clientId_ ->
                                    if clientId == clientId_ then
                                        Effect.Lamdera.sendToFrontend
                                            clientId_
                                            (LocalConfirmQnaMsgResponse
                                                qnaSessionId
                                                changeId
                                                (PinQuestionResponse currentTime)
                                            )

                                    else
                                        Effect.Lamdera.sendToFrontend
                                            clientId_
                                            (ServerMsgResponse qnaSessionId (QuestionPinned questionId pinStatus))
                                )
                            |> Command.batch
                        )

                    Nothing ->
                        ( qnaSession, Command.none )

            else
                ( qnaSession, Command.none )

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
                , Set.toList qnaSession.connections
                    |> List.map
                        (\clientId_ ->
                            if clientId == clientId_ then
                                Effect.Lamdera.sendToFrontend
                                    clientId_
                                    (LocalConfirmQnaMsgResponse
                                        qnaSessionId
                                        changeId
                                        DeleteQuestionResponse
                                    )

                            else
                                Effect.Lamdera.sendToFrontend
                                    clientId_
                                    (ServerMsgResponse qnaSessionId (QuestionDeleted questionId))
                        )
                    |> Command.batch
                )

            else
                ( qnaSession, Command.none )

        ChangeClosingTime closingTime ->
            ( { qnaSession | closingTime = Just closingTime }
            , Set.toList qnaSession.connections
                |> List.map
                    (\clientId_ ->
                        if clientId == clientId_ then
                            Effect.Lamdera.sendToFrontend
                                clientId_
                                (LocalConfirmQnaMsgResponse
                                    qnaSessionId
                                    changeId
                                    ChangeClosingTimeResponse
                                )

                        else
                            Effect.Lamdera.sendToFrontend
                                clientId_
                                (ServerMsgResponse qnaSessionId (ClosingTimeChanged closingTime))
                    )
                |> Command.batch
            )


addOrGetUserId : BackendQnaSession -> SessionId -> ClientId -> ( BackendQnaSession, UserId )
addOrGetUserId qnaSession sessionId clientId =
    let
        newUserId =
            UserId qnaSession.connectionCounter
    in
    ( { qnaSession
        | connections = Set.insert clientId qnaSession.connections
        , userIds =
            Dict.update
                sessionId
                (Maybe.withDefault newUserId >> Just)
                qnaSession.userIds
        , connectionCounter = qnaSession.connectionCounter + 1
      }
    , Dict.get sessionId qnaSession.userIds |> Maybe.withDefault newUserId
    )


updateFromFrontendWithTime :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> Time.Posix
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontendWithTime sessionId clientId msg model currentTime =
    case msg of
        LocalMsgRequest qnaSessionId changeId localQnaMsg ->
            updateQnaSession
                qnaSessionId
                sessionId
                (updateQnaSession_ sessionId clientId currentTime changeId localQnaMsg qnaSessionId)
                model

        GetQnaSessionWithHostInvite hostSecret ->
            case Dict.toList model.qnaSessions |> List.find (Tuple.second >> .hostSecret >> (==) hostSecret) of
                Just ( qnaSessionId, qnaSession ) ->
                    let
                        ( newQnaSession, userId ) =
                            addOrGetUserId
                                { qnaSession
                                    | connections = Set.insert clientId qnaSession.connections
                                    , host = Set.insert sessionId qnaSession.host
                                }
                                sessionId
                                clientId
                    in
                    ( { model | qnaSessions = Dict.insert qnaSessionId newQnaSession model.qnaSessions }
                    , Effect.Lamdera.sendToFrontend clientId
                        (GetQnaSessionWithHostInviteResponse
                            hostSecret
                            (Ok
                                ( qnaSessionId
                                , QnaSession.backendToFrontend sessionId userId qnaSession
                                )
                            )
                        )
                    )

                Nothing ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (GetQnaSessionWithHostInviteResponse hostSecret (Err ()))
                    )

        GetQnaSession qnaSessionId ->
            case Dict.get qnaSessionId model.qnaSessions of
                Just qnaSession ->
                    let
                        ( newQnaSession, userId ) =
                            addOrGetUserId
                                { qnaSession
                                    | connections = Set.insert clientId qnaSession.connections
                                }
                                sessionId
                                clientId
                    in
                    ( { model | qnaSessions = Dict.insert qnaSessionId newQnaSession model.qnaSessions }
                    , Effect.Lamdera.sendToFrontend clientId
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
                    , Effect.Lamdera.sendToFrontend clientId (GetQnaSessionResponse qnaSessionId (Err ()))
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
            , Effect.Lamdera.sendToFrontend clientId (CreateQnaSessionResponse qnaSessionId hostSecret)
            )

        CheckIfConnectedRequest ->
            ( model, Effect.Lamdera.sendToFrontend clientId CheckIfConnectedResponse )
