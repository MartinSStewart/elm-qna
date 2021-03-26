module Backend exposing (..)

import AssocList as Dict
import Env
import Html
import Lamdera exposing (ClientId, SessionId)
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
                            { qnaSession | connections = Set.remove clientId qnaSession.connections }
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
    -> (BackendQnaSession -> ( BackendQnaSession, Cmd BackendMsg ))
    -> BackendModel
    -> ( BackendModel, Cmd BackendMsg )
updateQnaSession qnaSessionId updateFunc model =
    case Dict.get qnaSessionId model.qnaSessions of
        Just qnaSession ->
            updateFunc qnaSession
                |> Tuple.mapFirst (\a -> { model | qnaSessions = Dict.insert qnaSessionId a model.qnaSessions })

        Nothing ->
            ( model, Cmd.none )


updateQnaSession_ :
    SessionId
    -> ClientId
    -> Time.Posix
    -> LocalQnaMsg
    -> BackendQnaSession
    -> ( BackendQnaSession, List ( ClientId, ServerQnaMsg ) )
updateQnaSession_ sessionId clientId currentTime localQnaMsg qnaSession =
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
                    , Set.toList qnaSession.connections
                        |> List.map
                            (\clientId_ ->
                                if clientId == clientId_ then
                                    ( clientId_
                                    , ToggleUpvoteResponse questionId
                                    )

                                else
                                    ( clientId_, VotesChanged questionId voteCount )
                            )
                    )

                Nothing ->
                    ( qnaSession, [] )

        CreateQuestion content ->
            let
                questionId =
                    Dict.size qnaSession.questions |> QuestionId
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
            , Set.toList qnaSession.connections
                |> List.map
                    (\clientId_ ->
                        if clientId == clientId_ then
                            ( clientId_, CreateQuestionResponse questionId currentTime )

                        else
                            ( clientId_, NewQuestion questionId currentTime content )
                    )
            )

        PinQuestion hostKey questionId ->
            if qnaSession.hostKey == hostKey then
                case Dict.get questionId qnaSession.questions of
                    Just question ->
                        ( { qnaSession
                            | questions =
                                Dict.insert
                                    questionId
                                    { question | isRead = not question.isRead }
                                    qnaSession.questions
                          }
                        , Set.toList qnaSession.connections
                            |> List.map
                                (\clientId_ ->
                                    if clientId == clientId_ then
                                        ( clientId_, PinQuestionResponse questionId )

                                    else
                                        ( clientId_, QuestionPinned questionId )
                                )
                        )

                    Nothing ->
                        ( qnaSession, [] )

            else
                ( qnaSession, [] )


updateFromFrontendWithTime :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> Time.Posix
    -> ( BackendModel, Cmd BackendMsg )
updateFromFrontendWithTime sessionId clientId msg model currentTime =
    case msg of
        LocalMsgRequest qnaSessionId localQnaMsg ->
            updateQnaSession
                qnaSessionId
                (updateQnaSession_ sessionId clientId currentTime localQnaMsg
                    >> Tuple.mapSecond
                        (\effects ->
                            List.map
                                (\( clientId_, serverMsg ) ->
                                    Lamdera.sendToFrontend clientId_ (ServerMsgResponse qnaSessionId serverMsg)
                                )
                                effects
                                |> Cmd.batch
                        )
                )
                model

        GetQnaSession qnaSessionId ->
            case Dict.get qnaSessionId model.qnaSessions of
                Just qnaSession ->
                    ( { model
                        | qnaSessions =
                            Dict.insert
                                qnaSessionId
                                { qnaSession | connections = Set.insert clientId qnaSession.connections }
                                model.qnaSessions
                      }
                    , Lamdera.sendToFrontend clientId
                        (GetQnaSessionResponse
                            qnaSessionId
                            (Ok (Types.backendToFrontendQnaSession qnaSession))
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

                ( model3, hostKey ) =
                    getCryptographicKey model2
            in
            ( { model3
                | qnaSessions =
                    Dict.insert
                        qnaSessionId
                        (Types.initBackendQnaSession hostKey sessionId currentTime qnaSessionName)
                        model3.qnaSessions
              }
            , Lamdera.sendToFrontend clientId (CreateQnaSessionResponse qnaSessionId)
            )


getCryptographicKey : BackendModel -> ( BackendModel, CryptographicKey a )
getCryptographicKey model =
    ( { model | keyCounter = model.keyCounter + 1 }
    , Env.secretKey ++ String.fromInt model.keyCounter |> Sha256.sha224 |> CryptographicKey
    )
