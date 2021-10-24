module Tests exposing (suite)

import AssocList as Dict exposing (Dict)
import AssocSet as Set
import Backend
import Duration exposing (Duration)
import Effect.Http exposing (Error(..))
import Effect.Lamdera as Lamdera exposing (ClientId)
import Effect.Test exposing (TestApp)
import Effect.Time
import Expect exposing (Expectation)
import Frontend
import Id exposing (UserId(..))
import Json.Decode
import QnaSession
import Question exposing (QuestionId(..))
import String.Nonempty exposing (NonemptyString(..))
import Test exposing (..)
import Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend)
import Url exposing (Url)


startTime =
    Effect.Time.millisToPosix 0


testApp : TestApp ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
testApp =
    Effect.Test.testApp
        Frontend.app_
        Backend.app_
        (\_ -> Effect.Http.NetworkError_)
        (\_ -> Nothing)
        (\_ -> Nothing)
        ("https://" ++ Frontend.domain |> unsafeUrl)


suite : Test
suite =
    describe "Q&A app tests"
        [ test "Q&A session is not removed after 1 day" <|
            \_ ->
                Backend.init
                    |> (\( model, _ ) ->
                            Backend.updateFromFrontendWithTime
                                (Lamdera.sessionIdFromString "sessionId")
                                (Lamdera.clientIdFromString "clientId")
                                (CreateQnaSession (NonemptyString 'T' "est"))
                                model
                                startTime
                       )
                    |> Tuple.first
                    |> Backend.update (CheckSessions (Duration.addTo startTime Duration.day))
                    |> Tuple.first
                    |> .qnaSessions
                    |> Dict.size
                    |> Expect.equal 1
        , test "Q&A session is removed after 14 days" <|
            \_ ->
                Backend.init
                    |> (\( model, _ ) ->
                            Backend.updateFromFrontendWithTime
                                (Lamdera.sessionIdFromString "sessionId")
                                (Lamdera.clientIdFromString "clientId")
                                (CreateQnaSession (NonemptyString 'T' "est"))
                                model
                                (Effect.Time.millisToPosix 0)
                       )
                    |> Tuple.first
                    |> Backend.update (CheckSessions (Duration.addTo startTime (Duration.days 14.01)))
                    |> Tuple.first
                    |> .qnaSessions
                    |> Dict.size
                    |> Expect.equal 0
        , test "Last Q&A activity" <|
            \_ ->
                QnaSession.initBackend
                    (Lamdera.sessionIdFromString "sessionId")
                    (Lamdera.clientIdFromString "clientId")
                    (Id.getShortCryptographicKey { keyCounter = 0 } |> Tuple.second)
                    startTime
                    (NonemptyString 'T' "est")
                    |> QnaSession.lastActivity
                    |> Expect.equal startTime
        , test "Create question in Q&A and then delete the session after 2 weeks" <|
            \_ ->
                testApp.init
                    |> testApp.simulateTime Duration.second
                    |> testApp.connectFrontend
                        (Lamdera.sessionIdFromString "sessionId0")
                        (unsafeUrl "https://question-and-answer.app")
                        { width = 1920, height = 1080 }
                        (\( state, client1 ) ->
                            testApp.simulateTime Duration.second state
                                |> client1.clickButton Frontend.createQnaSessionButtonId
                                |> testApp.simulateTime Duration.second
                                |> client1.clickButton Frontend.copyUrlButtonId
                                |> testApp.simulateTime Duration.second
                                |> Effect.Test.andThen
                                    (\state2 ->
                                        let
                                            clipboard =
                                                getClipboard client1.clientId state2
                                        in
                                        case Url.fromString clipboard of
                                            Nothing ->
                                                Effect.Test.continueWith state2
                                                    |> Effect.Test.checkState
                                                        (\_ ->
                                                            Err ("Clipboard text was not a url. Clipboard: " ++ clipboard)
                                                        )

                                            Just url ->
                                                Effect.Test.continueWith state2
                                                    |> testApp.connectFrontend
                                                        (Lamdera.sessionIdFromString "sessionId1")
                                                        url
                                                        { width = 1920, height = 1080 }
                                                        (\( state3, client2 ) ->
                                                            testApp.simulateTime Duration.second state3
                                                                |> client2.inputText Frontend.questionInputId "Hi"
                                                                |> testApp.simulateTime Duration.second
                                                                |> client2.clickButton Frontend.createQuestionButtonId
                                                                |> testApp.simulateTime Duration.second
                                                        )
                                                    |> Effect.Test.checkBackend
                                                        (\backend ->
                                                            let
                                                                expected =
                                                                    [ Dict.fromList
                                                                        [ ( QuestionId (UserId 1) 0
                                                                          , { creationTime = Duration.addTo startTime (Duration.milliseconds 6033)
                                                                            , content = NonemptyString 'H' "i"
                                                                            , isPinned = Nothing
                                                                            , votes = Set.empty
                                                                            }
                                                                          )
                                                                        ]
                                                                    ]

                                                                actual =
                                                                    Dict.values backend.qnaSessions
                                                                        |> List.map (\qnaSession -> qnaSession.questions)
                                                            in
                                                            if actual /= expected then
                                                                "QnA with question is missing. Instead got: "
                                                                    ++ Debug.toString actual
                                                                    |> Err

                                                            else
                                                                Ok ()
                                                        )
                                                    |> Effect.Test.fastForward (Duration.days 13)
                                                    |> testApp.simulateTime Duration.hour
                                                    |> Effect.Test.checkBackend
                                                        (\backend ->
                                                            if Dict.size backend.qnaSessions == 1 then
                                                                Ok ()

                                                            else
                                                                Err "QnA session was removed too early"
                                                        )
                                                    |> Effect.Test.fastForward (Duration.days 1)
                                                    |> testApp.simulateTime Duration.hour
                                                    |> Effect.Test.checkBackend
                                                        (\backend ->
                                                            if Dict.size backend.qnaSessions == 0 then
                                                                Ok ()

                                                            else
                                                                Err "QnA session was not removed"
                                                        )
                                    )
                        )
                    |> Effect.Test.toExpectation

        --, test "Handle disconnect and reconnect" <|
        --    \_ ->
        --        let
        --            check frontend =
        --                case frontend.model.remoteData of
        --                    Types.InQnaSession qnaSession ->
        --                        Network.localState
        --                            Frontend.qnaSessionUpdate
        --                            qnaSession.networkModel
        --                            |> .questions
        --                            |> Dict.values
        --                            |> (\questions ->
        --                                    case Debug.log "questions" questions of
        --                                        [ question ] ->
        --                                            if question.otherVotes == 1 then
        --                                                Nothing
        --
        --                                            else
        --                                                Just "Didn't get question vote"
        --
        --                                        _ ->
        --                                            Just "Wrong number of questions"
        --                               )
        --
        --                    _ ->
        --                        Just "Wrong state"
        --        in
        --        init
        --            |> (\a ->
        --                    let
        --                        _ =
        --                            Debug.log "a" ""
        --                    in
        --                    a
        --               )
        --            |> testApp.simulateTime Duration.second
        --            |> (\a ->
        --                    let
        --                        _ =
        --                            Debug.log "b" ""
        --                    in
        --                    a
        --               )
        --            |> testApp.connectFrontend (unsafeUrl "https://question-and-answer.app")
        --            |> (\a ->
        --                    let
        --                        _ =
        --                            Debug.log "c" ""
        --                    in
        --                    a
        --               )
        --            |> (\( state, client1 ) ->
        --                    testApp.simulateTime Duration.second state
        --                        |> client1.clickButton { htmlId = Frontend.createQnaSessionButtonId }
        --                        |> testApp.simulateTime Duration.second
        --                        |> client1.clickButton { htmlId = Frontend.copyUrlButtonId }
        --                        |> testApp.simulateTime Duration.second
        --                        |> Effect.Test.andThen
        --                            (\state2 ->
        --                                let
        --                                    clipboard =
        --                                        Dict.get client1 state2.frontends
        --                                            |> Maybe.map .clipboard
        --                                in
        --                                case Maybe.andThen Url.fromString clipboard of
        --                                    Nothing ->
        --                                        "Clipboard text was not a url. "
        --                                            ++ Debug.toString clipboard
        --                                            |> Expect.fail
        --
        --                                    Just url ->
        --                                        testApp.connectFrontend url state2
        --                                            |> (\( state3, client2 ) ->
        --                                                    testApp.simulateTime Duration.second state3
        --                                                        |> client2.typeInput { htmlId = Frontend.questionInputId, text = "Hi" }
        --                                                        |> testApp.simulateTime Duration.second
        --                                                        |> testApp.disconnectFrontend client2
        --                                                        |> (\( state4, disconnectedFrontend ) ->
        --                                                                testApp.simulateTime Duration.second state4
        --                                                                    |> testApp.reconnectFrontend disconnectedFrontend
        --                                                                    |> (\( state5, client3 ) ->
        --                                                                            testApp.simulateTime Duration.second state5
        --                                                                                |> client3.clickButton { htmlId = Frontend.createQuestionButtonId }
        --                                                                                |> testApp.simulateTime Duration.second
        --                                                                                |> client1.clickButton { htmlId = Frontend.toggleUpvoteButtonId (QuestionId (UserId 1) 0) }
        --                                                                                |> testApp.simulateTime Duration.second
        --                                                                                |> Debug.log "abc"
        --                                                                                |> client3.checkFrontend check
        --                                                                       )
        --                                                           )
        --                                               )
        --                            )
        --               )
        --            |> Effect.Test.toExpectation
        ]


getClipboard :
    ClientId
    -> Effect.Test.State ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> String
getClipboard clientId state =
    List.filterMap
        (\portToJs ->
            case ( portToJs.clientId == clientId, Json.Decode.decodeValue Json.Decode.string portToJs.value ) of
                ( True, Ok clipboard ) ->
                    Just clipboard

                _ ->
                    Nothing
        )
        state.portRequests
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""


unsafeUrl : String -> Url
unsafeUrl urlText =
    case Url.fromString urlText of
        Just url ->
            url

        Nothing ->
            Debug.todo ("Invalid url " ++ urlText)


animationFrame =
    Duration.seconds (1 / 60)
