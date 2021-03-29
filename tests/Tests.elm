module Tests exposing (suite)

import AssocList as Dict
import Backend
import Duration
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import String.Nonempty exposing (NonemptyString(..))
import Test exposing (..)
import Time
import Types exposing (BackendMsg(..), ToBackend(..))


startTime =
    Time.millisToPosix 0


suite : Test
suite =
    describe "Tests"
        [ test "Q&A session is not removed after 1 day" <|
            \_ ->
                Backend.init
                    |> Tuple.first
                    |> (\model ->
                            Backend.updateFromFrontendWithTime
                                "sessionId"
                                "clientId"
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
        , test "Q&A session is removed after 2 days" <|
            \_ ->
                Backend.init
                    |> Tuple.first
                    |> (\model ->
                            Backend.updateFromFrontendWithTime
                                "sessionId"
                                "clientId"
                                (CreateQnaSession (NonemptyString 'T' "est"))
                                model
                                (Time.millisToPosix 0)
                       )
                    |> Tuple.first
                    |> Backend.update (CheckSessions (Duration.addTo startTime (Duration.days 2.01)))
                    |> Tuple.first
                    |> .qnaSessions
                    |> Dict.size
                    |> Expect.equal 0
        , test "Last Q&A activity" <|
            \_ ->
                Types.initBackendQnaSession
                    "sessionId"
                    "clientId"
                    startTime
                    (NonemptyString 'T' "est")
                    |> Types.lastActivity
                    |> Expect.equal startTime
        ]