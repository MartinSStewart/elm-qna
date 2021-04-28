module Tests exposing (suite)

import AssocList as Dict exposing (Dict)
import Backend
import Basics.Extra as Basics
import Duration exposing (Duration)
import Expect exposing (Expectation)
import Frontend
import Id exposing (UserId(..))
import Lamdera exposing (ClientId, SessionId)
import Network
import QnaSession
import Quantity
import Question exposing (QuestionId(..))
import Set
import String.Nonempty exposing (NonemptyString(..))
import Test exposing (..)
import Time
import Types exposing (BackendEffect(..), BackendModel, BackendMsg(..), BackendSub(..), FrontendEffect(..), FrontendModel, FrontendMsg(..), FrontendSub(..), Key(..), ToBackend(..), ToFrontend)
import Url exposing (Url)


startTime =
    Time.millisToPosix 0


suite : Test
suite =
    describe "Q&A app tests"
        [ test "Q&A session is not removed after 1 day" <|
            \_ ->
                Backend.init
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
        , test "Q&A session is removed after 14 days" <|
            \_ ->
                Backend.init
                    |> (\model ->
                            Backend.updateFromFrontendWithTime
                                "sessionId"
                                "clientId"
                                (CreateQnaSession (NonemptyString 'T' "est"))
                                model
                                (Time.millisToPosix 0)
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
                    "sessionId"
                    "clientId"
                    (Id.getShortCryptographicKey { keyCounter = 0 } |> Tuple.second)
                    startTime
                    (NonemptyString 'T' "est")
                    |> QnaSession.lastActivity
                    |> Expect.equal startTime
        , test "Create question in Q&A and then delete the session after 2 weeks" <|
            \_ ->
                init
                    |> simulateTime Duration.second
                    |> connectFrontend (unsafeUrl "https://question-and-answer.app")
                    |> (\( state, clientId ) ->
                            simulateTime Duration.second state
                                |> runFrontendMsg clientId PressedCreateQnaSession
                                |> simulateTime Duration.second
                                |> runFrontendMsg clientId PressedCopyUrl
                                |> simulateTime Duration.second
                                |> (\state2 ->
                                        let
                                            clipboard =
                                                Dict.get clientId state2.frontends
                                                    |> Maybe.map .clipboard
                                        in
                                        case Maybe.andThen Url.fromString clipboard of
                                            Nothing ->
                                                "Clipboard text was not a url. "
                                                    ++ Debug.toString clipboard
                                                    |> Expect.fail

                                            Just url ->
                                                connectFrontend url state2
                                                    |> (\( state3, clientId2 ) ->
                                                            simulateTime Duration.second state3
                                                                |> runFrontendMsg clientId2 (TypedQuestion "Hi")
                                                                |> simulateTime Duration.second
                                                                |> runFrontendMsg clientId2 PressedCreateQuestion
                                                                |> simulateTime Duration.second
                                                       )
                                                    |> checkState
                                                        (\state3 ->
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
                                                                    Dict.values state3.backend.qnaSessions
                                                                        |> List.map (\qnaSession -> qnaSession.questions)
                                                            in
                                                            if actual /= expected then
                                                                "QnA with question is missing. Instead got: "
                                                                    ++ Debug.toString actual
                                                                    |> Just

                                                            else
                                                                Nothing
                                                        )
                                                    |> fastForward (Duration.days 13)
                                                    |> simulateTime Duration.hour
                                                    |> checkState
                                                        (\state3 ->
                                                            if Dict.size state3.backend.qnaSessions == 1 then
                                                                Nothing

                                                            else
                                                                Just "QnA session was removed too early"
                                                        )
                                                    |> fastForward (Duration.days 1)
                                                    |> simulateTime Duration.hour
                                                    |> checkState
                                                        (\state3 ->
                                                            if Dict.size state3.backend.qnaSessions == 0 then
                                                                Nothing

                                                            else
                                                                Just "QnA session was not removed"
                                                        )
                                                    |> finishSimulation
                                   )
                       )
        , test "Handle disconnect and reconnect" <|
            \_ ->
                let
                    check clientId state6 =
                        case Dict.get clientId state6.frontends of
                            Just frontend ->
                                case frontend.model.remoteData of
                                    Types.InQnaSession qnaSession ->
                                        Network.localState
                                            Frontend.qnaSessionUpdate
                                            qnaSession.networkModel
                                            |> .questions
                                            |> Dict.values
                                            |> (\questions ->
                                                    case questions of
                                                        [ question ] ->
                                                            if question.otherVotes == 1 then
                                                                Nothing

                                                            else
                                                                Just "Didn't get question vote"

                                                        _ ->
                                                            Just "Wrong number of questions"
                                               )

                                    _ ->
                                        Just "Wrong state"

                            Nothing ->
                                Just "ClientId not found"
                in
                init
                    |> simulateTime Duration.second
                    |> connectFrontend (unsafeUrl "https://question-and-answer.app")
                    |> (\( state, clientId ) ->
                            simulateTime Duration.second state
                                |> runFrontendMsg clientId PressedCreateQnaSession
                                |> simulateTime Duration.second
                                |> runFrontendMsg clientId PressedCopyUrl
                                |> simulateTime Duration.second
                                |> (\state2 ->
                                        let
                                            clipboard =
                                                Dict.get clientId state2.frontends
                                                    |> Maybe.map .clipboard
                                        in
                                        case Maybe.andThen Url.fromString clipboard of
                                            Nothing ->
                                                "Clipboard text was not a url. "
                                                    ++ Debug.toString clipboard
                                                    |> Expect.fail

                                            Just url ->
                                                connectFrontend url state2
                                                    |> (\( state3, clientId2 ) ->
                                                            simulateTime Duration.second state3
                                                                |> runFrontendMsg clientId2 (TypedQuestion "Hi")
                                                                |> simulateTime Duration.second
                                                                |> disconnectFrontend clientId2
                                                                |> (\( state4, disconnectedFrontend ) ->
                                                                        simulateTime Duration.second state4
                                                                            |> reconnectFrontend disconnectedFrontend
                                                                            |> (\( state5, clientId3 ) ->
                                                                                    simulateTime Duration.second state5
                                                                                        |> runFrontendMsg clientId3 PressedCreateQuestion
                                                                                        |> simulateTime Duration.second
                                                                                        |> runFrontendMsg clientId (PressedToggleUpvote (QuestionId (UserId 1) 0))
                                                                                        |> simulateTime Duration.second
                                                                                        |> checkState (check clientId3)
                                                                                        |> finishSimulation
                                                                               )
                                                                   )
                                                       )
                                   )
                       )
        ]


unsafeUrl : String -> Url
unsafeUrl urlText =
    case Url.fromString urlText of
        Just url ->
            url

        Nothing ->
            Debug.todo ("Invalid url " ++ urlText)


type alias State =
    { backend : BackendModel
    , pendingEffects : BackendEffect
    , frontends : Dict ClientId FrontendState
    , counter : Int
    , elapsedTime : Duration
    , toBackend : List ( SessionId, ClientId, ToBackend )
    , timers : Dict Duration { msg : Time.Posix -> BackendMsg, startTime : Time.Posix }
    , testErrors : List String
    }


checkState : (State -> Maybe String) -> State -> State
checkState checkFunc state =
    case checkFunc state of
        Just error ->
            { state | testErrors = state.testErrors ++ [ error ] }

        Nothing ->
            state


finishSimulation : State -> Expectation
finishSimulation state =
    if List.isEmpty state.testErrors then
        Expect.pass

    else
        Expect.fail <| String.join "," state.testErrors


type alias FrontendState =
    { model : FrontendModel
    , sessionId : SessionId
    , pendingEffects : FrontendEffect
    , toFrontend : List ToFrontend
    , clipboard : String
    , timers : Dict Duration { msg : Time.Posix -> FrontendMsg, startTime : Time.Posix }
    }


init : State
init =
    let
        backend =
            Backend.init
    in
    { backend = backend
    , pendingEffects = Batch []
    , frontends = Dict.empty
    , counter = 0
    , elapsedTime = Quantity.zero
    , toBackend = []
    , timers = getBackendTimers startTime (Backend.subscriptions backend)
    , testErrors = []
    }


getFrontendTimers : Time.Posix -> FrontendSub -> Dict Duration { msg : Time.Posix -> FrontendMsg, startTime : Time.Posix }
getFrontendTimers currentTime frontendSub =
    case frontendSub of
        SubBatch_ batch ->
            List.foldl (\sub dict -> Dict.union (getFrontendTimers currentTime sub) dict) Dict.empty batch

        TimeEvery_ duration msg ->
            Dict.singleton duration { msg = msg, startTime = currentTime }


getBackendTimers : Time.Posix -> BackendSub -> Dict Duration { msg : Time.Posix -> BackendMsg, startTime : Time.Posix }
getBackendTimers currentTime backendSub =
    case backendSub of
        SubBatch batch ->
            List.foldl (\sub dict -> Dict.union (getBackendTimers currentTime sub) dict) Dict.empty batch

        TimeEvery duration msg ->
            Dict.singleton duration { msg = msg, startTime = currentTime }

        _ ->
            Dict.empty


getClientDisconnectSubs : BackendSub -> List (SessionId -> ClientId -> BackendMsg)
getClientDisconnectSubs backendSub =
    case backendSub of
        SubBatch batch ->
            List.foldl (\sub list -> getClientDisconnectSubs sub ++ list) [] batch

        ClientDisconnected msg ->
            [ msg ]

        _ ->
            []


connectFrontend : Url -> State -> ( State, ClientId )
connectFrontend url state =
    let
        clientId =
            "clientId " ++ String.fromInt state.counter

        ( frontend, effects ) =
            Frontend.init url FakeKey

        subscriptions =
            Frontend.subscriptions frontend
    in
    ( { state
        | frontends =
            Dict.insert
                clientId
                { model = frontend
                , sessionId = "sessionId " ++ String.fromInt (state.counter + 1)
                , pendingEffects = effects
                , toFrontend = []
                , clipboard = ""
                , timers = getFrontendTimers (Duration.addTo startTime state.elapsedTime) subscriptions
                }
                state.frontends
        , counter = state.counter + 2
      }
    , clientId
    )


disconnectFrontend : ClientId -> State -> ( State, FrontendState )
disconnectFrontend clientId state =
    case Dict.get clientId state.frontends of
        Just frontend ->
            let
                ( backend, effects ) =
                    getClientDisconnectSubs (Backend.subscriptions state.backend)
                        |> List.foldl
                            (\msg ( newBackend, newEffects ) ->
                                Backend.update (msg frontend.sessionId clientId) newBackend
                                    |> Tuple.mapSecond (\a -> Batch [ newEffects, a ])
                            )
                            ( state.backend, state.pendingEffects )
            in
            ( { state | backend = backend, pendingEffects = effects }, { frontend | toFrontend = [] } )

        Nothing ->
            Debug.todo "Invalid clientId"


reconnectFrontend : FrontendState -> State -> ( State, ClientId )
reconnectFrontend frontendState state =
    let
        clientId =
            "clientId " ++ String.fromInt state.counter
    in
    ( { state
        | frontends =
            Dict.insert clientId frontendState state.frontends
        , counter = state.counter + 1
      }
    , clientId
    )


runFrontendMsg : ClientId -> FrontendMsg -> State -> State
runFrontendMsg clientId frontendMsg state =
    let
        _ =
            if Dict.member clientId state.frontends then
                ()

            else
                Debug.todo "clientId not found in runFrontendMsg"
    in
    { state
        | frontends =
            Dict.update
                clientId
                (Maybe.map
                    (\frontend ->
                        let
                            ( model, effects ) =
                                Frontend.update frontendMsg frontend.model
                        in
                        { frontend
                            | model = model
                            , pendingEffects = Batch_ [ frontend.pendingEffects, effects ]
                        }
                    )
                )
                state.frontends
    }


animationFrame =
    Duration.seconds (1 / 60)


simulateStep : State -> State
simulateStep state =
    let
        newTime =
            Quantity.plus state.elapsedTime animationFrame

        getCompletedTimers : Dict Duration { a | startTime : Time.Posix } -> List ( Duration, { a | startTime : Time.Posix } )
        getCompletedTimers timers =
            Dict.toList timers
                |> List.filter
                    (\( duration, value ) ->
                        let
                            offset : Duration
                            offset =
                                Duration.from startTime value.startTime

                            timerLength : Float
                            timerLength =
                                Duration.inMilliseconds duration
                        in
                        Basics.fractionalModBy timerLength (state.elapsedTime |> Quantity.minus offset |> Duration.inMilliseconds)
                            > Basics.fractionalModBy timerLength (newTime |> Quantity.minus offset |> Duration.inMilliseconds)
                    )

        ( newBackend, newBackendEffects ) =
            getCompletedTimers state.timers
                |> List.foldl
                    (\( _, { msg } ) ( backend, effects ) ->
                        Backend.update
                            (msg (Duration.addTo startTime newTime))
                            backend
                            |> Tuple.mapSecond (\a -> Batch [ effects, a ])
                    )
                    ( state.backend, state.pendingEffects )
    in
    { state
        | elapsedTime = newTime
        , pendingEffects = newBackendEffects
        , backend = newBackend
        , frontends =
            Dict.map
                (\_ frontend ->
                    let
                        ( newFrontendModel, newFrontendEffects ) =
                            getCompletedTimers frontend.timers
                                |> List.foldl
                                    (\( _, { msg } ) ( frontendModel, effects ) ->
                                        Frontend.update
                                            (msg (Duration.addTo startTime newTime))
                                            frontendModel
                                            |> Tuple.mapSecond (\a -> Batch_ [ effects, a ])
                                    )
                                    ( frontend.model, frontend.pendingEffects )
                    in
                    { frontend | pendingEffects = newFrontendEffects, model = newFrontendModel }
                )
                state.frontends
    }
        |> runEffects


simulateTime : Duration -> State -> State
simulateTime duration state =
    if duration |> Quantity.lessThan Quantity.zero then
        state

    else
        simulateTime (duration |> Quantity.minus animationFrame) (simulateStep state)


fastForward : Duration -> State -> State
fastForward duration state =
    { state | elapsedTime = Quantity.plus state.elapsedTime duration }


runEffects : State -> State
runEffects state =
    let
        state2 =
            runBackendEffects state.pendingEffects (clearEffects state)

        state4 =
            Dict.foldl
                (\clientId frontend state3 ->
                    runFrontendEffects frontend.sessionId clientId frontend.pendingEffects state3
                )
                state2
                state.frontends
    in
    { state4
        | pendingEffects = flattenBackendEffect state4.pendingEffects |> Batch
        , frontends =
            Dict.map
                (\_ frontend ->
                    { frontend | pendingEffects = flattenFrontendEffect frontend.pendingEffects |> Batch_ }
                )
                state4.frontends
    }
        |> runNetwork


runNetwork : State -> State
runNetwork state =
    let
        ( backendModel, effects ) =
            List.foldl
                (\( sessionId, clientId, toBackendMsg ) ( model, effects2 ) ->
                    --let
                    --    _ =
                    --        Debug.log "updateFromFrontend" ( clientId, toBackendMsg )
                    --in
                    Backend.updateFromFrontend sessionId clientId toBackendMsg model
                        |> Tuple.mapSecond (\a -> Batch [ effects2, a ])
                )
                ( state.backend, state.pendingEffects )
                state.toBackend

        frontends =
            Dict.map
                (\clientId frontend ->
                    let
                        ( newModel, newEffects2 ) =
                            List.foldl
                                (\msg ( model, newEffects ) ->
                                    --let
                                    --    _ =
                                    --        Debug.log "Frontend.updateFromBackend" ( clientId, msg )
                                    --in
                                    Frontend.updateFromBackend msg model
                                        |> Tuple.mapSecond (\a -> Batch_ [ newEffects, a ])
                                )
                                ( frontend.model, frontend.pendingEffects )
                                frontend.toFrontend
                    in
                    { frontend
                        | model = newModel
                        , pendingEffects = Batch_ [ frontend.pendingEffects, newEffects2 ]
                        , toFrontend = []
                    }
                )
                state.frontends
    in
    { state
        | toBackend = []
        , backend = backendModel
        , pendingEffects = flattenBackendEffect effects |> Batch
        , frontends = frontends
    }


clearEffects : State -> State
clearEffects state =
    { state
        | pendingEffects = Batch []
        , frontends = Dict.map (\_ frontend -> { frontend | pendingEffects = Batch_ [] }) state.frontends
    }


runFrontendEffects : SessionId -> ClientId -> FrontendEffect -> State -> State
runFrontendEffects sessionId clientId effect state =
    case effect of
        Batch_ effects ->
            List.foldl (runFrontendEffects sessionId clientId) state effects

        SendToBackend toBackend ->
            { state | toBackend = state.toBackend ++ [ ( sessionId, clientId, toBackend ) ] }

        PushUrl _ urlText ->
            handleUrlChange urlText clientId state

        ReplaceUrl _ urlText ->
            handleUrlChange urlText clientId state

        LoadUrl urlText ->
            handleUrlChange urlText clientId state

        FileDownload _ _ _ ->
            state

        CopyToClipboard text ->
            { state
                | frontends =
                    Dict.update clientId (Maybe.map (\frontend -> { frontend | clipboard = text })) state.frontends
            }

        ScrollToBottom _ ->
            state

        Blur _ ->
            state


handleUrlChange : String -> ClientId -> State -> State
handleUrlChange urlText clientId state =
    let
        urlText_ =
            if String.startsWith "/" urlText then
                Frontend.domain ++ urlText

            else
                urlText
    in
    case Url.fromString urlText_ of
        Just url ->
            case Dict.get clientId state.frontends of
                Just frontend ->
                    let
                        ( model, effects ) =
                            Frontend.update (UrlChanged url) frontend.model
                    in
                    { state
                        | frontends =
                            Dict.insert clientId
                                { frontend
                                    | model = model
                                    , pendingEffects = Batch_ [ frontend.pendingEffects, effects ]
                                }
                                state.frontends
                    }

                Nothing ->
                    state

        Nothing ->
            state


flattenFrontendEffect : FrontendEffect -> List FrontendEffect
flattenFrontendEffect effect =
    case effect of
        Batch_ effects ->
            List.concatMap flattenFrontendEffect effects

        _ ->
            [ effect ]


flattenBackendEffect : BackendEffect -> List BackendEffect
flattenBackendEffect effect =
    case effect of
        Batch effects ->
            List.concatMap flattenBackendEffect effects

        _ ->
            [ effect ]


runBackendEffects : BackendEffect -> State -> State
runBackendEffects effect state =
    case effect of
        Batch effects ->
            List.foldl runBackendEffects state effects

        SendToFrontend clientId toFrontend ->
            { state
                | frontends =
                    Dict.update
                        clientId
                        (Maybe.map (\frontend -> { frontend | toFrontend = frontend.toFrontend ++ [ toFrontend ] }))
                        state.frontends
            }

        TimeNow msg ->
            let
                ( model, effects ) =
                    Backend.update (msg (Duration.addTo startTime state.elapsedTime)) state.backend
            in
            { state | backend = model, pendingEffects = Batch [ state.pendingEffects, effects ] }
