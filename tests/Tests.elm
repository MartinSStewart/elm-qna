module Tests exposing (suite)

import AssocList as Dict exposing (Dict)
import Backend
import Duration
import Expect exposing (Expectation)
import Frontend
import Id
import Lamdera exposing (ClientId, SessionId)
import QnaSession
import String.Nonempty exposing (NonemptyString(..))
import Test exposing (..)
import Time
import Types exposing (BackendEffect(..), BackendModel, BackendMsg(..), FrontendEffect(..), FrontendModel, FrontendMsg(..), Key(..), ToBackend(..), ToFrontend)
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
        , test "a" <|
            \_ ->
                init
                    |> runEffects
                    |> connectFrontend (unsafeUrl "https://question-and-answer.app")
                    |> (\( newState, clientId ) ->
                            runEffects newState
                       )
                    |> always (Expect.fail "")
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
    , time : Time.Posix
    , toBackend : List ( SessionId, ClientId, ToBackend )
    }


type alias FrontendState =
    { model : FrontendModel, sessionId : SessionId, pendingEffects : FrontendEffect, toFrontend : List ToFrontend }


init : State
init =
    { backend = Backend.init
    , pendingEffects = Batch []
    , frontends = Dict.empty
    , counter = 0
    , time = startTime
    , toBackend = []
    }


connectFrontend : Url -> State -> ( State, ClientId )
connectFrontend url state =
    let
        clientId =
            "clientId " ++ String.fromInt state.counter

        ( frontend, effects ) =
            Frontend.init url FakeKey
    in
    ( { state
        | frontends =
            Dict.insert
                clientId
                { model = frontend
                , sessionId = "sessionId " ++ String.fromInt (state.counter + 1)
                , pendingEffects = effects
                , toFrontend = []
                }
                state.frontends
        , counter = state.counter + 2
      }
    , clientId
    )


runEffects : State -> State
runEffects state =
    let
        state2 =
            runBackendEffects
                state.pendingEffects
                { state
                    | pendingEffects = Batch []
                    , frontends = Dict.map (\_ frontend -> { frontend | pendingEffects = Batch_ [] }) state.frontends
                }

        state4 =
            Dict.foldl
                (\clientId frontend state3 ->
                    runFrontendEffects frontend.sessionId clientId frontend.pendingEffects state3
                )
                state2
                state.frontends
    in
    { state4
        | pendingEffects = flattenBackendEffect state.pendingEffects |> Batch
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

        CopyToClipboard _ ->
            state

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
                    ( { state | frontends = Dict.insert clientId { frontend | model = model } state.frontends }
                    , effects
                    )

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
                    Backend.update (msg state.time) state.backend
            in
            { state | backend = model, pendingEffects = Batch [ state.pendingEffects, effects ] }
