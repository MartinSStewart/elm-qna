port module Frontend exposing
    ( app
    , app_
    , copyHostUrlButtonId
    , copyUrlButtonId
    , createQnaSessionButtonId
    , createQuestionButtonId
    , dateInputId
    , deleteQuestionButtonId
    , domain
    , downloadQuestionsButtonId
    , initLoaded
    , qnaSessionUpdate
    , questionInputId
    , questionsViewId
    , togglePinButtonId
    , toggleUpvoteButtonId
    )

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest(..))
import Csv.Encode
import Date exposing (Date)
import Duration exposing (Duration)
import Effect.Browser.Dom as Dom exposing (HtmlId)
import Effect.Browser.Navigation as Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.File.Download
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task exposing (Task)
import Effect.Time
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Keyed
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Id exposing (CryptographicKey(..), HostSecret, QnaSessionId, UserId(..))
import Json.Decode
import Json.Encode
import Lamdera
import Network exposing (Change(..))
import QnaSession exposing (QnaSession)
import Quantity
import Question exposing (Question, QuestionId(..))
import Round
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as Property
import String.Nonempty as NonemptyString exposing (NonemptyString(..))
import Time
import Time.Extra as Time
import Types exposing (ConfirmLocalQnaMsg(..), FrontendLoaded, FrontendLoading, FrontendModel(..), FrontendMsg(..), FrontendStatus(..), HostState, InQnaSession_, LocalQnaMsg(..), Route(..), ServerQnaMsg(..), ToBackend(..), ToFrontend(..))
import Url exposing (Url)
import Url.Parser exposing ((</>))


port supermario_copy_to_clipboard_to_js : Json.Encode.Value -> Cmd msg


copyToClipboard : String -> Command FrontendOnly toMsg msg
copyToClipboard =
    Json.Encode.string >> Command.sendToJs "supermario_copy_to_clipboard_to_js" supermario_copy_to_clipboard_to_js


app =
    Effect.Lamdera.frontend Lamdera.sendToBackend app_


app_ =
    { init = init
    , onUrlRequest = UrlClicked
    , onUrlChange = UrlChanged
    , update = update
    , updateFromBackend = updateFromBackend
    , subscriptions = subscriptions
    , view = view
    }


subscriptions : FrontendModel -> Subscription FrontendOnly FrontendMsg
subscriptions _ =
    Subscription.batch
        [ Effect.Time.every Duration.second GotCurrentTime
        , Effect.Time.every (Duration.seconds 10) CheckIfConnected
        ]


init : Url.Url -> Navigation.Key -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
init url key =
    ( Loading { time = Nothing, timezone = Nothing, key = key, route = Url.Parser.parse urlDecoder url }
    , Command.batch
        [ Task.perform GotCurrentTime Effect.Time.now
        , Task.perform GotTimezone Effect.Time.here
        ]
    )


initLoaded : FrontendLoading -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
initLoaded loading =
    Maybe.map2
        (\timezone time ->
            case loading.route of
                Just (QnaSessionRoute qnaSessionId) ->
                    qnaSessionRouteInit timezone time False loading.key qnaSessionId

                Just (HostInviteRoute hostSecret) ->
                    hostInviteRouteInit timezone time False loading.key hostSecret

                Just HomepageRoute ->
                    homepageRouteInit timezone time False loading.key

                Nothing ->
                    homepageRouteInit timezone time False loading.key
        )
        loading.timezone
        loading.time
        |> Maybe.map (Tuple.mapFirst Loaded)
        |> Maybe.withDefault ( Loading loading, Command.none )


qnaSessionRouteInit :
    Effect.Time.Zone
    -> Effect.Time.Posix
    -> Bool
    -> Navigation.Key
    -> CryptographicKey QnaSessionId
    -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg )
qnaSessionRouteInit timezone time gotFirstConnectMsg key qnaSessionId =
    ( { key = key
      , remoteData = LoadingQnaSession qnaSessionId
      , timezone = timezone
      , time = time
      , lastConnectionCheck = Nothing
      , gotFirstConnectMsg = gotFirstConnectMsg
      }
    , Effect.Lamdera.sendToBackend (GetQnaSession qnaSessionId)
    )


hostInviteRouteInit :
    Effect.Time.Zone
    -> Effect.Time.Posix
    -> Bool
    -> Navigation.Key
    -> CryptographicKey HostSecret
    -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg )
hostInviteRouteInit timezone time gotFirstConnectMsg key hostSecret =
    ( { key = key
      , remoteData = LoadingQnaSessionWithHostInvite hostSecret
      , timezone = timezone
      , time = time
      , lastConnectionCheck = Nothing
      , gotFirstConnectMsg = gotFirstConnectMsg
      }
    , Effect.Lamdera.sendToBackend (GetQnaSessionWithHostInvite hostSecret)
    )


homepageRouteInit timezone time gotFirstConnectMsg key =
    ( { key = key
      , remoteData = Homepage
      , timezone = timezone
      , time = time
      , lastConnectionCheck = Nothing
      , gotFirstConnectMsg = gotFirstConnectMsg
      }
    , Command.none
    )


urlDecoder : Url.Parser.Parser (Route -> c) c
urlDecoder =
    Url.Parser.oneOf
        [ Url.Parser.top |> Url.Parser.map HomepageRoute
        , Url.Parser.s hostInvite </> Url.Parser.string |> Url.Parser.map (CryptographicKey >> HostInviteRoute)
        , Url.Parser.string |> Url.Parser.map (CryptographicKey >> QnaSessionRoute)
        ]


urlEncoder : CryptographicKey QnaSessionId -> String
urlEncoder (CryptographicKey qnaSessionId) =
    "/" ++ qnaSessionId


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case model of
        Loading loading ->
            case msg of
                GotCurrentTime time ->
                    { loading | time = Just time } |> initLoaded

                GotTimezone timezone ->
                    { loading | timezone = Just timezone } |> initLoaded

                _ ->
                    ( model, Command.none )

        Loaded loaded ->
            updateLoaded msg loaded |> Tuple.mapFirst Loaded


updateLoaded : FrontendMsg -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg )
updateLoaded msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Navigation.load url
                    )

        UrlChanged url ->
            case Url.Parser.parse urlDecoder url of
                Just HomepageRoute ->
                    ( model, Navigation.load "/" )

                Just (QnaSessionRoute qnaSessionId) ->
                    case model.remoteData of
                        InQnaSession inQnaSession ->
                            if inQnaSession.qnaSessionId == qnaSessionId then
                                ( model, Command.none )

                            else
                                ( model, Navigation.load (urlEncoder qnaSessionId) )

                        _ ->
                            ( model, Navigation.load (urlEncoder qnaSessionId) )

                _ ->
                    ( model, Command.none )

        NoOpFrontendMsg ->
            ( model, Command.none )

        PressedCreateQnaSession ->
            let
                name =
                    NonemptyString 'T' "est"
            in
            ( { model | remoteData = CreatingQnaSession name }, Effect.Lamdera.sendToBackend (CreateQnaSession name) )

        TypedQuestion text ->
            case model.remoteData of
                InQnaSession inQnaSession ->
                    ( { model | remoteData = InQnaSession { inQnaSession | question = text } }, Command.none )

                _ ->
                    ( model, Command.none )

        PressedCreateQuestion ->
            updateInQnaSession
                (\inQnaSession ->
                    case valiatedQuestion inQnaSession.question of
                        Ok nonempty ->
                            let
                                localMsg =
                                    CreateQuestion model.time nonempty
                            in
                            ( { inQnaSession
                                | networkModel =
                                    Network.updateFromUser
                                        inQnaSession.localChangeCounter
                                        localMsg
                                        inQnaSession.networkModel
                                , question = ""
                                , pressedCreateQuestion = False
                                , localChangeCounter = Network.incrementChangeId inQnaSession.localChangeCounter
                              }
                            , Command.batch
                                [ Effect.Lamdera.sendToBackend
                                    (LocalMsgRequest inQnaSession.qnaSessionId inQnaSession.localChangeCounter localMsg)
                                , Dom.getViewportOf questionsViewId
                                    |> Task.andThen
                                        (\{ scene, viewport } ->
                                            scrollToOf 200 questionsViewId (scene.height - viewport.height)
                                        )
                                    |> Task.attempt (\_ -> NoOpFrontendMsg)
                                , Dom.blur questionInputId |> Task.attempt (\_ -> TextInputBlurred)
                                ]
                            )

                        Err _ ->
                            ( { inQnaSession | pressedCreateQuestion = True }
                            , Command.none
                            )
                )
                model

        PressedToggleUpvote questionId ->
            updateInQnaSession (addLocalChange (ToggleUpvote questionId)) model

        PressedTogglePin questionId ->
            updateInQnaSession
                (addLocalChange (TogglePin questionId model.time))
                model

        GotCurrentTime currentTime ->
            ( { model
                | time = currentTime
                , lastConnectionCheck = Maybe.withDefault currentTime model.lastConnectionCheck |> Just
              }
            , Command.none
            )

        PressedDownloadQuestions ->
            updateInQnaSession
                (\inQnaSession ->
                    ( inQnaSession
                    , Effect.File.Download.string
                        "questions.csv"
                        "text/csv"
                        (Csv.Encode.encode
                            { encoder =
                                Csv.Encode.withFieldNames
                                    (\question ->
                                        [ ( "votes", String.fromInt (Question.votes question) )
                                        , ( "pinned"
                                          , if question.isPinned == Nothing then
                                                "0"

                                            else
                                                "1"
                                          )
                                        , ( "question", NonemptyString.toString question.content )
                                        ]
                                    )
                            , fieldSeparator = ';'
                            }
                            (Dict.values (Network.localState qnaSessionUpdate inQnaSession.networkModel).questions
                                |> List.sortBy (.creationTime >> Effect.Time.posixToMillis)
                            )
                        )
                    )
                )
                model

        PressedDeleteQuestion questionId ->
            updateInQnaSession
                (addLocalChange (DeleteQuestion questionId))
                model

        PressedCopyHostUrl ->
            updateInQnaSession
                (\inQnaSession ->
                    case inQnaSession.isHost of
                        Just { secret } ->
                            ( { inQnaSession | copiedHostUrl = Just model.time }
                            , copyToClipboard ("https://" ++ hostSecretToUrl secret)
                            )

                        Nothing ->
                            ( inQnaSession, Command.none )
                )
                model

        PressedCopyUrl ->
            updateInQnaSession
                (\inQnaSession ->
                    ( { inQnaSession | copiedUrl = Just model.time }
                    , copyToClipboard ("https://" ++ domain ++ urlEncoder inQnaSession.qnaSessionId)
                    )
                )
                model

        CheckIfConnected _ ->
            ( model, Effect.Lamdera.sendToBackend CheckIfConnectedRequest )

        TextInputBlurred ->
            ( model, Command.none )

        GotTimezone timezone ->
            ( { model | timezone = timezone }, Command.none )

        TypedClosingDate closingDateText ->
            updateInQnaSession
                (setClosingTimeHelper model (\hostState -> { hostState | closingDateText = closingDateText }))
                model

        TypedClosingTime closingTimeText ->
            updateInQnaSession
                (setClosingTimeHelper model (\hostState -> { hostState | closingTimeText = closingTimeText }))
                model

        PressedToggleShowSettings ->
            updateInQnaSession
                (\qnaSession ->
                    ( case qnaSession.isHost of
                        Just hostState ->
                            { qnaSession | isHost = Just { hostState | showSettings = not hostState.showSettings } }

                        Nothing ->
                            qnaSession
                    , Command.none
                    )
                )
                model


setClosingTimeHelper :
    FrontendLoaded
    -> (HostState -> HostState)
    -> InQnaSession_
    -> ( InQnaSession_, Command FrontendOnly ToBackend FrontendMsg )
setClosingTimeHelper model updateFunc qnaSession =
    case qnaSession.isHost of
        Just hostState ->
            let
                hostState2 =
                    updateFunc hostState
            in
            case validateDateTime model.time model.timezone hostState2.closingDateText hostState2.closingTimeText of
                Ok closingTime ->
                    addLocalChange (ChangeClosingTime closingTime) { qnaSession | isHost = Just hostState2 }

                Err _ ->
                    ( qnaSession, Command.none )

        Nothing ->
            ( qnaSession, Command.none )


hostSecretToUrl : CryptographicKey HostSecret -> String
hostSecretToUrl hostSecret =
    domain ++ "/" ++ hostInvite ++ "/" ++ Id.crytographicKeyToString hostSecret


domain : String
domain =
    "question-and-answer.app"


hostInvite : String
hostInvite =
    "host-invite"


addLocalChange : LocalQnaMsg -> InQnaSession_ -> ( InQnaSession_, Command FrontendOnly ToBackend FrontendMsg )
addLocalChange localMsg inQnaSession =
    ( { inQnaSession
        | networkModel =
            Network.updateFromUser
                inQnaSession.localChangeCounter
                localMsg
                inQnaSession.networkModel
        , localChangeCounter = Network.incrementChangeId inQnaSession.localChangeCounter
      }
    , Effect.Lamdera.sendToBackend (LocalMsgRequest inQnaSession.qnaSessionId inQnaSession.localChangeCounter localMsg)
    )


questionsViewId : HtmlId
questionsViewId =
    Dom.id "questions-view-id"


questionInputId : HtmlId
questionInputId =
    Dom.id "question-input-id"


scrollToOf : Int -> HtmlId -> Float -> Task.Task FrontendOnly Dom.Error ()
scrollToOf millis id y =
    Task.map2
        (\{ viewport } startTime ->
            Task.andThen
                (step (Dom.setViewportOf id) millis viewport.y y startTime)
                Effect.Time.now
        )
        (Dom.getViewportOf id)
        Effect.Time.now
        |> Task.andThen identity


step :
    (number -> Float -> Task.Task restriction x a)
    -> Int
    -> Float
    -> Float
    -> Effect.Time.Posix
    -> Effect.Time.Posix
    -> Task.Task restriction x a
step f millis start end startTime now =
    let
        elapsed : Int
        elapsed =
            Effect.Time.posixToMillis now - Effect.Time.posixToMillis startTime
    in
    f 0 (position millis start end elapsed)
        |> Task.andThen
            (if elapsed < millis then
                \_ -> Effect.Time.now |> Task.andThen (step f millis start end startTime)

             else
                Task.succeed
            )


position : Int -> Float -> Float -> Int -> Float
position millis start end elapsed =
    if elapsed < millis then
        start + (end - start) * (toFloat elapsed / toFloat millis)

    else
        end


updateInQnaSession :
    (InQnaSession_ -> ( InQnaSession_, Command FrontendOnly ToBackend FrontendMsg ))
    -> FrontendLoaded
    -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg )
updateInQnaSession updateFunc model =
    case model.remoteData of
        InQnaSession inQnaSession ->
            updateFunc inQnaSession
                |> Tuple.mapFirst (\a -> { model | remoteData = InQnaSession a })

        _ ->
            ( model, Command.none )


toggleUpvote : QuestionId -> QnaSession -> QnaSession
toggleUpvote questionId qnaSession =
    { qnaSession
        | questions =
            Dict.update
                questionId
                (Maybe.map (\question -> { question | isUpvoted = not question.isUpvoted }))
                qnaSession.questions
    }


pinQuestion : QuestionId -> Effect.Time.Posix -> QnaSession -> QnaSession
pinQuestion questionId currentTime qnaSession =
    { qnaSession
        | questions =
            Dict.update
                questionId
                (Maybe.map
                    (\question ->
                        { question
                            | isPinned =
                                case question.isPinned of
                                    Just _ ->
                                        Nothing

                                    Nothing ->
                                        Just currentTime
                        }
                    )
                )
                qnaSession.questions
    }


createQuestion : Effect.Time.Posix -> NonemptyString -> QnaSession -> QnaSession
createQuestion creationTime content qnaSession =
    let
        questionId : QuestionId
        questionId =
            Types.getQuestionId qnaSession.questions qnaSession.userId
    in
    { qnaSession
        | questions =
            Dict.insert
                questionId
                { creationTime = creationTime
                , content = content
                , isPinned = Nothing
                , otherVotes = 0
                , isUpvoted = False
                }
                qnaSession.questions
    }


deleteQuestion : QuestionId -> QnaSession -> QnaSession
deleteQuestion questionId qnaSession =
    { qnaSession | questions = Dict.remove questionId qnaSession.questions }


changeClosingTime : Effect.Time.Posix -> QnaSession -> QnaSession
changeClosingTime closingTime qnaSession =
    { qnaSession | closingTime = Just closingTime }


qnaSessionUpdate : Change LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg -> QnaSession -> QnaSession
qnaSessionUpdate msg qnaSession =
    case msg of
        LocalChange _ (ToggleUpvote questionId) ->
            toggleUpvote questionId qnaSession

        LocalChange _ (CreateQuestion creationTime content) ->
            createQuestion creationTime content qnaSession

        LocalChange _ (TogglePin questionId pinTime) ->
            pinQuestion questionId pinTime qnaSession

        LocalChange _ (DeleteQuestion questionId) ->
            deleteQuestion questionId qnaSession

        LocalChange _ (ChangeClosingTime closingTime) ->
            changeClosingTime closingTime qnaSession

        ConfirmLocalChange _ localChange ToggleUpvoteResponse ->
            case localChange of
                ToggleUpvote questionId ->
                    toggleUpvote questionId qnaSession

                _ ->
                    qnaSession

        ConfirmLocalChange _ localChange (CreateQuestionResponse creationTime) ->
            case localChange of
                CreateQuestion _ content ->
                    createQuestion creationTime content qnaSession

                _ ->
                    qnaSession

        ConfirmLocalChange _ localChange (PinQuestionResponse pinTime) ->
            case localChange of
                TogglePin questionId _ ->
                    pinQuestion questionId pinTime qnaSession

                _ ->
                    qnaSession

        ConfirmLocalChange _ localChange DeleteQuestionResponse ->
            case localChange of
                DeleteQuestion questionId ->
                    deleteQuestion questionId qnaSession

                _ ->
                    qnaSession

        ConfirmLocalChange _ localChange ChangeClosingTimeResponse ->
            case localChange of
                ChangeClosingTime closingTime ->
                    changeClosingTime closingTime qnaSession

                _ ->
                    qnaSession

        ServerChange (NewQuestion questionId creationTime content) ->
            { qnaSession
                | questions =
                    Dict.insert questionId
                        { creationTime = creationTime
                        , content = content
                        , isPinned = Nothing
                        , otherVotes = 0
                        , isUpvoted = False
                        }
                        qnaSession.questions
            }

        ServerChange (VoteAdded questionId) ->
            { qnaSession
                | questions =
                    Dict.update
                        questionId
                        (Maybe.map
                            (\question -> { question | otherVotes = question.otherVotes + 1 })
                        )
                        qnaSession.questions
            }

        ServerChange (VoteRemoved questionId) ->
            { qnaSession
                | questions =
                    Dict.update
                        questionId
                        (Maybe.map
                            (\question -> { question | otherVotes = question.otherVotes - 1 })
                        )
                        qnaSession.questions
            }

        ServerChange (QuestionPinned questionId maybePinned) ->
            { qnaSession
                | questions =
                    Dict.update
                        questionId
                        (Maybe.map
                            (\question -> { question | isPinned = maybePinned })
                        )
                        qnaSession.questions
            }

        ServerChange (QuestionDeleted questionId) ->
            deleteQuestion questionId qnaSession

        ServerChange (ClosingTimeChanged closingTime) ->
            changeClosingTime closingTime qnaSession


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case model of
        Loading _ ->
            ( model, Command.none )

        Loaded loaded ->
            updateLoadedFromBackend msg loaded |> Tuple.mapFirst Loaded


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg )
updateLoadedFromBackend msg model =
    case msg of
        ServerMsgResponse qnaSessionId serverQnaMsg ->
            updateInQnaSession
                (\inQnaSession ->
                    ( if inQnaSession.qnaSessionId == qnaSessionId then
                        { inQnaSession
                            | networkModel =
                                Network.serverChange qnaSessionUpdate serverQnaMsg inQnaSession.networkModel
                        }

                      else
                        inQnaSession
                    , Command.none
                    )
                )
                model

        LocalConfirmQnaMsgResponse qnaSessionId changeId confirmLocalMsg ->
            updateInQnaSession
                (\inQnaSession ->
                    ( if inQnaSession.qnaSessionId == qnaSessionId then
                        { inQnaSession
                            | networkModel =
                                Network.confirmLocalChange
                                    qnaSessionUpdate
                                    changeId
                                    confirmLocalMsg
                                    inQnaSession.networkModel
                        }

                      else
                        inQnaSession
                    , Command.none
                    )
                )
                model

        GetQnaSessionResponse qnaSessionId result ->
            ( case model.remoteData of
                LoadingQnaSession qnaSessionId_ ->
                    if qnaSessionId == qnaSessionId_ then
                        { model
                            | remoteData =
                                case result of
                                    Ok { isHost, qnaSession } ->
                                        InQnaSession (Types.initInQnaSession qnaSessionId qnaSession isHost)

                                    Err () ->
                                        LoadingQnaSessionFailed ()
                        }

                    else
                        model

                _ ->
                    model
            , Command.none
            )

        CreateQnaSessionResponse qnaSessionId hostSecret ->
            case model.remoteData of
                CreatingQnaSession qnaSessionName ->
                    ( { model
                        | remoteData =
                            InQnaSession
                                (Types.initInQnaSession
                                    qnaSessionId
                                    (QnaSession.init Nothing qnaSessionName)
                                    (Just hostSecret)
                                )
                      }
                    , Navigation.pushUrl model.key (urlEncoder qnaSessionId)
                    )

                _ ->
                    ( model, Command.none )

        GetQnaSessionWithHostInviteResponse hostSecret result ->
            case model.remoteData of
                LoadingQnaSessionWithHostInvite hostSecret_ ->
                    if hostSecret == hostSecret_ then
                        case result of
                            Ok ( qnaSessionId, qnaSession ) ->
                                ( { model
                                    | remoteData =
                                        Types.initInQnaSession qnaSessionId qnaSession (Just hostSecret_)
                                            |> InQnaSession
                                  }
                                , Navigation.replaceUrl model.key (urlEncoder qnaSessionId)
                                )

                            Err () ->
                                ( { model | remoteData = LoadingQnaSessionFailed () }, Command.none )

                    else
                        ( model, Command.none )

                _ ->
                    ( model, Command.none )

        CheckIfConnectedResponse ->
            ( { model | lastConnectionCheck = Just model.time }
            , Command.none
            )

        NewConnection ->
            if model.gotFirstConnectMsg then
                case model.remoteData of
                    Homepage ->
                        homepageRouteInit model.timezone model.time True model.key

                    LoadingQnaSession qnaSessionId ->
                        qnaSessionRouteInit model.timezone model.time True model.key qnaSessionId

                    LoadingQnaSessionWithHostInvite hostSecret ->
                        hostInviteRouteInit model.timezone model.time True model.key hostSecret

                    CreatingQnaSession _ ->
                        homepageRouteInit model.timezone model.time True model.key

                    LoadingQnaSessionFailed () ->
                        homepageRouteInit model.timezone model.time True model.key

                    InQnaSession inQnaSession_ ->
                        qnaSessionRouteInit model.timezone model.time True model.key inQnaSession_.qnaSessionId

            else
                ( { model | gotFirstConnectMsg = True }, Command.none )


button : List (Element.Attribute msg) -> { htmlId : HtmlId, onPress : msg, label : Element msg } -> Element msg
button attributes { htmlId, onPress, label } =
    Element.Input.button
        (Element.htmlAttribute (Dom.idToAttribute htmlId) :: attributes)
        { onPress = Just onPress
        , label = label
        }


createQnaSessionButtonId =
    Dom.id "createQnaSessionButton"


copyUrlButtonId =
    Dom.id "copyUrlButton"


view : FrontendModel -> { title : String, body : List (Html FrontendMsg) }
view model =
    { title = "Q&A"
    , body =
        case model of
            Loading _ ->
                []

            Loaded loaded ->
                [ Element.layout
                    [ Element.inFront (notConnectedView loaded) ]
                    (case loaded.remoteData of
                        Homepage ->
                            Element.column
                                [ Element.centerX, Element.centerY, Element.spacing 16, Element.paddingXY 16 0 ]
                                [ Element.paragraph
                                    [ Element.centerX ]
                                    [ Element.text "To join a Q&A session, please use the link your host has provided." ]
                                , Element.el [ Element.Font.size 24, Element.centerX ] (Element.text "OR")
                                , button
                                    (Element.centerX :: buttonStyle)
                                    { htmlId = createQnaSessionButtonId
                                    , onPress = PressedCreateQnaSession
                                    , label = Element.paragraph [] [ Element.text "Create a new Q&A session" ]
                                    }
                                ]

                        LoadingQnaSessionWithHostInvite _ ->
                            Element.el [ Element.centerX, Element.centerY ] (Element.text "Loading...")

                        LoadingQnaSession _ ->
                            Element.el [ Element.centerX, Element.centerY ] (Element.text "Loading...")

                        CreatingQnaSession _ ->
                            Element.el [ Element.centerX, Element.centerY ] (Element.text "Creating...")

                        LoadingQnaSessionFailed () ->
                            Element.paragraph
                                [ Element.Font.center, Element.centerY, Element.padding 8 ]
                                [ Element.text "Sorry, this Q&A session doesn't exist." ]

                        InQnaSession inQnaSession ->
                            let
                                qnaSession : QnaSession
                                qnaSession =
                                    Network.localState qnaSessionUpdate inQnaSession.networkModel
                            in
                            Element.column
                                [ Element.spacing 16
                                , Element.width <| Element.maximum 800 Element.fill
                                , Element.centerX
                                , Element.paddingXY 16 16
                                , Element.height Element.fill
                                ]
                                [ Element.column
                                    [ Element.width Element.fill, Element.height Element.fill, Element.spacing 6 ]
                                    [ Element.text "Questions"
                                    , questionsView
                                        inQnaSession.qnaSessionId
                                        inQnaSession.copiedUrl
                                        loaded.time
                                        (inQnaSession.isHost /= Nothing)
                                        qnaSession.userId
                                        qnaSession.questions
                                    ]
                                , case inQnaSession.isHost of
                                    Just hostState ->
                                        Element.column
                                            [ Element.width Element.fill, Element.spacing 8 ]
                                            [ button (buttonStyle ++ [ Element.padding 8 ])
                                                { htmlId = toggleShowSettingsId
                                                , onPress = PressedToggleShowSettings
                                                , label = Element.text "⚙️"
                                                }
                                            , if hostState.showSettings then
                                                hostView loaded inQnaSession hostState qnaSession

                                              else
                                                Element.none
                                            ]

                                    Nothing ->
                                        questionInputView inQnaSession
                                ]
                    )
                ]
    }


toggleShowSettingsId : HtmlId
toggleShowSettingsId =
    Dom.id "toggleShowSettingsButton"


notConnectedView : FrontendLoaded -> Element msg
notConnectedView model =
    case model.lastConnectionCheck of
        Just lastCheck ->
            if
                Duration.from lastCheck model.time
                    |> Quantity.lessThan (Duration.seconds 30)
            then
                Element.none

            else
                Element.paragraph
                    [ Element.width Element.fill
                    , Element.Background.color <| Element.rgb 1 0.6 0.6
                    , Element.padding 16
                    , Element.Font.center
                    ]
                    [ Element.text "I can't reach the server! Try refreshing the page?" ]

        Nothing ->
            Element.none


hostView : FrontendLoaded -> InQnaSession_ -> HostState -> QnaSession -> Element FrontendMsg
hostView model inQnaSession hostState qnaSession =
    Element.column
        [ Element.width Element.fill, Element.spacing 12 ]
        [ copyHostUrlButton inQnaSession.copiedHostUrl
        , Element.row
            []
            [ dateTimeInput
                { dateInputId = dateInputId
                , timeInputId = timeInputId
                , dateChanged = TypedClosingDate
                , timeChanged = TypedClosingTime
                , labelText = "How long do people have to ask questions?"
                , minTime = model.time
                , timezone = model.timezone
                , dateText = hostState.closingDateText
                , timeText = hostState.closingTimeText
                , isDisabled = False
                , maybeError = Nothing
                }
            , case qnaSession.closingTime of
                Just closingTime ->
                    "Questions close in "
                        ++ diffToString model.time closingTime
                        |> Element.text
                        |> Element.el [ Element.centerY ]

                Nothing ->
                    Element.none
            ]
        , animatedParagraph
            (Animation.fromTo
                { duration = 2000, options = [] }
                [ Property.opacity 0 ]
                [ Property.opacity <|
                    if Dict.isEmpty qnaSession.questions then
                        0

                    else
                        1
                ]
            )
            [ smallFont
            , (if Dict.isEmpty qnaSession.questions then
                "none"

               else
                "auto"
              )
                |> Html.Attributes.style "pointer-events"
                |> Element.htmlAttribute
            ]
            [ Element.text "Questions will be deleted after 14 days of inactivity. "
            , button
                [ Element.Font.color <| Element.rgb 0.2 0.2 1
                , Element.Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
                , Element.Border.color <| Element.rgba 0 0 0 0
                , Element.mouseOver [ Element.Border.color <| Element.rgb 0.2 0.2 1 ]
                ]
                { htmlId = downloadQuestionsButtonId
                , onPress = PressedDownloadQuestions
                , label = Element.text "Click here"
                }
            , Element.text " to download them."
            ]
        ]


downloadQuestionsButtonId =
    Dom.id "downloadQuestionsButton"


smallFont : Element.Attr decorative msg
smallFont =
    Element.Font.size 16


copyHostUrlButtonId : HtmlId
copyHostUrlButtonId =
    Dom.id "copyHostUrlButton"


dateInputId : HtmlId
dateInputId =
    Dom.id "dateInput"


timeInputId : HtmlId
timeInputId =
    Dom.id "timeInput"


copyHostUrlButton : Maybe Effect.Time.Posix -> Element FrontendMsg
copyHostUrlButton copiedHostUrl =
    Element.row
        [ Element.width Element.fill, Element.spacing 12, smallFont ]
        [ button
            (buttonStyle ++ [ Element.padding 12 ])
            { htmlId = copyHostUrlButtonId
            , onPress = PressedCopyHostUrl
            , label = Element.text "Add another host"
            }
        , case copiedHostUrl of
            Just copiedTime ->
                Element.Keyed.el
                    [ Element.width Element.fill ]
                    ( Effect.Time.posixToMillis copiedTime |> String.fromInt
                    , animatedParagraph
                        (Animation.steps
                            { options = [], startAt = [ Property.opacity 0 ] }
                            [ Animation.step 100 [ Property.opacity 1 ]
                            , Animation.step 20000 [ Property.opacity 1 ]
                            , Animation.step 3000 [ Property.opacity 0 ]
                            ]
                        )
                        []
                        [ Element.text "Host invite link copied. Paste it to someone so they can join as a host." ]
                    )

            Nothing ->
                Element.none
        ]


maxQuestionChars : number
maxQuestionChars =
    200


questionInputView : InQnaSession_ -> Element FrontendMsg
questionInputView inQnaSession =
    Element.column
        [ Element.width Element.fill, Element.spacing 16 ]
        [ Element.el
            [ Element.inFront <|
                Element.el
                    [ Element.alignBottom
                    , Element.alignRight
                    , Element.Font.color <|
                        if String.length inQnaSession.question > maxQuestionChars then
                            errorColor

                        else
                            Element.rgb 0.2 0.2 0.2
                    , Element.Font.size 18
                    , Element.moveLeft 24
                    , Element.moveUp 4
                    ]
                    (Element.text
                        (String.fromInt (String.length inQnaSession.question)
                            ++ "/"
                            ++ String.fromInt maxQuestionChars
                        )
                    )
            , Element.width Element.fill
            ]
            (Element.Input.multiline
                [ Element.height <| Element.px 120
                , Element.htmlAttribute <|
                    Html.Events.preventDefaultOn "keydown"
                        (Json.Decode.map3 (\key shift ctrl -> ( key, shift, ctrl ))
                            (Json.Decode.field "key" Json.Decode.string)
                            (Json.Decode.field "shiftKey" Json.Decode.bool)
                            (Json.Decode.field "ctrlKey" Json.Decode.bool)
                            |> Json.Decode.andThen
                                (\( key, shift, ctrl ) ->
                                    if key == "Enter" && not shift && not ctrl then
                                        Json.Decode.succeed ( PressedCreateQuestion, True )

                                    else
                                        Json.Decode.fail ""
                                )
                        )
                , Element.htmlAttribute <| Dom.idToAttribute questionInputId
                ]
                { onChange = TypedQuestion
                , placeholder = Nothing
                , spellcheck = True
                , label =
                    Element.Input.labelAbove
                        []
                        (Element.text "What do you want to ask?")
                , text = inQnaSession.question
                }
            )
        , Element.row [ Element.spacing 16 ]
            [ button
                buttonStyle
                { htmlId = createQuestionButtonId
                , onPress = PressedCreateQuestion
                , label =
                    Element.text "Submit question"
                }
            , case ( valiatedQuestion inQnaSession.question, inQnaSession.pressedCreateQuestion ) of
                ( Err error, True ) ->
                    Element.paragraph
                        [ Element.Font.color errorColor ]
                        [ Element.text error ]

                _ ->
                    Element.none
            ]
        ]


createQuestionButtonId =
    Dom.id "createQuestionButton"


valiatedQuestion : String -> Result String NonemptyString
valiatedQuestion text =
    if String.length text > maxQuestionChars then
        Err "Your question is too long"

    else
        case NonemptyString.fromString (String.trim text) of
            Just nonempty ->
                Ok nonempty

            Nothing ->
                Err "Write something first!"


errorColor : Element.Color
errorColor =
    Element.rgb 0.8 0.2 0.2


questionsView :
    CryptographicKey QnaSessionId
    -> Maybe Effect.Time.Posix
    -> Effect.Time.Posix
    -> Bool
    -> UserId
    -> Dict QuestionId Question
    -> Element FrontendMsg
questionsView qnaSessionId maybeCopiedUrl currentTime isHost userId questions =
    let
        containerStyle =
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.Border.rounded 4
            , Element.scrollbars
            , Element.htmlAttribute <| Dom.idToAttribute questionsViewId
            , Element.Border.width 1
            , Element.Border.color <| Element.rgb 0.5 0.5 0.5
            ]

        pinnedQuestions : List ( String, Element FrontendMsg )
        pinnedQuestions =
            Dict.toList questions
                |> List.filterMap
                    (\( questionId, question ) ->
                        question.isPinned |> Maybe.map (\pinTime -> ( questionId, pinTime, question ))
                    )
                |> List.sortBy (\( _, pinTime, _ ) -> Effect.Time.posixToMillis pinTime)
                |> List.map
                    (\( questionId, _, question ) ->
                        keyedQuestion False True ( questionId, question )
                    )

        unpinnedQuestions : List ( String, Element FrontendMsg )
        unpinnedQuestions =
            Dict.toList questions
                |> List.filter (Tuple.second >> .isPinned >> (==) Nothing)
                |> List.sortWith
                    (\( _, a ) ( _, b ) ->
                        case compare (Question.votes a) (Question.votes b) of
                            GT ->
                                LT

                            LT ->
                                GT

                            EQ ->
                                compare (Effect.Time.posixToMillis a.creationTime) (Effect.Time.posixToMillis b.creationTime)
                    )
                |> List.indexedMap
                    (\index value ->
                        keyedQuestion (index == 0 && not (List.isEmpty pinnedQuestions)) False value
                    )

        keyedQuestion : Bool -> Bool -> ( QuestionId, Question ) -> ( String, Element FrontendMsg )
        keyedQuestion isFirstUnpinnedQuestion isPinned ( questionId, question ) =
            ( Question.questionIdToString questionId
            , questionView isFirstUnpinnedQuestion isPinned currentTime isHost userId questionId question
            )
    in
    if Dict.isEmpty questions then
        Element.el containerStyle
            (Element.column
                [ Element.width Element.fill
                , Element.centerY
                , Element.spacing 16
                , Element.padding 8
                ]
                (emptyContainer qnaSessionId isHost maybeCopiedUrl)
            )

    else
        pinnedQuestions
            ++ unpinnedQuestions
            |> Element.Keyed.column containerStyle


emptyContainer : CryptographicKey QnaSessionId -> Bool -> Maybe Effect.Time.Posix -> List (Element FrontendMsg)
emptyContainer qnaSessionId isHost maybeCopiedUrl =
    if isHost then
        [ Element.el [ Element.centerX, Element.Font.size 36 ] (Element.text "You are the host!")
        , Element.column
            [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.paragraph
                [ Element.Font.center, Element.Font.size 20 ]
                [ Element.text "Copy the link below so people can ask you questions:" ]
            , button
                [ Element.centerX
                , Element.Font.size 20
                , Element.onRight <|
                    case maybeCopiedUrl of
                        Just copiedUrl ->
                            Element.Keyed.el
                                []
                                ( Effect.Time.posixToMillis copiedUrl |> String.fromInt
                                , animatedEl
                                    (Animation.steps
                                        { options = [], startAt = [ Property.opacity 0 ] }
                                        [ Animation.step 100 [ Property.opacity 1 ]
                                        , Animation.step 1000 [ Property.opacity 1 ]
                                        , Animation.step 3000 [ Property.opacity 0 ]
                                        ]
                                    )
                                    [ Element.paddingEach { left = 4, right = 0, top = 0, bottom = 0 } ]
                                    (Element.text "Copied!")
                                )

                        Nothing ->
                            Element.none
                ]
                { htmlId = copyUrlButtonId
                , onPress = PressedCopyUrl
                , label =
                    Element.row
                        [ Element.spacing 2 ]
                        [ Element.text (domain ++ urlEncoder qnaSessionId), Element.text "📋" ]
                }
            ]
        ]

    else
        [ Element.paragraph
            [ Element.Font.color <| Element.rgb 0.6 0.6 0.6, Element.Font.size 30, Element.Font.center ]
            [ Element.text "No questions yet. You\u{00A0}can be the first to write one!" ]
        ]


animatedUi : (List (Element.Attribute msg) -> children -> Element msg) -> Animation -> List (Element.Attribute msg) -> children -> Element msg
animatedUi =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }


animatedParagraph : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
animatedParagraph =
    animatedUi Element.paragraph


animatedRow : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
animatedRow =
    animatedUi Element.row


animatedEl : Animation -> List (Element.Attribute msg) -> Element msg -> Element msg
animatedEl =
    animatedUi Element.el


questionView : Bool -> Bool -> Effect.Time.Posix -> Bool -> UserId -> QuestionId -> Question -> Element FrontendMsg
questionView isFirstUnpinnedQuestion isPinned time isHost userId questionId question =
    animatedRow
        (Animation.fromTo
            { duration = 1500, options = [] }
            [ Property.backgroundColor
                (if question.isPinned == Nothing then
                    if Question.isNewQuestion time question then
                        "lightgreen"

                    else
                        "white"

                 else
                    "lightyellow"
                )
            ]
            [ Property.backgroundColor
                (if question.isPinned == Nothing then
                    "white"

                 else
                    "lightyellow"
                )
            ]
        )
        [ Element.paddingEach { left = 8, right = 12, top = 8, bottom = 8 }
        , Element.spacing 8
        , Element.width Element.fill
        , Element.inFront
            (if isFirstUnpinnedQuestion then
                let
                    lineColor =
                        Element.rgb 0.5 0.5 0.5
                in
                Element.row
                    [ Element.width Element.fill
                    , Element.paddingXY 8 0
                    , Element.alignTop
                    , Element.Font.size 14
                    , Element.spacing 8
                    , Element.moveUp 7
                    ]
                    [ Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 1)
                        , Element.Background.color lineColor
                        , Element.centerY
                        ]
                        Element.none
                    , Element.text "Pinned 📌"
                    , Element.el
                        [ Element.width (Element.px 24)
                        , Element.height (Element.px 1)
                        , Element.Background.color lineColor
                        , Element.centerY
                        ]
                        Element.none
                    ]

             else
                Element.none
            )
        ]
        [ upvoteButton questionId question
        , Element.paragraph
            [ Element.htmlAttribute <| Html.Attributes.style "word-break" "break-word" ]
            [ Element.text (NonemptyString.toString question.content) ]
        , if isHost then
            button
                (buttonStyle ++ [ Element.padding 8, smallFont ])
                { htmlId = togglePinButtonId
                , onPress = PressedTogglePin questionId
                , label =
                    case question.isPinned of
                        Just _ ->
                            Element.text "Unpin"

                        Nothing ->
                            Element.text "Answer"
                }

          else if Question.isCreator userId questionId && not isPinned then
            button
                (buttonStyle ++ [ smallFont, Element.paddingXY 4 6 ])
                { htmlId = deleteQuestionButtonId
                , onPress = PressedDeleteQuestion questionId
                , label = Element.text "🗑️"
                }

          else
            Element.none
        ]


togglePinButtonId =
    Dom.id "togglePinButton"


deleteQuestionButtonId =
    Dom.id "deleteQuestionButton"


upvoteButton : QuestionId -> Question -> Element FrontendMsg
upvoteButton questionId question =
    button
        [ Element.Border.rounded 999
        , Element.padding 8
        , Element.Background.color <|
            if question.isUpvoted then
                Element.rgb 0.92 0.78 0.68

            else
                Element.rgb 0.9 0.9 0.9
        , Element.width <| Element.px 48
        , Element.height <| Element.px 48
        , Element.Border.width 2
        , Element.Border.color <|
            if question.isUpvoted then
                Element.rgb 0.8 0.4 0.35

            else
                Element.rgb 0.4 0.4 0.4
        ]
        { htmlId = toggleUpvoteButtonId questionId
        , onPress = PressedToggleUpvote questionId
        , label =
            Element.row
                [ Element.centerX, Element.centerY, smallFont, Element.spacing 2 ]
                [ Element.text (String.fromInt (Question.votes question))
                , Element.el [ Element.Font.size 14 ] (Element.text "❤️")
                ]
        }


toggleUpvoteButtonId : QuestionId -> HtmlId
toggleUpvoteButtonId (QuestionId (UserId userId) index) =
    "toggleUpvoteButton_" ++ String.fromInt userId ++ "_" ++ String.fromInt index |> Dom.id


buttonStyle : List (Element.Attr () msg)
buttonStyle =
    [ Element.Background.color <| Element.rgb 0.8 0.8 0.8
    , Element.padding 16
    , Element.Border.rounded 4
    ]


dateTimeInput :
    { dateInputId : HtmlId
    , timeInputId : HtmlId
    , dateChanged : String -> msg
    , timeChanged : String -> msg
    , labelText : String
    , minTime : Effect.Time.Posix
    , timezone : Effect.Time.Zone
    , dateText : String
    , timeText : String
    , isDisabled : Bool
    , maybeError : Maybe String
    }
    -> Element msg
dateTimeInput config =
    Element.column
        [ Element.spacing 4 ]
        [ formLabelAboveEl config.labelText
        , Element.row [ Element.spacing 8, Element.width Element.fill ]
            [ dateInput dateInputId config.dateChanged (Date.fromPosix config.timezone config.minTime) config.dateText config.isDisabled
            , timeInput config.timeInputId config.timeChanged config.timeText config.isDisabled
            ]
        , config.maybeError |> Maybe.map errorView |> Maybe.withDefault Element.none
        ]


formLabelAboveEl : String -> Element msg
formLabelAboveEl labelText =
    Element.el
        [ Element.paddingEach { top = 0, right = 0, bottom = 5, left = 0 }
        , Element.Font.medium
        , Element.Font.size 13
        ]
        (Element.paragraph [] [ Element.text labelText ])


errorView : String -> Element msg
errorView errorMessage =
    Element.paragraph
        [ Element.paddingEach { left = 4, right = 4, top = 4, bottom = 0 }
        , Element.Font.color errorColor
        , Element.Font.size 14
        , Element.Font.medium
        ]
        [ Element.text errorMessage ]


timeInput : HtmlId -> (String -> msg) -> String -> Bool -> Element msg
timeInput htmlId onChange time isDisabled =
    Html.input
        ([ Html.Attributes.type_ "time"
         , Html.Events.onInput onChange
         , Html.Attributes.value time
         , Html.Attributes.style "padding" "5px"
         , Dom.idToAttribute htmlId
         , Html.Attributes.disabled isDisabled
         ]
            ++ htmlInputBorderStyles
        )
        []
        |> Element.html
        |> Element.el []


htmlInputBorderStyles =
    [ Html.Attributes.style "border-color" "gray"
    , Html.Attributes.style "border-width" "1px"
    , Html.Attributes.style "border-style" "solid"
    , Html.Attributes.style "border-radius" "4px"
    ]


timeToString : Effect.Time.Zone -> Effect.Time.Posix -> String
timeToString timezone time =
    String.fromInt (Effect.Time.toHour timezone time)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Effect.Time.toMinute timezone time))


dateInput : HtmlId -> (String -> msg) -> Date -> String -> Bool -> Element msg
dateInput htmlId onChange minDateTime date isDisabled =
    Html.input
        ([ Html.Attributes.type_ "date"
         , Html.Attributes.min (datestamp minDateTime)
         , Html.Events.onInput onChange
         , Html.Attributes.value date
         , Html.Attributes.style "padding" "5px"
         , Dom.idToAttribute htmlId
         , Html.Attributes.disabled isDisabled
         ]
            ++ htmlInputBorderStyles
        )
        []
        |> Element.html
        |> Element.el []


datetimeToString : Effect.Time.Zone -> Effect.Time.Posix -> String
datetimeToString timezone time =
    let
        offset =
            toFloat (Time.toOffset timezone time) / 60
    in
    (time |> Date.fromPosix timezone |> Date.format "MMMM ddd")
        ++ ", "
        ++ timeToString timezone time
        ++ (if offset >= 0 then
                removeTrailing0s 1 offset |> (++) " GMT+"

            else
                removeTrailing0s 1 offset |> (++) " GMT"
           )


datestamp : Date -> String
datestamp date =
    String.fromInt (Date.year date)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Date.monthNumber date))
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Date.day date))


{-| Timestamp used by time input field.
-}
timestamp : Int -> Int -> String
timestamp hour minute =
    String.padLeft 2 '0' (String.fromInt hour) ++ ":" ++ String.padLeft 2 '0' (String.fromInt minute)


removeTrailing0s : Int -> Float -> String
removeTrailing0s decimalPoints value =
    case Round.round decimalPoints value |> String.split "." of
        [ nonDecimal, decimal ] ->
            if decimalPoints > 0 then
                nonDecimal
                    ++ "."
                    ++ (String.foldr
                            (\char ( text, reachedNonZero ) ->
                                if reachedNonZero || char /= '0' then
                                    ( text, True )

                                else
                                    ( String.dropRight 1 text, False )
                            )
                            ( decimal, False )
                            decimal
                            |> Tuple.first
                       )
                    |> dropSuffix "."

            else
                nonDecimal

        [ nonDecimal ] ->
            nonDecimal

        _ ->
            "0"


dropSuffix : String -> String -> String
dropSuffix suffix string =
    if String.endsWith suffix string then
        String.dropRight (String.length suffix) string

    else
        string


validateDateTime : Effect.Time.Posix -> Effect.Time.Zone -> String -> String -> Result String Effect.Time.Posix
validateDateTime currentTime timezone date time =
    if String.trim date == "" then
        Err "Date value missing"

    else
        case String.split "-" date |> List.map String.toInt of
            [ Just year, Just monthInt, Just day ] ->
                case intToMonth monthInt of
                    Just month ->
                        if String.trim time == "" then
                            Err "Time value missing"

                        else
                            case String.split ":" time |> List.map String.toInt of
                                [ Just hour, Just minute ] ->
                                    let
                                        timePosix =
                                            Time.partsToPosix
                                                timezone
                                                { year = year
                                                , month = month
                                                , day = day
                                                , hour = hour
                                                , minute = minute
                                                , second = 0
                                                , millisecond = 0
                                                }
                                    in
                                    if Duration.from currentTime timePosix |> Quantity.lessThanZero then
                                        Err "The event can't start in the past"

                                    else
                                        Ok timePosix

                                _ ->
                                    Err "Invalid time format. Expected something like 22:59"

                    Nothing ->
                        Err "Invalid date format. Expected something like 2020-01-31"

            _ ->
                Err "Invalid date format. Expected something like 2020-01-31"


intToMonth : Int -> Maybe Time.Month
intToMonth value =
    case value of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


diffToString : Time.Posix -> Time.Posix -> String
diffToString start end =
    let
        difference : Duration
        difference =
            Duration.from start end |> Quantity.abs

        months =
            Duration.inDays difference / 30 |> floor

        weeks =
            Duration.inWeeks difference |> floor

        days =
            Duration.inDays difference

        hours =
            Duration.inHours difference |> floor

        minutes =
            Duration.inMinutes difference |> round

        suffix =
            if Time.posixToMillis start <= Time.posixToMillis end then
                ""

            else
                " ago"
    in
    if months >= 2 then
        String.fromInt months ++ "\u{00A0}months" ++ suffix

    else if weeks >= 2 then
        String.fromInt weeks ++ "\u{00A0}weeks" ++ suffix

    else if days > 1 then
        removeTrailing0s 1 days ++ "\u{00A0}days" ++ suffix

    else if days > 0.9 && days < 1.1 then
        if Time.posixToMillis start <= Time.posixToMillis end then
            "1\u{00A0}day"

        else
            "yesterday"

    else if hours > 6 then
        String.fromInt hours ++ "\u{00A0}hours" ++ suffix

    else if Duration.inHours difference >= 1.2 then
        removeTrailing0s 1 (Duration.inHours difference) ++ "\u{00A0}hours" ++ suffix

    else if minutes > 1 then
        String.fromInt minutes ++ "\u{00A0}minutes" ++ suffix

    else if minutes == 1 then
        "1\u{00A0}minute" ++ suffix

    else
        "now"
