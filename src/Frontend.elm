port module Frontend exposing (app, init, update, updateFromBackend, view)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation
import Csv.Encode
import Duration
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Keyed
import File.Download
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Id exposing (CryptographicKey(..), HostSecret, QnaSessionId, UserId(..))
import Json.Decode
import Lamdera
import Network exposing (Change(..))
import QnaSession exposing (QnaSession)
import Quantity
import Question exposing (Question, QuestionId)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as Property
import String.Nonempty as NonemptyString exposing (NonemptyString(..))
import Task exposing (Task)
import Time
import Types
    exposing
        ( ConfirmLocalQnaMsg(..)
        , FrontendModel
        , FrontendMsg(..)
        , FrontendStatus(..)
        , InQnaSession_
        , LocalQnaMsg(..)
        , ServerQnaMsg(..)
        , ToBackend(..)
        , ToFrontend(..)
        )
import Url exposing (Url)
import Url.Parser exposing ((</>))


port supermario_copy_to_clipboard_to_js : String -> Cmd msg


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Time.every 1000 GotCurrentTime
                    , Time.every 10000 CheckIfConnected
                    ]
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    case Url.Parser.parse urlDecoder url of
        Just (QnaSessionRoute qnaSessionId) ->
            ( { key = key
              , remoteData = LoadingQnaSession qnaSessionId
              , currentTime = Nothing
              , lastConnectionCheck = Nothing
              }
            , Lamdera.sendToBackend (GetQnaSession qnaSessionId)
            )

        Just (HostInviteRoute hostSecret) ->
            ( { key = key
              , remoteData = LoadingQnaSessionWithHostInvite hostSecret
              , currentTime = Nothing
              , lastConnectionCheck = Nothing
              }
            , Lamdera.sendToBackend (GetQnaSessionWithHostInvite hostSecret)
            )

        Just HomepageRoute ->
            ( { key = key, remoteData = Homepage, currentTime = Nothing, lastConnectionCheck = Nothing }
            , Cmd.none
            )

        Nothing ->
            ( { key = key, remoteData = Homepage, currentTime = Nothing, lastConnectionCheck = Nothing }
            , Cmd.none
            )


type Route
    = HomepageRoute
    | HostInviteRoute (CryptographicKey HostSecret)
    | QnaSessionRoute (CryptographicKey QnaSessionId)


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


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Browser.Navigation.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged url ->
            case Url.Parser.parse urlDecoder url of
                Just HomepageRoute ->
                    ( model, Browser.Navigation.load "/" )

                Just (QnaSessionRoute qnaSessionId) ->
                    case model.remoteData of
                        InQnaSession inQnaSession ->
                            if inQnaSession.qnaSessionId == qnaSessionId then
                                ( model, Cmd.none )

                            else
                                ( model, Browser.Navigation.load (urlEncoder qnaSessionId) )

                        _ ->
                            ( model, Browser.Navigation.load (urlEncoder qnaSessionId) )

                _ ->
                    ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        PressedCreateQnaSession ->
            let
                name =
                    NonemptyString 'T' "est"
            in
            ( { model | remoteData = CreatingQnaSession name }, Lamdera.sendToBackend (CreateQnaSession name) )

        TypedQuestion text ->
            case model.remoteData of
                InQnaSession inQnaSession ->
                    ( { model | remoteData = InQnaSession { inQnaSession | question = text } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PressedCreateQuestion ->
            updateInQnaSession
                (\inQnaSession ->
                    case valiatedQuestion inQnaSession.question of
                        Ok nonempty ->
                            let
                                localMsg =
                                    CreateQuestion (Maybe.withDefault (Time.millisToPosix 0) model.currentTime) nonempty
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
                            , Cmd.batch
                                [ Lamdera.sendToBackend
                                    (LocalMsgRequest inQnaSession.qnaSessionId inQnaSession.localChangeCounter localMsg)
                                , Browser.Dom.getViewportOf questionsViewId
                                    |> Task.andThen
                                        (\{ scene, viewport } ->
                                            scrollToOf 200 questionsViewId (scene.height - viewport.height)
                                        )
                                    |> Task.attempt (always NoOpFrontendMsg)
                                , Browser.Dom.blur questionInputId |> Task.attempt (always NoOpFrontendMsg)
                                ]
                            )

                        Err _ ->
                            ( { inQnaSession | pressedCreateQuestion = True }
                            , Cmd.none
                            )
                )
                model

        PressedToggleUpvote questionId ->
            updateInQnaSession (addLocalChange (ToggleUpvote questionId)) model

        PressedTogglePin questionId ->
            updateInQnaSession
                (let
                    localMsg =
                        TogglePin
                            questionId
                            (Maybe.withDefault (Time.millisToPosix 0) model.currentTime)
                 in
                 addLocalChange localMsg
                )
                model

        GotCurrentTime currentTime ->
            ( { model
                | currentTime = Just currentTime
                , lastConnectionCheck = Maybe.withDefault currentTime model.lastConnectionCheck |> Just
              }
            , Cmd.none
            )

        PressedDownloadQuestions ->
            updateInQnaSession
                (\inQnaSession ->
                    ( inQnaSession
                    , File.Download.string
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
                                |> List.sortBy (.creationTime >> Time.posixToMillis)
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
                        Just hostSecret ->
                            ( { inQnaSession | copiedHostUrl = model.currentTime }
                            , supermario_copy_to_clipboard_to_js (hostSecretToUrl hostSecret)
                            )

                        Nothing ->
                            ( inQnaSession, Cmd.none )
                )
                model

        PressedCopyUrl ->
            updateInQnaSession
                (\inQnaSession ->
                    ( { inQnaSession | copiedUrl = model.currentTime }
                    , supermario_copy_to_clipboard_to_js (domain ++ urlEncoder inQnaSession.qnaSessionId)
                    )
                )
                model

        CheckIfConnected _ ->
            ( model, Lamdera.sendToBackend CheckIfConnectedRequest )


hostSecretToUrl : CryptographicKey HostSecret -> String
hostSecretToUrl hostSecret =
    domain ++ "/" ++ hostInvite ++ "/" ++ Id.crytographicKeyToString hostSecret


domain : String
domain =
    "question-and-answer.app"


hostInvite : String
hostInvite =
    "host-invite"


addLocalChange : LocalQnaMsg -> InQnaSession_ -> ( InQnaSession_, Cmd FrontendMsg )
addLocalChange localMsg inQnaSession =
    ( { inQnaSession
        | networkModel =
            Network.updateFromUser
                inQnaSession.localChangeCounter
                localMsg
                inQnaSession.networkModel
        , localChangeCounter = Network.incrementChangeId inQnaSession.localChangeCounter
      }
    , Lamdera.sendToBackend (LocalMsgRequest inQnaSession.qnaSessionId inQnaSession.localChangeCounter localMsg)
    )


questionsViewId : String
questionsViewId =
    "questions-view-id"


questionInputId : String
questionInputId =
    "question-input-id"


scrollToOf : Int -> String -> Float -> Task Browser.Dom.Error ()
scrollToOf millis id y =
    Task.map2
        (\{ viewport } startTime ->
            Task.andThen
                (step (Browser.Dom.setViewportOf id) millis viewport.y y startTime)
                Time.now
        )
        (Browser.Dom.getViewportOf id)
        Time.now
        |> Task.andThen identity


step : (number -> Float -> Task x a) -> Int -> Float -> Float -> Time.Posix -> Time.Posix -> Task x a
step f millis start end startTime now =
    let
        elapsed : Int
        elapsed =
            Time.posixToMillis now - Time.posixToMillis startTime
    in
    f 0 (position millis start end elapsed)
        |> Task.andThen
            (if elapsed < millis then
                \_ -> Time.now |> Task.andThen (step f millis start end startTime)

             else
                Task.succeed
            )


position : Int -> Float -> Float -> Int -> Float
position millis start end elapsed =
    if elapsed < millis then
        start + (end - start) * (toFloat elapsed / toFloat millis)

    else
        end


updateInQnaSession : (InQnaSession_ -> ( InQnaSession_, Cmd FrontendMsg )) -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateInQnaSession updateFunc model =
    case model.remoteData of
        InQnaSession inQnaSession ->
            updateFunc inQnaSession
                |> Tuple.mapFirst (\a -> { model | remoteData = InQnaSession a })

        _ ->
            ( model, Cmd.none )


toggleUpvote : QuestionId -> QnaSession -> QnaSession
toggleUpvote questionId qnaSession =
    { qnaSession
        | questions =
            Dict.update
                questionId
                (Maybe.map (\question -> { question | isUpvoted = not question.isUpvoted }))
                qnaSession.questions
    }


pinQuestion : QuestionId -> Time.Posix -> QnaSession -> QnaSession
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


createQuestion : Time.Posix -> NonemptyString -> QnaSession -> QnaSession
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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
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
                    , Cmd.none
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
                    , Cmd.none
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
            , Cmd.none
            )

        CreateQnaSessionResponse qnaSessionId hostSecret ->
            case model.remoteData of
                CreatingQnaSession qnaSessionName ->
                    ( { model
                        | remoteData =
                            InQnaSession
                                (Types.initInQnaSession
                                    qnaSessionId
                                    (QnaSession.init qnaSessionName)
                                    (Just hostSecret)
                                )
                      }
                    , Browser.Navigation.pushUrl model.key (urlEncoder qnaSessionId)
                    )

                _ ->
                    ( model, Cmd.none )

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
                                , Browser.Navigation.replaceUrl model.key (urlEncoder qnaSessionId)
                                )

                            Err () ->
                                ( { model | remoteData = LoadingQnaSessionFailed () }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CheckIfConnectedResponse ->
            ( { model | lastConnectionCheck = model.currentTime }
            , Cmd.none
            )


view : FrontendModel -> { title : String, body : List (Html FrontendMsg) }
view model =
    { title = "Q&A"
    , body =
        [ Element.layout
            []
            (case model.remoteData of
                Homepage ->
                    Element.column
                        [ Element.centerX, Element.centerY, Element.spacing 16, Element.paddingXY 16 0 ]
                        [ Element.paragraph
                            [ Element.centerX ]
                            [ Element.text "To join a Q&A session, please use the link your host has provided." ]
                        , Element.el [ Element.Font.size 24, Element.centerX ] (Element.text "OR")
                        , Element.Input.button
                            (Element.centerX :: buttonStyle)
                            { onPress = Just PressedCreateQnaSession
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
                        , Element.inFront (notConnectedView model)
                        ]
                        [ Element.column
                            [ Element.width Element.fill, Element.height Element.fill, Element.spacing 6 ]
                            [ Element.text "Questions"
                            , questionsView
                                inQnaSession.qnaSessionId
                                inQnaSession.copiedUrl
                                model.currentTime
                                (inQnaSession.isHost /= Nothing)
                                qnaSession.userId
                                qnaSession.questions
                            ]
                        , case inQnaSession.isHost of
                            Just _ ->
                                hostView inQnaSession.copiedHostUrl qnaSession

                            Nothing ->
                                questionInputView inQnaSession
                        ]
            )
        ]
    }


notConnectedView : FrontendModel -> Element msg
notConnectedView model =
    case ( model.lastConnectionCheck, model.currentTime ) of
        ( Just lastCheck, Just currentTime ) ->
            if
                Duration.from lastCheck currentTime
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

        _ ->
            Element.none


hostView : Maybe Time.Posix -> QnaSession -> Element FrontendMsg
hostView copiedHostUrl qnaSession =
    Element.column
        [ Element.width Element.fill, Element.spacing 16 ]
        [ copyHostUrlButton copiedHostUrl
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
            [ Element.Font.size 16
            , (if Dict.isEmpty qnaSession.questions then
                "none"

               else
                "auto"
              )
                |> Html.Attributes.style "pointer-events"
                |> Element.htmlAttribute
            ]
            [ Element.text "Questions will be deleted after 14 days of inactivity. "
            , Element.Input.button
                [ Element.Font.color <| Element.rgb 0.2 0.2 1
                , Element.Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
                , Element.Border.color <| Element.rgba 0 0 0 0
                , Element.mouseOver [ Element.Border.color <| Element.rgb 0.2 0.2 1 ]
                ]
                { onPress = Just PressedDownloadQuestions, label = Element.text "Click here" }
            , Element.text " to download them."
            ]
        ]


copyHostUrlButton : Maybe Time.Posix -> Element FrontendMsg
copyHostUrlButton copiedHostUrl =
    Element.row
        [ Element.width Element.fill, Element.spacing 16 ]
        [ Element.Input.button
            buttonStyle
            { onPress = Just PressedCopyHostUrl
            , label = Element.text "Add another host"
            }
        , case copiedHostUrl of
            Just copiedTime ->
                Element.Keyed.el
                    [ Element.width Element.fill ]
                    ( Time.posixToMillis copiedTime |> String.fromInt
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
                , Element.htmlAttribute <| Html.Attributes.id questionInputId
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
            [ Element.Input.button
                buttonStyle
                { onPress = Just PressedCreateQuestion
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
    -> Maybe Time.Posix
    -> Maybe Time.Posix
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
            , Element.htmlAttribute <| Html.Attributes.id questionsViewId
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
                |> List.sortBy (\( _, pinTime, _ ) -> Time.posixToMillis pinTime)
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
                                compare (Time.posixToMillis a.creationTime) (Time.posixToMillis b.creationTime)
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


emptyContainer : CryptographicKey QnaSessionId -> Bool -> Maybe Time.Posix -> List (Element FrontendMsg)
emptyContainer qnaSessionId isHost maybeCopiedUrl =
    if isHost then
        [ Element.el [ Element.centerX, Element.Font.size 36 ] (Element.text "You are the host!")
        , Element.column
            [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.paragraph
                [ Element.Font.center, Element.Font.size 20 ]
                [ Element.text "Copy the link below so people can ask you questions:" ]
            , Element.Input.button
                [ Element.centerX
                , Element.Font.size 20
                , Element.onRight <|
                    case maybeCopiedUrl of
                        Just copiedUrl ->
                            Element.Keyed.el
                                []
                                ( Time.posixToMillis copiedUrl |> String.fromInt
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
                { onPress = Just PressedCopyUrl
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


questionView : Bool -> Bool -> Maybe Time.Posix -> Bool -> UserId -> QuestionId -> Question -> Element FrontendMsg
questionView isFirstUnpinnedQuestion isPinned currentTime isHost userId questionId question =
    animatedRow
        (Animation.fromTo
            { duration = 1500, options = [] }
            [ Property.backgroundColor
                (if question.isPinned == Nothing then
                    if
                        currentTime
                            |> Maybe.map (\time -> Question.isNewQuestion time question)
                            |> Maybe.withDefault False
                    then
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
            Element.Input.button
                (buttonStyle ++ [ Element.Font.size 18, Element.padding 8 ])
                { onPress = Just (PressedTogglePin questionId)
                , label =
                    Element.text
                        (case question.isPinned of
                            Just _ ->
                                "Unpin"

                            Nothing ->
                                "Pin"
                        )
                }

          else if Question.isCreator userId questionId && not isPinned then
            Element.Input.button
                (buttonStyle ++ [ Element.Font.size 16, Element.paddingXY 4 6 ])
                { onPress = Just (PressedDeleteQuestion questionId)
                , label =
                    Element.text "🗑️"
                }

          else
            Element.none
        ]


upvoteButton : QuestionId -> Question -> Element FrontendMsg
upvoteButton questionId question =
    Element.Input.button
        [ Element.Border.rounded 999
        , Element.padding 8
        , Element.Background.color <|
            if question.isUpvoted then
                Element.rgb 0.65 0.65 0.65

            else
                Element.rgb 0.9 0.9 0.9
        , Element.width <| Element.px 48
        , Element.height <| Element.px 48
        , Element.Border.width 2
        , Element.Border.color <| Element.rgb 0.4 0.4 0.4
        ]
        { onPress = Just (PressedToggleUpvote questionId)
        , label =
            Element.row
                [ Element.centerX, Element.centerY, Element.Font.size 16, Element.spacing 2 ]
                [ Element.text (String.fromInt (Question.votes question))
                , Element.el [ Element.Font.size 14 ] (Element.text "❤️")
                ]
        }


buttonStyle : List (Element.Attr () msg)
buttonStyle =
    [ Element.Background.color <| Element.rgb 0.8 0.8 0.8
    , Element.padding 16
    , Element.Border.rounded 4
    ]
