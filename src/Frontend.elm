module Frontend exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation
import Csv.Encode
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
import Json.Decode
import Lamdera
import Network exposing (Change(..))
import Question exposing (Question)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as Property
import String.Nonempty as NonemptyString exposing (NonemptyString(..))
import Task exposing (Task)
import Time
import Types
    exposing
        ( ConfirmLocalQnaMsg(..)
        , CryptographicKey(..)
        , FrontendModel
        , FrontendMsg(..)
        , FrontendStatus(..)
        , InQnaSession_
        , LocalQnaMsg(..)
        , QnaSession
        , QnaSessionId
        , QuestionId(..)
        , ServerQnaMsg(..)
        , ToBackend(..)
        , ToFrontend(..)
        , UserId(..)
        )
import Url
import Url.Parser


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Time.every 1000 GotCurrentTime
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    case Url.Parser.parse urlDecoder url of
        Just (Just qnaSessionId) ->
            ( { key = key, remoteData = LoadingQnaSession qnaSessionId, currentTime = Nothing }
            , Lamdera.sendToBackend (GetQnaSession qnaSessionId)
            )

        _ ->
            ( { key = key, remoteData = Homepage, currentTime = Nothing }
            , Cmd.none
            )


urlDecoder : Url.Parser.Parser (Maybe (CryptographicKey QnaSessionId) -> c) c
urlDecoder =
    Url.Parser.oneOf
        [ Url.Parser.top |> Url.Parser.map Nothing
        , Url.Parser.string |> Url.Parser.map (CryptographicKey >> Just)
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
                Just Nothing ->
                    ( model, Browser.Navigation.load "/" )

                Just (Just qnaSessionId) ->
                    case model.remoteData of
                        CreatingQnaSession qnaSessionName ->
                            ( { model
                                | remoteData =
                                    InQnaSession
                                        (Types.initInQnaSession
                                            qnaSessionId
                                            (Types.initQnaSession qnaSessionName True)
                                        )
                              }
                            , Cmd.none
                            )

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
                                    CreateQuestion nonempty
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

        PressedCloseHostBanner ->
            updateInQnaSession
                (\inQnaSession -> ( { inQnaSession | closedHostBanner = True }, Cmd.none ))
                model

        PressedTogglePin questionId ->
            updateInQnaSession
                (let
                    localMsg =
                        TogglePin
                            questionId
                            -- We just need a time value bigger than everything else until the backend can give us the actual time
                            (Time.millisToPosix 999999999999999999)
                 in
                 addLocalChange localMsg
                )
                model

        GotCurrentTime currentTime ->
            ( { model | currentTime = Just currentTime }, Cmd.none )

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
                                        , ( "question", NonemptyString.toString question.content )
                                        ]
                                    )
                            , fieldSeparator = ','
                            }
                            (Dict.values (Network.localState qnaSessionUpdate inQnaSession.networkModel).questions
                                |> List.sortBy (.creationTime >> Time.posixToMillis)
                            )
                        )
                    )
                )
                model


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


createQuestion : Maybe Time.Posix -> NonemptyString -> QnaSession -> QnaSession
createQuestion maybeTime content qnaSession =
    let
        questionId : QuestionId
        questionId =
            Types.getQuestionId qnaSession.questions qnaSession.userId
    in
    { qnaSession
        | questions =
            Dict.insert
                questionId
                { creationTime = Maybe.withDefault (Time.millisToPosix 0) maybeTime
                , content = content
                , isPinned = Nothing
                , otherVotes = 0
                , isUpvoted = False
                }
                qnaSession.questions
    }


qnaSessionUpdate : Change LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg -> QnaSession -> QnaSession
qnaSessionUpdate msg qnaSession =
    case msg of
        LocalChange _ (ToggleUpvote questionId) ->
            toggleUpvote questionId qnaSession

        LocalChange _ (CreateQuestion content) ->
            createQuestion Nothing content qnaSession

        LocalChange _ (TogglePin questionId pinTime) ->
            pinQuestion questionId pinTime qnaSession

        ConfirmLocalChange _ localChange ToggleUpvoteResponse ->
            case localChange of
                ToggleUpvote questionId ->
                    toggleUpvote questionId qnaSession

                _ ->
                    qnaSession

        ConfirmLocalChange _ localChange (CreateQuestionResponse creationTime) ->
            case localChange of
                CreateQuestion content ->
                    createQuestion (Just creationTime) content qnaSession

                _ ->
                    qnaSession

        ConfirmLocalChange _ localChange (PinQuestionResponse pinTime) ->
            case localChange of
                TogglePin questionId _ ->
                    pinQuestion questionId pinTime qnaSession

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
                                    Ok qnaSession ->
                                        InQnaSession (Types.initInQnaSession qnaSessionId qnaSession)

                                    Err () ->
                                        LoadingQnaSessionFailed ()
                        }

                    else
                        model

                _ ->
                    model
            , Cmd.none
            )

        CreateQnaSessionResponse qnaSessionId ->
            ( model
            , Browser.Navigation.pushUrl model.key (urlEncoder qnaSessionId)
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

                LoadingQnaSession qnaSessionId ->
                    Element.text "Loading..."

                CreatingQnaSession _ ->
                    Element.text "Creating..."

                LoadingQnaSessionFailed () ->
                    Element.paragraph [] [ Element.text "That Q&A session doesn't exist." ]

                InQnaSession inQnaSession ->
                    let
                        qnaSession : QnaSession
                        qnaSession =
                            Network.localState qnaSessionUpdate inQnaSession.networkModel
                    in
                    Element.el
                        [ Element.inFront
                            (if inQnaSession.closedHostBanner || not qnaSession.isHost then
                                Element.none

                             else
                                hostBannerView
                            )
                        , Element.width Element.fill
                        , Element.height Element.fill
                        ]
                        (Element.column
                            [ Element.spacing 16
                            , Element.width <| Element.maximum 800 Element.fill
                            , Element.centerX
                            , Element.paddingXY 16 16
                            , Element.height <| Element.maximum 800 Element.fill
                            ]
                            [ Element.column
                                [ Element.width Element.fill, Element.height Element.fill, Element.spacing 6 ]
                                [ Element.text "Questions"
                                , questionsView
                                    model.currentTime
                                    qnaSession.isHost
                                    qnaSession.questions
                                ]
                            , if qnaSession.isHost then
                                if Dict.isEmpty qnaSession.questions then
                                    Element.none

                                else
                                    animatedParagraph
                                        (Animation.fromTo
                                            { duration = 2000, options = [] }
                                            [ Property.opacity 0 ]
                                            [ Property.opacity 1 ]
                                        )
                                        [ Element.Font.size 16 ]
                                        [ Element.text "Questions will be deleted after 2 days of inactivity. If you want to save them, "
                                        , Element.Input.button
                                            [ Element.Font.color <| Element.rgb 0.2 0.2 1
                                            , Element.Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
                                            , Element.Border.color <| Element.rgba 0 0 0 0
                                            , Element.mouseOver [ Element.Border.color <| Element.rgb 0.2 0.2 1 ]
                                            ]
                                            { onPress = Just PressedDownloadQuestions, label = Element.text "click here" }
                                        , Element.text " to download them as a spreadsheet."
                                        ]

                              else
                                questionInputView inQnaSession
                            ]
                        )
            )
        ]
    }


hostBannerView : Element FrontendMsg
hostBannerView =
    Element.paragraph
        [ Element.width Element.fill
        , Element.Background.color <| Element.rgb 0.9 0.9 0.7
        , Element.paddingXY 8 12
        , Element.Font.center
        , Element.spacing 8
        ]
        [ Element.text "You are the host. To invite people, copy the url in the address bar. "
        , Element.Input.button
            [ Element.Background.color <| Element.rgb 0.8 0.8 0.8
            , Element.paddingXY 8 4
            , Element.Border.rounded 4
            ]
            { onPress = Just PressedCloseHostBanner
            , label = Element.text "OK"
            }
        ]


domain =
    "https://question-and-answer.lamdera.app/"


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


questionsView : Maybe Time.Posix -> Bool -> Dict QuestionId Question -> Element FrontendMsg
questionsView currentTime isHost questions =
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
                        keyedQuestion False ( questionId, question )
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
                        keyedQuestion (index == 0 && not (List.isEmpty pinnedQuestions)) value
                    )

        keyedQuestion : Bool -> ( QuestionId, Question ) -> ( String, Element FrontendMsg )
        keyedQuestion isFirstUnpinnedQuestion ( (QuestionId (UserId userId) questionIndex) as questionId, question ) =
            ( String.fromInt userId ++ " " ++ String.fromInt questionIndex
            , questionView isFirstUnpinnedQuestion currentTime isHost questionId question
            )
    in
    if Dict.isEmpty questions then
        Element.el containerStyle
            (Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Font.size 32
                , Element.Font.color <| Element.rgb 0.6 0.6 0.6
                ]
                (Element.text "No questions yet...")
            )

    else
        pinnedQuestions
            ++ unpinnedQuestions
            |> Element.Keyed.column containerStyle


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


questionView : Bool -> Maybe Time.Posix -> Bool -> QuestionId -> Question -> Element FrontendMsg
questionView isFirstUnpinnedQuestion currentTime isHost questionId question =
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
        [ Element.padding 8
        , Element.spacing 16
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
        , Element.width <| Element.px 56
        , Element.height <| Element.px 56
        , Element.Border.width 2
        , Element.Border.color <| Element.rgb 0.4 0.4 0.4
        ]
        { onPress = Just (PressedToggleUpvote questionId)
        , label =
            Element.row
                [ Element.centerX, Element.centerY, Element.Font.size 18, Element.spacing 2 ]
                [ Element.text (String.fromInt (Question.votes question))
                , Element.el [ Element.Font.size 16 ] (Element.text "❤️")
                ]
        }


buttonStyle : List (Element.Attr () msg)
buttonStyle =
    [ Element.Background.color <| Element.rgb 0.8 0.8 0.8
    , Element.padding 16
    , Element.Border.rounded 4
    ]
