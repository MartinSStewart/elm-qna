module Frontend exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Keyed
import Element.Lazy
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
import Types exposing (..)
import Url
import Url.Parser


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    case Url.Parser.parse urlDecoder url of
        Just (Just qnaSessionId) ->
            ( { key = key, remoteData = Loading qnaSessionId }
            , Lamdera.sendToBackend (GetQnaSession qnaSessionId)
            )

        _ ->
            ( { key = key, remoteData = NotAsked }
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
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        PressedCreateQnaSession ->
            let
                name =
                    NonemptyString 'T' "est"
            in
            ( { model | remoteData = Creating name }, Lamdera.sendToBackend (CreateQnaSession name) )

        TypedQuestion text ->
            case model.remoteData of
                Success success ->
                    ( { model | remoteData = Success { success | question = text } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PressedCreateQuestion ->
            updateSuccessState
                (\success ->
                    case valiatedQuestion success.question of
                        Ok nonempty ->
                            let
                                localMsg =
                                    CreateQuestion nonempty
                            in
                            ( { success
                                | networkModel =
                                    Network.updateFromUser
                                        success.localChangeCounter
                                        localMsg
                                        success.networkModel
                                , question = ""
                                , pressedCreateQuestion = False
                                , localChangeCounter = Network.incrementChangeId success.localChangeCounter
                              }
                            , Cmd.batch
                                [ Lamdera.sendToBackend
                                    (LocalMsgRequest success.qnaSessionId success.localChangeCounter localMsg)
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
                            ( { success | pressedCreateQuestion = True }
                            , Cmd.none
                            )
                )
                model

        PressedToggledUpvote questionId ->
            updateSuccessState
                (\success ->
                    let
                        localMsg =
                            ToggleUpvote questionId
                    in
                    ( { success
                        | networkModel =
                            Network.updateFromUser
                                success.localChangeCounter
                                localMsg
                                success.networkModel
                        , localChangeCounter = Network.incrementChangeId success.localChangeCounter
                      }
                    , Lamdera.sendToBackend (LocalMsgRequest success.qnaSessionId success.localChangeCounter localMsg)
                    )
                )
                model

        PressedCloseHostBanner ->
            updateSuccessState
                (\success -> ( { success | closedHostBanner = True }, Cmd.none ))
                model


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


updateSuccessState : (SuccessModel -> ( SuccessModel, Cmd FrontendMsg )) -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateSuccessState updateFunc model =
    case model.remoteData of
        Success success ->
            updateFunc success
                |> Tuple.mapFirst (\a -> { model | remoteData = Success a })

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


pinQuestion : QuestionId -> QnaSession -> QnaSession
pinQuestion questionId qnaSession =
    { qnaSession
        | questions =
            Dict.update
                questionId
                (Maybe.map (\question -> { question | isPinned = not question.isPinned }))
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
                , isPinned = False
                , otherVotes = 0
                , isUpvoted = False
                , isNewQuestion = True
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

        LocalChange _ (PinQuestion questionId) ->
            pinQuestion questionId qnaSession

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

        ConfirmLocalChange _ localChange PinQuestionResponse ->
            case localChange of
                PinQuestion questionId ->
                    pinQuestion questionId qnaSession

                _ ->
                    qnaSession

        ServerChange (NewQuestion questionId creationTime content) ->
            { qnaSession
                | questions =
                    Dict.insert questionId
                        { creationTime = creationTime
                        , content = content
                        , isPinned = False
                        , otherVotes = 0
                        , isUpvoted = False
                        , isNewQuestion = True
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

        ServerChange (QuestionPinned questionId) ->
            { qnaSession
                | questions =
                    Dict.update
                        questionId
                        (Maybe.map
                            (\question -> { question | isPinned = not question.isPinned })
                        )
                        qnaSession.questions
            }


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        ServerMsgResponse qnaSessionId serverQnaMsg ->
            updateSuccessState
                (\success ->
                    ( if success.qnaSessionId == qnaSessionId then
                        { success
                            | networkModel =
                                Network.serverChange qnaSessionUpdate serverQnaMsg success.networkModel
                        }

                      else
                        success
                    , Cmd.none
                    )
                )
                model

        LocalConfirmQnaMsgResponse qnaSessionId changeId confirmLocalMsg ->
            updateSuccessState
                (\success ->
                    ( if success.qnaSessionId == qnaSessionId then
                        { success
                            | networkModel =
                                Network.confirmLocalChange
                                    qnaSessionUpdate
                                    changeId
                                    confirmLocalMsg
                                    success.networkModel
                        }

                      else
                        success
                    , Cmd.none
                    )
                )
                model

        GetQnaSessionResponse qnaSessionId result ->
            ( case model.remoteData of
                Loading qnaSessionId_ ->
                    if qnaSessionId == qnaSessionId_ then
                        { model
                            | remoteData =
                                case result of
                                    Ok qnaSession ->
                                        Success (initSuccessModel qnaSessionId qnaSession)

                                    Err () ->
                                        Failure ()
                        }

                    else
                        model

                _ ->
                    model
            , Cmd.none
            )

        CreateQnaSessionResponse qnaSessionId ->
            ( case model.remoteData of
                Creating qnaSessionName ->
                    { model
                        | remoteData =
                            Success (initSuccessModel qnaSessionId (initQnaSession qnaSessionName True))
                    }

                _ ->
                    model
            , Browser.Navigation.pushUrl model.key (urlEncoder qnaSessionId)
            )


view : FrontendModel -> { title : String, body : List (Html FrontendMsg) }
view model =
    { title = "Q&A"
    , body =
        [ Element.layout
            []
            (case model.remoteData of
                NotAsked ->
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

                Loading qnaSessionId ->
                    Element.text "Loading..."

                Creating _ ->
                    Element.text "Creating..."

                Failure () ->
                    Element.paragraph [] [ Element.text "That Q&A session doesn't exist." ]

                Success success ->
                    let
                        qnaSession : QnaSession
                        qnaSession =
                            Network.localState qnaSessionUpdate success.networkModel
                    in
                    Element.el
                        [ Element.inFront
                            (if success.closedHostBanner || not qnaSession.isHost then
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
                            [ --Element.paragraph [] [ Element.text (NonemptyString.toString qnaSession.name) ]
                              Element.column
                                [ Element.width Element.fill, Element.height Element.fill, Element.spacing 6 ]
                                [ Element.text "Questions", questionsView qnaSession.questions ]
                            , questionInputView success
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
        [ Element.text "You are the host. To invite people, copy the url address. "
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


questionInputView : SuccessModel -> Element FrontendMsg
questionInputView success =
    Element.column
        [ Element.width Element.fill, Element.spacing 16 ]
        [ Element.el
            [ Element.inFront <|
                Element.el
                    [ Element.alignBottom
                    , Element.alignRight
                    , Element.Font.color <|
                        if String.length success.question > maxQuestionChars then
                            errorColor

                        else
                            Element.rgb 0.2 0.2 0.2
                    , Element.Font.size 18
                    , Element.moveLeft 24
                    , Element.moveUp 4
                    ]
                    (Element.text
                        (String.fromInt (String.length success.question)
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
                , text = success.question
                }
            )
        , Element.row [ Element.spacing 16 ]
            [ Element.Input.button
                buttonStyle
                { onPress = Just PressedCreateQuestion
                , label =
                    Element.text "Submit question"
                }
            , case ( valiatedQuestion success.question, success.pressedCreateQuestion ) of
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


errorColor =
    Element.rgb 0.8 0.2 0.2


questionsView : Dict QuestionId Question -> Element FrontendMsg
questionsView questions =
    Dict.toList questions
        |> List.sortBy (Tuple.second >> .creationTime >> Time.posixToMillis)
        |> List.sortBy (Tuple.second >> Question.votes >> negate)
        |> List.map
            (\( (QuestionId (UserId userId) questionIndex) as questionId, question ) ->
                ( String.fromInt userId ++ " " ++ String.fromInt questionIndex
                , questionView questionId question
                )
            )
        |> Element.Keyed.column
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.Border.rounded 4
            , Element.scrollbars
            , Element.htmlAttribute <| Html.Attributes.id questionsViewId
            , Element.Border.width 1
            , Element.Border.color <| Element.rgb 0.5 0.5 0.5
            ]


animatedUi : (List (Element.Attribute msg) -> children -> Element msg) -> Animation -> List (Element.Attribute msg) -> children -> Element msg
animatedUi =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }


animatedColumn : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
animatedColumn =
    animatedUi Element.column


animatedRow : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
animatedRow =
    animatedUi Element.row


questionView : QuestionId -> Question -> Element FrontendMsg
questionView questionId question =
    (if question.isNewQuestion then
        animatedRow
            (Animation.fromTo
                { duration = 2000, options = [] }
                [ Property.backgroundColor "lightgreen" ]
                [ Property.backgroundColor "white" ]
            )

     else
        Element.row
    )
        [ Element.padding 8, Element.spacing 16, Element.width Element.fill ]
        [ upvoteButton questionId question
        , Element.paragraph [] [ Element.text (NonemptyString.toString question.content) ]
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
        { onPress = Just (PressedToggledUpvote questionId)
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
