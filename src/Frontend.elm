module Frontend exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Lamdera
import Network
import String.Nonempty as NonemptyString exposing (NonemptyString(..))
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
                    case NonemptyString.fromString success.question of
                        Just nonempty ->
                            let
                                localMsg =
                                    CreateQuestion nonempty
                            in
                            ( { success
                                | networkModel = Network.updateFromUser (LocalMsg localMsg) success.networkModel
                                , question = ""
                                , pressedCreateQuestion = False
                              }
                            , Lamdera.sendToBackend (LocalMsgRequest success.qnaSessionId localMsg)
                            )

                        Nothing ->
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
                        | networkModel = Network.updateFromUser (LocalMsg localMsg) success.networkModel
                      }
                    , Lamdera.sendToBackend (LocalMsgRequest success.qnaSessionId localMsg)
                    )
                )
                model


updateSuccessState : (SuccessModel -> ( SuccessModel, Cmd FrontendMsg )) -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateSuccessState updateFunc model =
    case model.remoteData of
        Success success ->
            updateFunc success
                |> Tuple.mapFirst (\a -> { model | remoteData = Success a })

        _ ->
            ( model, Cmd.none )


toggleUpvote questionId qnaSession =
    { qnaSession
        | questions =
            Dict.update
                questionId
                (Maybe.map (\question -> { question | isUpvoted = not question.isUpvoted }))
                qnaSession.questions
    }


pinQuestion questionId qnaSession =
    { qnaSession
        | questions =
            Dict.update
                questionId
                (Maybe.map (\question -> { question | isRead = not question.isRead }))
                qnaSession.questions
    }


qnaSessionUpdate : QnaMsg -> QnaSession -> QnaSession
qnaSessionUpdate msg qnaSession =
    case msg of
        LocalMsg (ToggleUpvote questionId) ->
            toggleUpvote questionId qnaSession

        LocalMsg (CreateQuestion content) ->
            let
                questionId : QuestionId
                questionId =
                    Types.getQuestionId qnaSession.questions qnaSession.userId
            in
            { qnaSession
                | questions =
                    Dict.insert
                        questionId
                        { creationTime = Time.millisToPosix 0
                        , content = content
                        , isRead = False
                        , votes = 0
                        , isUpvoted = False
                        }
                        qnaSession.questions
            }

        LocalMsg (PinQuestion _ questionId) ->
            pinQuestion questionId qnaSession

        ServerMsg (ToggleUpvoteResponse questionId) ->
            toggleUpvote questionId qnaSession

        ServerMsg (NewQuestion questionId creationTime content) ->
            { qnaSession
                | questions =
                    Dict.insert questionId
                        { creationTime = creationTime
                        , content = content
                        , isRead = False
                        , votes = 0
                        , isUpvoted = False
                        }
                        qnaSession.questions
            }

        ServerMsg (CreateQuestionResponse questionId creationTime) ->
            { qnaSession
                | questions =
                    Dict.update
                        questionId
                        (Maybe.map
                            (\question -> { question | creationTime = creationTime })
                        )
                        qnaSession.questions
            }

        ServerMsg (PinQuestionResponse questionId) ->
            pinQuestion questionId qnaSession

        ServerMsg (VotesChanged questionId voteCount) ->
            { qnaSession
                | questions =
                    Dict.update
                        questionId
                        (Maybe.map
                            (\question -> { question | votes = voteCount })
                        )
                        qnaSession.questions
            }

        ServerMsg (QuestionPinned questionId) ->
            { qnaSession
                | questions =
                    Dict.update
                        questionId
                        (Maybe.map
                            (\question -> { question | isRead = not question.isRead })
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
                                Network.updateFromBackend
                                    qnaSessionUpdate
                                    (ServerMsg serverQnaMsg)
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
                                        Success (initSuccessModel qnaSessionId (Debug.log "qnaSession" qnaSession))

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
                            Success (initSuccessModel qnaSessionId (initQnaSession qnaSessionName))
                    }

                _ ->
                    model
            , Browser.Navigation.pushUrl model.key (urlEncoder qnaSessionId)
            )


view : FrontendModel -> { title : String, body : List (Html FrontendMsg) }
view model =
    { title = "Questions & Answers"
    , body =
        [ Element.layout
            []
            (case model.remoteData of
                NotAsked ->
                    Element.column
                        [ Element.centerX, Element.centerY, Element.spacing 16, Element.paddingXY 16 0 ]
                        [ Element.paragraph
                            [ Element.centerX ]
                            [ Element.text "If you want to join a Q&A session, please use the link your host has provided." ]
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

                Failure error ->
                    Element.paragraph [] [ Element.text "That Q&A session doesn't exist." ]

                Success success ->
                    let
                        qnaSession : QnaSession
                        qnaSession =
                            Network.localState qnaSessionUpdate success.networkModel
                    in
                    Element.column
                        [ Element.spacing 8
                        , Element.width <| Element.maximum 800 Element.fill
                        , Element.centerX
                        , Element.paddingXY 0 16
                        ]
                        [ Element.paragraph [] [ Element.text (NonemptyString.toString qnaSession.name) ]
                        , questionsView qnaSession.questions
                        , Element.Input.multiline
                            [ Element.height <| Element.px 120 ]
                            { onChange = TypedQuestion
                            , placeholder = Nothing
                            , spellcheck = True
                            , label =
                                Element.Input.labelAbove
                                    []
                                    (Element.text "What do you want to ask?")
                            , text = success.question
                            }
                        , Element.row [ Element.spacing 16 ]
                            [ Element.Input.button
                                buttonStyle
                                { onPress = Just PressedCreateQuestion
                                , label =
                                    Element.text "Submit question"
                                }
                            , case ( NonemptyString.fromString success.question, success.pressedCreateQuestion ) of
                                ( Nothing, True ) ->
                                    Element.paragraph
                                        [ Element.Font.color <| Element.rgb 0.8 0.3 0.3 ]
                                        [ Element.text "Write something first!" ]

                                _ ->
                                    Element.none
                            ]
                        ]
            )
        ]
    }


questionsView : Dict QuestionId Question -> Element FrontendMsg
questionsView questions =
    Dict.toList questions
        |> List.sortBy (Tuple.second >> .creationTime >> Time.posixToMillis)
        |> List.map questionView
        |> Element.column
            [ Element.spacing 8
            , Element.height (Element.px 300)
            , Element.Background.color <| Element.rgb 0.9 0.9 0.9
            , Element.width Element.fill
            , Element.Border.rounded 4
            , Element.scrollbars
            ]


questionView : ( QuestionId, Question ) -> Element FrontendMsg
questionView ( questionId, question ) =
    Element.row
        [ Element.padding 8, Element.spacing 16 ]
        [ Element.Input.button
            [ Element.Border.rounded 99999
            , Element.padding 8
            , Element.Background.color <| Element.rgb 0.5 0.5 0.5
            , Element.width <| Element.px 56
            , Element.height <| Element.px 56
            , Element.Border.width 2
            , Element.Border.color <| Element.rgb 0.4 0.4 0.4
            ]
            { onPress = Just (PressedToggledUpvote questionId)
            , label =
                Element.el
                    [ Element.centerX, Element.centerY ]
                    (Element.text
                        ("👍" ++ String.fromInt question.votes)
                    )
            }
        , Element.paragraph [] [ Element.text (NonemptyString.toString question.content) ]
        ]


buttonStyle : List (Element.Attr () msg)
buttonStyle =
    [ Element.Background.color <| Element.rgb 0.8 0.8 0.8
    , Element.padding 16
    , Element.Border.rounded 4
    ]
