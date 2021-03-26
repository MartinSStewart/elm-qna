module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
import Element.Font
import Element.Input
import Html
import Html.Attributes as Attr
import Lamdera
import Network
import String.Nonempty as NonemptyString exposing (NonemptyString(..))
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


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
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


urlDecoder : Url.Parser.Parser (Maybe QnaSessionId -> c) c
urlDecoder =
    Url.Parser.oneOf
        [ Url.Parser.top |> Url.Parser.map Nothing
        , Url.Parser.string |> Url.Parser.map (QnaSessionId >> Just)
        ]


urlEncoder : QnaSessionId -> String
urlEncoder (QnaSessionId qnaSessionId) =
    "/" ++ qnaSessionId


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
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
                                    LocalMsg (CreateQuestion success.qnaSessionId nonempty)
                            in
                            ( { success
                                | networkModel = Network.updateFromUser localMsg success.networkModel
                              }
                            , Lamdera.sendToBackend localMsg
                            )

                        Nothing ->
                            ( { success | pressedCreateQuestion = True }
                            , Cmd.none
                            )
                )
                model

        PressedToggledUpvote ->
            Debug.todo ""


updateSuccessState : (SuccessModel -> ( SuccessModel, Cmd FrontendMsg )) -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateSuccessState updateFunc model =
    case model.remoteData of
        Success success ->
            updateFunc success
                |> Tuple.mapFirst (\a -> { model | remoteData = Success a })

        _ ->
            ( model, Cmd.none )


qnaSessionUpdate : QnaMsg -> QnaSession -> QnaSession
qnaSessionUpdate msg model =
    case msg of
        LocalMsg (ToggleUpvote questionId) ->
            Debug.todo ""

        LocalMsg (CreateQuestion nonempty) ->
            Debug.todo ""

        ServerMsg (VotesChanged questionId) ->
            Debug.todo ""

        ServerMsg (NewQuestion questionId creationTime nonempty) ->
            Debug.todo ""

        ServerMsg (CreateQuestionResponse questionId creationTime) ->
            Debug.todo ""

        ServerMsg (QuestionReadToggled questionId) ->
            Debug.todo ""


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        ServerMsgResponse serverQnaMsg ->
            ( case model.remoteData of
                Success _ qnaState ->
                    { model
                        | remoteData =
                            Network.updateFromBackend
                                qnaSessionUpdate
                                (ServerMsg serverQnaMsg)
                                qnaState
                                |> Success
                    }

                _ ->
                    model
            , Cmd.none
            )

        GetQnaSessionResponse qnaSessionId result ->
            ( case model.remoteData of
                Loading qnaSessionId_ ->
                    if qnaSessionId == qnaSessionId_ then
                        { model
                            | remoteData =
                                case result of
                                    Ok qnaSession ->
                                        Success qnaSessionId (Network.init qnaSession)

                                    Err () ->
                                        Failure ()
                        }

                    else
                        model

                _ ->
                    model
            , Cmd.none
            )


view model =
    { title = "Questions & Answers"
    , body =
        [ Element.layout
            []
            (case model.remoteData of
                NotAsked ->
                    Element.column
                        [ Element.centerX, Element.centerY, Element.spacing 16 ]
                        [ Element.paragraph
                            []
                            [ Element.text "If you want to join a Q&A session, please use the link your host has provided." ]
                        , Element.el [ Element.Font.size 24 ] (Element.text "OR")
                        , Element.Input.button
                            []
                            { onPress = Just PressedCreateQnaSession
                            , label = Element.paragraph [] [ Element.text "Create a new Q&A session" ]
                            }
                        ]

                Loading qnaSessionId ->
                    Element.text "Loading..."

                Failure error ->
                    Element.paragraph [] [ Element.text "That Q&A session doesn't exist." ]

                Success qnaSessionId networkModel ->
                    Debug.todo ""
            )
        ]
    }
