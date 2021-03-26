module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
import Element.Font
import Element.Input
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
                                    CreateQuestion nonempty
                            in
                            ( { success
                                | networkModel = Network.updateFromUser (LocalMsg localMsg) success.networkModel
                              }
                            , Lamdera.sendToBackend (LocalMsgRequest success.qnaSessionId localMsg)
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

        LocalMsg (PinQuestion hostKey questionId) ->
            Debug.todo ""

        ServerMsg (ToggleUpvoteResponse questionId) ->
            Debug.todo ""

        ServerMsg (NewQuestion questionId creationTime nonempty) ->
            Debug.todo ""

        ServerMsg (CreateQuestionResponse questionId creationTime) ->
            Debug.todo ""

        ServerMsg (PinQuestionResponse _) ->
            Debug.todo ""

        ServerMsg (VotesChanged _ _) ->
            Debug.todo ""

        ServerMsg (QuestionPinned _) ->
            Debug.todo ""


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
                            Success (initSuccessModel qnaSessionId (initQnaSession qnaSessionName))
                    }

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

                Creating _ ->
                    Element.text "Creating..."

                Failure error ->
                    Element.paragraph [] [ Element.text "That Q&A session doesn't exist." ]

                Success success ->
                    Debug.todo ""
            )
        ]
    }
