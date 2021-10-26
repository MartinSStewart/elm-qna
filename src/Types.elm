module Types exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest)
import Date exposing (Date)
import Effect.Browser.Navigation as Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time as Time
import Id exposing (CryptographicKey, HostSecret, QnaSessionId, UserId(..))
import Network exposing (ChangeId, NetworkModel)
import QnaSession exposing (BackendQnaSession, HostStatus, QnaSession)
import Question exposing (BackendQuestion, Question, QuestionId(..))
import String.Nonempty exposing (NonemptyString)
import Url exposing (Url)


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendLoading =
    { time : Maybe Time.Posix, timezone : Maybe Time.Zone, key : Navigation.Key, route : Maybe Route }


type alias FrontendLoaded =
    { key : Navigation.Key
    , remoteData : FrontendStatus
    , time : Time.Posix
    , timezone : Time.Zone
    , lastConnectionCheck : Maybe Time.Posix
    , gotFirstConnectMsg : Bool
    }


type Route
    = HomepageRoute
    | HostInviteRoute (CryptographicKey HostSecret)
    | QnaSessionRoute (CryptographicKey QnaSessionId)


type FrontendStatus
    = Homepage
    | LoadingQnaSession (CryptographicKey QnaSessionId)
    | LoadingQnaSessionWithHostInvite (CryptographicKey HostSecret)
    | CreatingQnaSession NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias InQnaSession_ =
    { qnaSessionId : CryptographicKey QnaSessionId
    , networkModel : NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : ChangeId
    , copiedHostUrl : Maybe Time.Posix
    , copiedUrl : Maybe Time.Posix
    , isHost : Maybe HostState
    }


type alias HostState =
    { secret : CryptographicKey HostSecret
    , closingDateText : String
    , closingTimeText : String
    , showSettings : Bool
    }


initInQnaSession : Time.Zone -> CryptographicKey QnaSessionId -> QnaSession -> Maybe (CryptographicKey HostSecret) -> InQnaSession_
initInQnaSession timezone qnaSessionId qnaSesssion hostStatus =
    { qnaSessionId = qnaSessionId
    , networkModel = Network.init qnaSesssion
    , question = ""
    , pressedCreateQuestion = False
    , localChangeCounter = Network.initChangeId
    , copiedHostUrl = Nothing
    , copiedUrl = Nothing
    , isHost =
        case hostStatus of
            Just secret ->
                { secret = secret
                , closingDateText =
                    case qnaSesssion.closingTime of
                        Just closingTime ->
                            Date.fromPosix timezone closingTime |> Date.toIsoString

                        Nothing ->
                            ""
                , closingTimeText =
                    case qnaSesssion.closingTime of
                        Just closingTime ->
                            timestamp (Time.toHour timezone closingTime) (Time.toMinute timezone closingTime)

                        Nothing ->
                            ""
                , showSettings = False
                }
                    |> Just

            Nothing ->
                Nothing
    }


{-| Timestamp used by time input field.
-}
timestamp : Int -> Int -> String
timestamp hour minute =
    String.padLeft 2 '0' (String.fromInt hour) ++ ":" ++ String.padLeft 2 '0' (String.fromInt minute)


type alias BackendModel =
    { qnaSessions : Dict (CryptographicKey QnaSessionId) BackendQnaSession
    , keyCounter : Int
    }


getQuestionId : Dict QuestionId v -> UserId -> QuestionId
getQuestionId questions userId =
    Dict.filter
        (\(QuestionId userId_ _) _ -> userId_ == userId)
        questions
        |> Dict.size
        |> QuestionId userId


type LocalQnaMsg
    = ToggleUpvote QuestionId
    | CreateQuestion Time.Posix NonemptyString
    | TogglePin QuestionId Time.Posix
    | DeleteQuestion QuestionId
    | ChangeClosingTime Time.Posix


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse
    | ChangeClosingTimeResponse


type ServerQnaMsg
    = VoteAdded QuestionId
    | VoteRemoved QuestionId
    | NewQuestion QuestionId Time.Posix NonemptyString
    | QuestionPinned QuestionId (Maybe Time.Posix)
    | QuestionDeleted QuestionId
    | ClosingTimeChanged Time.Posix


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote QuestionId
    | PressedTogglePin QuestionId
    | GotCurrentTime Time.Posix
    | GotTimezone Time.Zone
    | PressedDownloadQuestions
    | PressedDeleteQuestion QuestionId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Time.Posix
    | TextInputBlurred
    | TypedClosingDate String
    | TypedClosingTime String
    | PressedToggleShowSettings


type ToBackend
    = LocalMsgRequest (CryptographicKey QnaSessionId) ChangeId LocalQnaMsg
    | GetQnaSession (CryptographicKey QnaSessionId)
    | GetQnaSessionWithHostInvite (CryptographicKey HostSecret)
    | CreateQnaSession NonemptyString
    | CheckIfConnectedRequest


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime SessionId ClientId ToBackend Time.Posix
    | UserDisconnected SessionId ClientId
    | UserConnected SessionId ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (CryptographicKey QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (CryptographicKey QnaSessionId) ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (CryptographicKey QnaSessionId)
        (Result
            ()
            { isHost : Maybe (CryptographicKey HostSecret)
            , qnaSession : QnaSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (CryptographicKey HostSecret) (Result () ( CryptographicKey QnaSessionId, QnaSession ))
    | CreateQnaSessionResponse (CryptographicKey QnaSessionId) (CryptographicKey HostSecret)
    | CheckIfConnectedResponse
    | NewConnection
