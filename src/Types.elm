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
    , closingDateText : String
    , closingTimeText : String
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
    , isHost : Maybe (CryptographicKey HostSecret)
    }


initInQnaSession : CryptographicKey QnaSessionId -> QnaSession -> Maybe (CryptographicKey HostSecret) -> InQnaSession_
initInQnaSession qnaSessionId qnaSesssion hostStatus =
    { qnaSessionId = qnaSessionId
    , networkModel = Network.init qnaSesssion
    , question = ""
    , pressedCreateQuestion = False
    , localChangeCounter = Network.initChangeId
    , copiedHostUrl = Nothing
    , copiedUrl = Nothing
    , isHost = hostStatus
    }


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


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded QuestionId
    | VoteRemoved QuestionId
    | NewQuestion QuestionId Time.Posix NonemptyString
    | QuestionPinned QuestionId (Maybe Time.Posix)
    | QuestionDeleted QuestionId


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
