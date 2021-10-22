module Types exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest)
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time
import Id exposing (CryptographicKey, HostSecret, QnaSessionId, UserId(..))
import Network exposing (ChangeId, NetworkModel)
import QnaSession exposing (BackendQnaSession, HostStatus, QnaSession)
import Question exposing (BackendQuestion, Question, QuestionId(..))
import String.Nonempty exposing (NonemptyString)
import Url exposing (Url)


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , remoteData : FrontendStatus
    , currentTime : Maybe Effect.Time.Posix
    , lastConnectionCheck : Maybe Effect.Time.Posix
    , gotFirstConnectMsg : Bool
    }


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
    , copiedHostUrl : Maybe Effect.Time.Posix
    , copiedUrl : Maybe Effect.Time.Posix
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
    | CreateQuestion Effect.Time.Posix NonemptyString
    | TogglePin QuestionId Effect.Time.Posix
    | DeleteQuestion QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Effect.Time.Posix
    | PinQuestionResponse Effect.Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded QuestionId
    | VoteRemoved QuestionId
    | NewQuestion QuestionId Effect.Time.Posix NonemptyString
    | QuestionPinned QuestionId (Maybe Effect.Time.Posix)
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
    | GotCurrentTime Effect.Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion QuestionId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Effect.Time.Posix
    | TextInputBlurred


type ToBackend
    = LocalMsgRequest (CryptographicKey QnaSessionId) ChangeId LocalQnaMsg
    | GetQnaSession (CryptographicKey QnaSessionId)
    | GetQnaSessionWithHostInvite (CryptographicKey HostSecret)
    | CreateQnaSession NonemptyString
    | CheckIfConnectedRequest


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime SessionId ClientId ToBackend Effect.Time.Posix
    | UserDisconnected SessionId ClientId
    | UserConnected SessionId ClientId
    | CheckSessions Effect.Time.Posix


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
