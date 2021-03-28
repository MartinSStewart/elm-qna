module Evergreen.V4.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Evergreen.V4.Id
import Evergreen.V4.Network
import Evergreen.V4.Question
import Lamdera
import String.Nonempty
import Time
import Url


type LocalQnaMsg
    = ToggleUpvote Evergreen.V4.Question.QuestionId
    | CreateQuestion String.Nonempty.NonemptyString
    | TogglePin Evergreen.V4.Question.QuestionId Time.Posix
    | DeleteQuestion Evergreen.V4.Question.QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded Evergreen.V4.Question.QuestionId
    | VoteRemoved Evergreen.V4.Question.QuestionId
    | NewQuestion Evergreen.V4.Question.QuestionId Time.Posix String.Nonempty.NonemptyString
    | QuestionPinned Evergreen.V4.Question.QuestionId (Maybe Time.Posix)
    | QuestionDeleted Evergreen.V4.Question.QuestionId


type alias QnaSession = 
    { questions : (AssocList.Dict Evergreen.V4.Question.QuestionId Evergreen.V4.Question.Question)
    , name : String.Nonempty.NonemptyString
    , userId : Evergreen.V4.Id.UserId
    , isHost : Bool
    }


type alias InQnaSession_ = 
    { qnaSessionId : (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId)
    , networkModel : (Evergreen.V4.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg QnaSession)
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V4.Network.ChangeId
    , closedHostBanner : Bool
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId)
    | CreatingQnaSession String.Nonempty.NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , remoteData : FrontendStatus
    , currentTime : (Maybe Time.Posix)
    }


type alias BackendQnaSession = 
    { questions : (AssocList.Dict Evergreen.V4.Question.QuestionId Evergreen.V4.Question.BackendQuestion)
    , host : Lamdera.SessionId
    , creationTime : Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : (AssocList.Dict Lamdera.ClientId Evergreen.V4.Id.UserId)
    , connectionCounter : Int
    }


type alias BackendModel =
    { qnaSessions : (AssocList.Dict (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId) BackendQnaSession)
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote Evergreen.V4.Question.QuestionId
    | PressedCloseHostBanner
    | PressedTogglePin Evergreen.V4.Question.QuestionId
    | GotCurrentTime Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion Evergreen.V4.Question.QuestionId


type ToBackend
    = LocalMsgRequest (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId) Evergreen.V4.Network.ChangeId LocalQnaMsg
    | GetQnaSession (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId)
    | CreateQnaSession String.Nonempty.NonemptyString


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId) Evergreen.V4.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId) (Result () QnaSession)
    | CreateQnaSessionResponse (Evergreen.V4.Id.CryptographicKey Evergreen.V4.Id.QnaSessionId)