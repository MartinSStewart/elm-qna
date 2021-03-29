module Evergreen.V12.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Evergreen.V12.Id
import Evergreen.V12.Network
import Evergreen.V12.QnaSession
import Evergreen.V12.Question
import Lamdera
import String.Nonempty
import Time
import Url


type LocalQnaMsg
    = ToggleUpvote Evergreen.V12.Question.QuestionId
    | CreateQuestion Time.Posix String.Nonempty.NonemptyString
    | TogglePin Evergreen.V12.Question.QuestionId Time.Posix
    | DeleteQuestion Evergreen.V12.Question.QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded Evergreen.V12.Question.QuestionId
    | VoteRemoved Evergreen.V12.Question.QuestionId
    | NewQuestion Evergreen.V12.Question.QuestionId Time.Posix String.Nonempty.NonemptyString
    | QuestionPinned Evergreen.V12.Question.QuestionId (Maybe Time.Posix)
    | QuestionDeleted Evergreen.V12.Question.QuestionId


type alias InQnaSession_ = 
    { qnaSessionId : (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId)
    , networkModel : (Evergreen.V12.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg Evergreen.V12.QnaSession.QnaSession)
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V12.Network.ChangeId
    , closedHostBanner : Bool
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId)
    | CreatingQnaSession String.Nonempty.NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , remoteData : FrontendStatus
    , currentTime : (Maybe Time.Posix)
    }


type alias BackendModel =
    { qnaSessions : (AssocList.Dict (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId) Evergreen.V12.QnaSession.BackendQnaSession)
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote Evergreen.V12.Question.QuestionId
    | PressedCloseHostBanner
    | PressedTogglePin Evergreen.V12.Question.QuestionId
    | GotCurrentTime Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion Evergreen.V12.Question.QuestionId


type ToBackend
    = LocalMsgRequest (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId) Evergreen.V12.Network.ChangeId LocalQnaMsg
    | GetQnaSession (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId)
    | CreateQnaSession String.Nonempty.NonemptyString


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId) Evergreen.V12.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId) (Result () Evergreen.V12.QnaSession.QnaSession)
    | CreateQnaSessionResponse (Evergreen.V12.Id.CryptographicKey Evergreen.V12.Id.QnaSessionId)