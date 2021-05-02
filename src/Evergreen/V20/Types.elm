module Evergreen.V20.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Evergreen.V20.Id
import Evergreen.V20.Network
import Evergreen.V20.QnaSession
import Evergreen.V20.Question
import String.Nonempty
import Time
import Url


type Key
    = FakeKey
    | ActualKey Browser.Navigation.Key


type LocalQnaMsg
    = ToggleUpvote Evergreen.V20.Question.QuestionId
    | CreateQuestion Time.Posix String.Nonempty.NonemptyString
    | TogglePin Evergreen.V20.Question.QuestionId Time.Posix
    | DeleteQuestion Evergreen.V20.Question.QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded Evergreen.V20.Question.QuestionId
    | VoteRemoved Evergreen.V20.Question.QuestionId
    | NewQuestion Evergreen.V20.Question.QuestionId Time.Posix String.Nonempty.NonemptyString
    | QuestionPinned Evergreen.V20.Question.QuestionId (Maybe Time.Posix)
    | QuestionDeleted Evergreen.V20.Question.QuestionId


type alias InQnaSession_ =
    { qnaSessionId : Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId
    , networkModel : Evergreen.V20.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg Evergreen.V20.QnaSession.QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V20.Network.ChangeId
    , copiedHostUrl : Maybe Time.Posix
    , copiedUrl : Maybe Time.Posix
    , isHost : Maybe (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.HostSecret)
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId)
    | LoadingQnaSessionWithHostInvite (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.HostSecret)
    | CreatingQnaSession String.Nonempty.NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias FrontendModel =
    { key : Key
    , remoteData : FrontendStatus
    , currentTime : Maybe Time.Posix
    , lastConnectionCheck : Maybe Time.Posix
    , gotFirstConnectMsg : Bool
    }


type alias BackendModel =
    { qnaSessions : AssocList.Dict (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId) Evergreen.V20.QnaSession.BackendQnaSession
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote Evergreen.V20.Question.QuestionId
    | PressedTogglePin Evergreen.V20.Question.QuestionId
    | GotCurrentTime Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion Evergreen.V20.Question.QuestionId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Time.Posix


type ToBackend
    = LocalMsgRequest (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId) Evergreen.V20.Network.ChangeId LocalQnaMsg
    | GetQnaSession (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId)
    | GetQnaSessionWithHostInvite (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.HostSecret)
    | CreateQnaSession String.Nonempty.NonemptyString
    | CheckIfConnectedRequest


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Evergreen.V20.Id.SessionId Evergreen.V20.Id.ClientId ToBackend Time.Posix
    | UserDisconnected Evergreen.V20.Id.SessionId Evergreen.V20.Id.ClientId
    | UserConnected Evergreen.V20.Id.SessionId Evergreen.V20.Id.ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId) Evergreen.V20.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId)
        (Result
            ()
            { isHost : Maybe (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.HostSecret)
            , qnaSession : Evergreen.V20.QnaSession.QnaSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.HostSecret) (Result () ( Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId, Evergreen.V20.QnaSession.QnaSession ))
    | CreateQnaSessionResponse (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.QnaSessionId) (Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.HostSecret)
    | CheckIfConnectedResponse
    | NewConnection
