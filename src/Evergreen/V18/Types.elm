module Evergreen.V18.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Evergreen.V18.Id
import Evergreen.V18.Network
import Evergreen.V18.QnaSession
import Evergreen.V18.Question
import Lamdera
import String.Nonempty
import Time
import Url


type Key
    = FakeKey
    | ActualKey Browser.Navigation.Key


type LocalQnaMsg
    = ToggleUpvote Evergreen.V18.Question.QuestionId
    | CreateQuestion Time.Posix String.Nonempty.NonemptyString
    | TogglePin Evergreen.V18.Question.QuestionId Time.Posix
    | DeleteQuestion Evergreen.V18.Question.QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded Evergreen.V18.Question.QuestionId
    | VoteRemoved Evergreen.V18.Question.QuestionId
    | NewQuestion Evergreen.V18.Question.QuestionId Time.Posix String.Nonempty.NonemptyString
    | QuestionPinned Evergreen.V18.Question.QuestionId (Maybe Time.Posix)
    | QuestionDeleted Evergreen.V18.Question.QuestionId


type alias InQnaSession_ =
    { qnaSessionId : Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId
    , networkModel : Evergreen.V18.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg Evergreen.V18.QnaSession.QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V18.Network.ChangeId
    , copiedHostUrl : Maybe Time.Posix
    , copiedUrl : Maybe Time.Posix
    , isHost : Maybe (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.HostSecret)
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId)
    | LoadingQnaSessionWithHostInvite (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.HostSecret)
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
    { qnaSessions : AssocList.Dict (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId) Evergreen.V18.QnaSession.BackendQnaSession
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote Evergreen.V18.Question.QuestionId
    | PressedTogglePin Evergreen.V18.Question.QuestionId
    | GotCurrentTime Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion Evergreen.V18.Question.QuestionId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Time.Posix


type ToBackend
    = LocalMsgRequest (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId) Evergreen.V18.Network.ChangeId LocalQnaMsg
    | GetQnaSession (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId)
    | GetQnaSessionWithHostInvite (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.HostSecret)
    | CreateQnaSession String.Nonempty.NonemptyString
    | CheckIfConnectedRequest


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | UserConnected Lamdera.SessionId Lamdera.ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId) Evergreen.V18.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId)
        (Result
            ()
            { isHost : Maybe (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.HostSecret)
            , qnaSession : Evergreen.V18.QnaSession.QnaSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.HostSecret) (Result () ( Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId, Evergreen.V18.QnaSession.QnaSession ))
    | CreateQnaSessionResponse (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.QnaSessionId) (Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.HostSecret)
    | CheckIfConnectedResponse
    | NewConnection
