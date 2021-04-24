module Evergreen.V15.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Evergreen.V15.Id
import Evergreen.V15.Network
import Evergreen.V15.QnaSession
import Evergreen.V15.Question
import Lamdera
import String.Nonempty
import Time
import Url


type LocalQnaMsg
    = ToggleUpvote Evergreen.V15.Question.QuestionId
    | CreateQuestion Time.Posix String.Nonempty.NonemptyString
    | TogglePin Evergreen.V15.Question.QuestionId Time.Posix
    | DeleteQuestion Evergreen.V15.Question.QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded Evergreen.V15.Question.QuestionId
    | VoteRemoved Evergreen.V15.Question.QuestionId
    | NewQuestion Evergreen.V15.Question.QuestionId Time.Posix String.Nonempty.NonemptyString
    | QuestionPinned Evergreen.V15.Question.QuestionId (Maybe Time.Posix)
    | QuestionDeleted Evergreen.V15.Question.QuestionId


type alias InQnaSession_ =
    { qnaSessionId : Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId
    , networkModel : Evergreen.V15.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg Evergreen.V15.QnaSession.QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V15.Network.ChangeId
    , copiedHostUrl : Maybe Time.Posix
    , copiedUrl : Maybe Time.Posix
    , isHost : Maybe (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.HostSecret)
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId)
    | LoadingQnaSessionWithHostInvite (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.HostSecret)
    | CreatingQnaSession String.Nonempty.NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , remoteData : FrontendStatus
    , currentTime : Maybe Time.Posix
    , lastConnectionCheck : Maybe Time.Posix
    }


type alias BackendModel =
    { qnaSessions : AssocList.Dict (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId) Evergreen.V15.QnaSession.BackendQnaSession
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote Evergreen.V15.Question.QuestionId
    | PressedTogglePin Evergreen.V15.Question.QuestionId
    | GotCurrentTime Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion Evergreen.V15.Question.QuestionId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Time.Posix


type ToBackend
    = LocalMsgRequest (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId) Evergreen.V15.Network.ChangeId LocalQnaMsg
    | GetQnaSession (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId)
    | GetQnaSessionWithHostInvite (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.HostSecret)
    | CreateQnaSession String.Nonempty.NonemptyString
    | CheckIfConnectedRequest


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId) Evergreen.V15.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId)
        (Result
            ()
            { isHost : Maybe (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.HostSecret)
            , qnaSession : Evergreen.V15.QnaSession.QnaSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.HostSecret) (Result () ( Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId, Evergreen.V15.QnaSession.QnaSession ))
    | CreateQnaSessionResponse (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.QnaSessionId) (Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.HostSecret)
    | CheckIfConnectedResponse
