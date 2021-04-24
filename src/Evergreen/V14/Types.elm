module Evergreen.V14.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Evergreen.V14.Id
import Evergreen.V14.Network
import Evergreen.V14.QnaSession
import Evergreen.V14.Question
import Lamdera
import String.Nonempty
import Time
import Url


type LocalQnaMsg
    = ToggleUpvote Evergreen.V14.Question.QuestionId
    | CreateQuestion Time.Posix String.Nonempty.NonemptyString
    | TogglePin Evergreen.V14.Question.QuestionId Time.Posix
    | DeleteQuestion Evergreen.V14.Question.QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded Evergreen.V14.Question.QuestionId
    | VoteRemoved Evergreen.V14.Question.QuestionId
    | NewQuestion Evergreen.V14.Question.QuestionId Time.Posix String.Nonempty.NonemptyString
    | QuestionPinned Evergreen.V14.Question.QuestionId (Maybe Time.Posix)
    | QuestionDeleted Evergreen.V14.Question.QuestionId


type alias InQnaSession_ =
    { qnaSessionId : Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId
    , networkModel : Evergreen.V14.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg Evergreen.V14.QnaSession.QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V14.Network.ChangeId
    , copiedHostUrl : Maybe Time.Posix
    , copiedUrl : Maybe Time.Posix
    , isHost : Maybe (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.HostSecret)
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId)
    | LoadingQnaSessionWithHostInvite (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.HostSecret)
    | CreatingQnaSession String.Nonempty.NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , remoteData : FrontendStatus
    , currentTime : Maybe Time.Posix
    }


type alias BackendModel =
    { qnaSessions : AssocList.Dict (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId) Evergreen.V14.QnaSession.BackendQnaSession
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote Evergreen.V14.Question.QuestionId
    | PressedTogglePin Evergreen.V14.Question.QuestionId
    | GotCurrentTime Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion Evergreen.V14.Question.QuestionId
    | PressedCopyHostUrl
    | PressedCopyUrl


type ToBackend
    = LocalMsgRequest (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId) Evergreen.V14.Network.ChangeId LocalQnaMsg
    | GetQnaSession (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId)
    | GetQnaSessionWithHostInvite (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.HostSecret)
    | CreateQnaSession String.Nonempty.NonemptyString


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId) Evergreen.V14.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId)
        (Result
            ()
            { isHost : Maybe (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.HostSecret)
            , qnaSession : Evergreen.V14.QnaSession.QnaSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.HostSecret) (Result () ( Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId, Evergreen.V14.QnaSession.QnaSession ))
    | CreateQnaSessionResponse (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.QnaSessionId) (Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.HostSecret)
