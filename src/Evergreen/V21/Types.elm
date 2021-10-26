module Evergreen.V21.Types exposing (..)

import AssocList
import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V21.Id
import Evergreen.V21.Network
import Evergreen.V21.QnaSession
import Evergreen.V21.Question
import String.Nonempty
import Url


type Route
    = HomepageRoute
    | HostInviteRoute (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.HostSecret)
    | QnaSessionRoute (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId)


type alias FrontendLoading =
    { time : Maybe Effect.Time.Posix
    , timezone : Maybe Effect.Time.Zone
    , key : Effect.Browser.Navigation.Key
    , route : Maybe Route
    }


type LocalQnaMsg
    = ToggleUpvote Evergreen.V21.Question.QuestionId
    | CreateQuestion Effect.Time.Posix String.Nonempty.NonemptyString
    | TogglePin Evergreen.V21.Question.QuestionId Effect.Time.Posix
    | DeleteQuestion Evergreen.V21.Question.QuestionId
    | ChangeClosingTime Effect.Time.Posix


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Effect.Time.Posix
    | PinQuestionResponse Effect.Time.Posix
    | DeleteQuestionResponse
    | ChangeClosingTimeResponse


type ServerQnaMsg
    = VoteAdded Evergreen.V21.Question.QuestionId
    | VoteRemoved Evergreen.V21.Question.QuestionId
    | NewQuestion Evergreen.V21.Question.QuestionId Effect.Time.Posix String.Nonempty.NonemptyString
    | QuestionPinned Evergreen.V21.Question.QuestionId (Maybe Effect.Time.Posix)
    | QuestionDeleted Evergreen.V21.Question.QuestionId
    | ClosingTimeChanged Effect.Time.Posix


type alias HostState =
    { secret : Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.HostSecret
    , closingDateText : String
    , closingTimeText : String
    , showSettings : Bool
    }


type alias InQnaSession_ =
    { qnaSessionId : Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId
    , networkModel : Evergreen.V21.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg Evergreen.V21.QnaSession.QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V21.Network.ChangeId
    , copiedHostUrl : Maybe Effect.Time.Posix
    , copiedUrl : Maybe Effect.Time.Posix
    , isHost : Maybe HostState
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId)
    | LoadingQnaSessionWithHostInvite (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.HostSecret)
    | CreatingQnaSession String.Nonempty.NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , remoteData : FrontendStatus
    , time : Effect.Time.Posix
    , timezone : Effect.Time.Zone
    , lastConnectionCheck : Maybe Effect.Time.Posix
    , gotFirstConnectMsg : Bool
    }


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias BackendModel =
    { qnaSessions : AssocList.Dict (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId) Evergreen.V21.QnaSession.BackendQnaSession
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote Evergreen.V21.Question.QuestionId
    | PressedTogglePin Evergreen.V21.Question.QuestionId
    | GotCurrentTime Effect.Time.Posix
    | GotTimezone Effect.Time.Zone
    | PressedDownloadQuestions
    | PressedDeleteQuestion Evergreen.V21.Question.QuestionId
    | PressedCopyHostUrl
    | PressedCopyUrl
    | CheckIfConnected Effect.Time.Posix
    | TextInputBlurred
    | TypedClosingDate String
    | TypedClosingTime String
    | PressedToggleShowSettings


type ToBackend
    = LocalMsgRequest (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId) Evergreen.V21.Network.ChangeId LocalQnaMsg
    | GetQnaSession (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId)
    | GetQnaSessionWithHostInvite (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.HostSecret)
    | CreateQnaSession String.Nonempty.NonemptyString
    | CheckIfConnectedRequest


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix
    | UserDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | UserConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | CheckSessions Effect.Time.Posix


type ToFrontend
    = ServerMsgResponse (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId) Evergreen.V21.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse
        (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId)
        (Result
            ()
            { isHost : Maybe (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.HostSecret)
            , qnaSession : Evergreen.V21.QnaSession.QnaSession
            }
        )
    | GetQnaSessionWithHostInviteResponse (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.HostSecret) (Result () ( Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId, Evergreen.V21.QnaSession.QnaSession ))
    | CreateQnaSessionResponse (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.QnaSessionId) (Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.HostSecret)
    | CheckIfConnectedResponse
    | NewConnection
