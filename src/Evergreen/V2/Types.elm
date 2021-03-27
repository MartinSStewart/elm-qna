module Evergreen.V2.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Evergreen.V2.Network
import Evergreen.V2.Question
import Lamdera
import String.Nonempty
import Time
import Url


type QnaSessionId
    = QnaSessionId Never


type CryptographicKey a
    = CryptographicKey String


type UserId
    = UserId Int


type QuestionId
    = QuestionId UserId Int


type LocalQnaMsg
    = ToggleUpvote QuestionId
    | CreateQuestion String.Nonempty.NonemptyString
    | TogglePin QuestionId Time.Posix


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix


type ServerQnaMsg
    = VoteAdded QuestionId
    | VoteRemoved QuestionId
    | NewQuestion QuestionId Time.Posix String.Nonempty.NonemptyString
    | QuestionPinned QuestionId (Maybe Time.Posix)


type alias QnaSession = 
    { questions : (AssocList.Dict QuestionId Evergreen.V2.Question.Question)
    , name : String.Nonempty.NonemptyString
    , userId : UserId
    , isHost : Bool
    }


type alias SuccessModel = 
    { qnaSessionId : (CryptographicKey QnaSessionId)
    , networkModel : (Evergreen.V2.Network.NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg QnaSession)
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : Evergreen.V2.Network.ChangeId
    , closedHostBanner : Bool
    }


type RemoteData
    = NotAsked
    | Loading (CryptographicKey QnaSessionId)
    | Creating String.Nonempty.NonemptyString
    | Failure ()
    | Success SuccessModel


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , remoteData : RemoteData
    }


type alias BackendQnaSession = 
    { questions : (AssocList.Dict QuestionId Evergreen.V2.Question.BackendQuestion)
    , host : Lamdera.SessionId
    , creationTime : Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : (AssocList.Dict Lamdera.ClientId UserId)
    , connectionCounter : Int
    }


type alias BackendModel =
    { qnaSessions : (AssocList.Dict (CryptographicKey QnaSessionId) BackendQnaSession)
    , keyCounter : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote QuestionId
    | PressedCloseHostBanner
    | PressedTogglePin QuestionId


type ToBackend
    = LocalMsgRequest (CryptographicKey QnaSessionId) Evergreen.V2.Network.ChangeId LocalQnaMsg
    | GetQnaSession (CryptographicKey QnaSessionId)
    | CreateQnaSession String.Nonempty.NonemptyString


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime Lamdera.SessionId Lamdera.ClientId ToBackend Time.Posix
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = ServerMsgResponse (CryptographicKey QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (CryptographicKey QnaSessionId) Evergreen.V2.Network.ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse (CryptographicKey QnaSessionId) (Result () QnaSession)
    | CreateQnaSessionResponse (CryptographicKey QnaSessionId)