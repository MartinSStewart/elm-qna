module Types exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Network exposing (NetworkModel)
import Set exposing (Set)
import String.Nonempty exposing (NonemptyString)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , remoteData : RemoteData
    }


type RemoteData
    = NotAsked
    | Loading (CryptographicKey QnaSessionId)
    | Creating NonemptyString
    | Failure ()
    | Success SuccessModel


type alias SuccessModel =
    { qnaSessionId : CryptographicKey QnaSessionId
    , networkModel : NetworkModel QnaMsg QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    }


initSuccessModel : CryptographicKey QnaSessionId -> QnaSession -> SuccessModel
initSuccessModel qnaSessionId qnaSesssion =
    { qnaSessionId = qnaSessionId
    , networkModel = Network.init qnaSesssion
    , question = ""
    , pressedCreateQuestion = False
    }


type alias BackendModel =
    { qnaSessions : Dict (CryptographicKey QnaSessionId) BackendQnaSession
    , keyCounter : Int
    }


type alias QnaSession =
    { questions : Dict QuestionId Question
    , name : NonemptyString
    }


initQnaSession : NonemptyString -> QnaSession
initQnaSession name =
    { questions = Dict.empty
    , name = name
    }


type alias BackendQnaSession =
    { questions : Dict QuestionId BackendQuestion
    , host : SessionId
    , creationTime : Time.Posix
    , name : NonemptyString
    , connections : Set ClientId
    , hostKey : CryptographicKey HostKey
    }


backendToFrontendQnaSession : BackendQnaSession -> QnaSession
backendToFrontendQnaSession qnaSession =
    { questions = Dict.map (\_ question -> backendToFrontendQuestion question) qnaSession.questions
    , name = qnaSession.name
    }


backendToFrontendQuestion : BackendQuestion -> Question
backendToFrontendQuestion backendQuestion =
    { creationTime = backendQuestion.creationTime
    , content = backendQuestion.content
    , isRead = backendQuestion.isRead
    , votes = Set.size backendQuestion.votes
    }


initBackendQnaSession : CryptographicKey HostKey -> SessionId -> Time.Posix -> NonemptyString -> BackendQnaSession
initBackendQnaSession hostKey hostSessionId creationTime name =
    { questions = Dict.empty
    , host = hostSessionId
    , creationTime = creationTime
    , name = name
    , connections = Set.empty
    , hostKey = hostKey
    }


type QnaMsg
    = LocalMsg LocalQnaMsg
    | ServerMsg ServerQnaMsg


type LocalQnaMsg
    = ToggleUpvote QuestionId
    | CreateQuestion NonemptyString
    | PinQuestion (CryptographicKey HostKey) QuestionId


type ServerQnaMsg
    = ToggleUpvoteResponse QuestionId
    | CreateQuestionResponse QuestionId Time.Posix
    | PinQuestionResponse QuestionId
    | VotesChanged QuestionId Int
    | NewQuestion QuestionId Time.Posix NonemptyString
    | QuestionPinned QuestionId


type Status
    = Host HostKey
    | Participant


type QnaSessionId
    = QnaSessionId Never


type HostKey
    = HostKey Never


type CryptographicKey a
    = CryptographicKey String


type QuestionId
    = QuestionId Int


type alias Question =
    { creationTime : Time.Posix
    , content : NonemptyString
    , isRead : Bool
    , votes : Int
    }


type alias BackendQuestion =
    { creationTime : Time.Posix
    , content : NonemptyString
    , isRead : Bool
    , votes : Set SessionId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggledUpvote


type ToBackend
    = LocalMsgRequest (CryptographicKey QnaSessionId) LocalQnaMsg
    | GetQnaSession (CryptographicKey QnaSessionId)
    | CreateQnaSession NonemptyString


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime SessionId ClientId ToBackend Time.Posix
    | UserDisconnected SessionId ClientId


type ToFrontend
    = ServerMsgResponse (CryptographicKey QnaSessionId) ServerQnaMsg
    | GetQnaSessionResponse (CryptographicKey QnaSessionId) (Result () QnaSession)
    | CreateQnaSessionResponse (CryptographicKey QnaSessionId)
