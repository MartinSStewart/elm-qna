module Types exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Network exposing (ChangeId, NetworkModel)
import Question exposing (BackendQuestion, Question)
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
    , networkModel : NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : ChangeId
    }


initSuccessModel : CryptographicKey QnaSessionId -> QnaSession -> SuccessModel
initSuccessModel qnaSessionId qnaSesssion =
    { qnaSessionId = qnaSessionId
    , networkModel = Network.init qnaSesssion
    , question = ""
    , pressedCreateQuestion = False
    , localChangeCounter = Network.initChangeId
    }


type alias BackendModel =
    { qnaSessions : Dict (CryptographicKey QnaSessionId) BackendQnaSession
    , keyCounter : Int
    }


type alias QnaSession =
    { questions : Dict QuestionId Question
    , name : NonemptyString
    , userId : UserId
    }


initQnaSession : NonemptyString -> QnaSession
initQnaSession name =
    { questions = Dict.empty
    , name = name
    , userId = UserId 0
    }


type alias BackendQnaSession =
    { questions : Dict QuestionId BackendQuestion
    , host : SessionId
    , creationTime : Time.Posix
    , name : NonemptyString
    , connections : Dict ClientId UserId
    , connectionCounter : Int
    }


getQuestionId : Dict QuestionId v -> UserId -> QuestionId
getQuestionId questions userId =
    Dict.filter
        (\(QuestionId userId_ _) _ -> userId_ == userId)
        questions
        |> Dict.size
        |> QuestionId userId


backendToFrontendQnaSession : SessionId -> UserId -> BackendQnaSession -> QnaSession
backendToFrontendQnaSession sessionId userId qnaSession =
    { questions = Dict.map (\_ question -> Question.backendToFrontend sessionId False question) qnaSession.questions
    , name = qnaSession.name
    , userId = userId
    }


initBackendQnaSession : SessionId -> Time.Posix -> NonemptyString -> BackendQnaSession
initBackendQnaSession hostSessionId creationTime name =
    { questions = Dict.empty
    , host = hostSessionId
    , creationTime = creationTime
    , name = name
    , connections = Dict.empty
    , connectionCounter = 0
    }


type LocalQnaMsg
    = ToggleUpvote QuestionId
    | CreateQuestion NonemptyString
    | PinQuestion QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse


type ServerQnaMsg
    = VoteAdded QuestionId
    | VoteRemoved QuestionId
    | NewQuestion QuestionId Time.Posix NonemptyString
    | QuestionPinned QuestionId


type Status
    = Host
    | Participant


type QnaSessionId
    = QnaSessionId Never


type CryptographicKey a
    = CryptographicKey String


type UserId
    = UserId Int


type QuestionId
    = QuestionId UserId Int


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggledUpvote QuestionId


type ToBackend
    = LocalMsgRequest (CryptographicKey QnaSessionId) ChangeId LocalQnaMsg
    | GetQnaSession (CryptographicKey QnaSessionId)
    | CreateQnaSession NonemptyString


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime SessionId ClientId ToBackend Time.Posix
    | UserDisconnected SessionId ClientId


type ToFrontend
    = ServerMsgResponse (CryptographicKey QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (CryptographicKey QnaSessionId) ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse (CryptographicKey QnaSessionId) (Result () QnaSession)
    | CreateQnaSessionResponse (CryptographicKey QnaSessionId)
