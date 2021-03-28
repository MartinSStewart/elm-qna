module Types exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Id exposing (CryptographicKey, QnaSessionId, UserId(..))
import Lamdera exposing (ClientId, SessionId)
import Network exposing (ChangeId, NetworkModel)
import Question exposing (BackendQuestion, Question, QuestionId(..))
import String.Nonempty exposing (NonemptyString)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , remoteData : FrontendStatus
    , currentTime : Maybe Time.Posix
    }


type FrontendStatus
    = Homepage
    | LoadingQnaSession (CryptographicKey QnaSessionId)
    | CreatingQnaSession NonemptyString
    | LoadingQnaSessionFailed ()
    | InQnaSession InQnaSession_


type alias InQnaSession_ =
    { qnaSessionId : CryptographicKey QnaSessionId
    , networkModel : NetworkModel LocalQnaMsg ConfirmLocalQnaMsg ServerQnaMsg QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    , localChangeCounter : ChangeId
    , closedHostBanner : Bool
    }


initInQnaSession : CryptographicKey QnaSessionId -> QnaSession -> InQnaSession_
initInQnaSession qnaSessionId qnaSesssion =
    { qnaSessionId = qnaSessionId
    , networkModel = Network.init qnaSesssion
    , question = ""
    , pressedCreateQuestion = False
    , localChangeCounter = Network.initChangeId
    , closedHostBanner = False
    }


type alias BackendModel =
    { qnaSessions : Dict (CryptographicKey QnaSessionId) BackendQnaSession
    , keyCounter : Int
    }


type alias QnaSession =
    { questions : Dict QuestionId Question
    , name : NonemptyString
    , userId : UserId
    , isHost : Bool
    }


lastActivity : BackendQnaSession -> Time.Posix
lastActivity qnaSession =
    List.maximum
        (Time.posixToMillis qnaSession.creationTime
            :: List.map (.creationTime >> Time.posixToMillis) (Dict.values qnaSession.questions)
        )
        |> Maybe.map Time.millisToPosix
        |> Maybe.withDefault qnaSession.creationTime


initQnaSession : NonemptyString -> Bool -> QnaSession
initQnaSession name isHost =
    { questions = Dict.empty
    , name = name
    , userId = UserId 0
    , isHost = isHost
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
    { questions = Dict.map (\_ question -> Question.backendToFrontend sessionId question) qnaSession.questions
    , name = qnaSession.name
    , userId = userId
    , isHost = qnaSession.host == sessionId
    }


initBackendQnaSession : SessionId -> ClientId -> Time.Posix -> NonemptyString -> BackendQnaSession
initBackendQnaSession hostSessionId hostClientId creationTime name =
    { questions = Dict.empty
    , host = hostSessionId
    , creationTime = creationTime
    , name = name
    , connections = Dict.singleton hostClientId (UserId 0)
    , connectionCounter = 1
    }


type LocalQnaMsg
    = ToggleUpvote QuestionId
    | CreateQuestion NonemptyString
    | TogglePin QuestionId Time.Posix
    | DeleteQuestion QuestionId


type ConfirmLocalQnaMsg
    = ToggleUpvoteResponse
    | CreateQuestionResponse Time.Posix
    | PinQuestionResponse Time.Posix
    | DeleteQuestionResponse


type ServerQnaMsg
    = VoteAdded QuestionId
    | VoteRemoved QuestionId
    | NewQuestion QuestionId Time.Posix NonemptyString
    | QuestionPinned QuestionId (Maybe Time.Posix)
    | QuestionDeleted QuestionId


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PressedCreateQnaSession
    | TypedQuestion String
    | PressedCreateQuestion
    | PressedToggleUpvote QuestionId
    | PressedCloseHostBanner
    | PressedTogglePin QuestionId
    | GotCurrentTime Time.Posix
    | PressedDownloadQuestions
    | PressedDeleteQuestion QuestionId


type ToBackend
    = LocalMsgRequest (CryptographicKey QnaSessionId) ChangeId LocalQnaMsg
    | GetQnaSession (CryptographicKey QnaSessionId)
    | CreateQnaSession NonemptyString


type BackendMsg
    = NoOpBackendMsg
    | ToBackendWithTime SessionId ClientId ToBackend Time.Posix
    | UserDisconnected SessionId ClientId
    | CheckSessions Time.Posix


type ToFrontend
    = ServerMsgResponse (CryptographicKey QnaSessionId) ServerQnaMsg
    | LocalConfirmQnaMsgResponse (CryptographicKey QnaSessionId) ChangeId ConfirmLocalQnaMsg
    | GetQnaSessionResponse (CryptographicKey QnaSessionId) (Result () QnaSession)
    | CreateQnaSessionResponse (CryptographicKey QnaSessionId)
