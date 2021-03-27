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
    , networkModel : NetworkModel Change QnaSession
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
    , localChangeCounter = ChangeId 0
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
    { questions = Dict.map (\_ question -> backendToFrontendQuestion sessionId question) qnaSession.questions
    , name = qnaSession.name
    , userId = userId
    }


backendToFrontendQuestion : SessionId -> BackendQuestion -> Question
backendToFrontendQuestion sessionId backendQuestion =
    { creationTime = backendQuestion.creationTime
    , content = backendQuestion.content
    , isRead = backendQuestion.isRead
    , votes = Set.size backendQuestion.votes
    , isUpvoted = Set.member sessionId backendQuestion.votes
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


type Change
    = LocalChange ChangeId LocalQnaMsg
    | ServerChange (Maybe ChangeId) ServerQnaMsg


changeId : Change -> Maybe ChangeId
changeId change =
    case change of
        LocalChange changeId_ _ ->
            Just changeId_

        ServerChange (Just changeId_) _ ->
            Just changeId_

        ServerChange Nothing _ ->
            Nothing


type LocalQnaMsg
    = ToggleUpvote QuestionId
    | CreateQuestion NonemptyString
    | PinQuestion QuestionId


type ServerQnaMsg
    = ToggleUpvoteResponse QuestionId
    | CreateQuestionResponse QuestionId Time.Posix
    | PinQuestionResponse QuestionId
    | VotesChanged QuestionId Int
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


type ChangeId
    = ChangeId Int


incrementChangeId : ChangeId -> ChangeId
incrementChangeId (ChangeId changeId_) =
    changeId_ + 1 |> ChangeId


type alias Question =
    { creationTime : Time.Posix
    , content : NonemptyString
    , isRead : Bool
    , votes : Int
    , isUpvoted : Bool
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
    = ServerMsgResponse (CryptographicKey QnaSessionId) (Maybe ChangeId) ServerQnaMsg
    | GetQnaSessionResponse (CryptographicKey QnaSessionId) (Result () QnaSession)
    | CreateQnaSessionResponse (CryptographicKey QnaSessionId)
