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
    | Loading QnaSessionId
    | Creating NonemptyString
    | Failure ()
    | Success SuccessModel


type alias SuccessModel =
    { qnaSessionId : QnaSessionId
    , networkModel : NetworkModel QnaMsg QnaSession
    , question : String
    , pressedCreateQuestion : Bool
    }


type alias BackendModel =
    { message : String
    , qnaSessions : Dict QnaSessionId BackendQnaSession
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
    , name : String
    }


type QnaMsg
    = LocalMsg LocalQnaMsg
    | ServerMsg ServerQnaMsg


type LocalQnaMsg
    = ToggleUpvote QuestionId
    | CreateQuestion QnaSessionId NonemptyString


type ServerQnaMsg
    = VotesChanged QuestionId
    | NewQuestion QuestionId Time.Posix NonemptyString
    | CreateQuestionResponse QuestionId Time.Posix
    | QuestionReadToggled QuestionId


type Status
    = Host HostKey
    | Participant


type QnaSessionId
    = QnaSessionId String


type HostKey
    = HostKey String


type QuestionId
    = QuestionId Int


type alias Question =
    { creationTime : Time.Posix
    , content : String
    , isRead : Bool
    , votes : Int
    }


type alias BackendQuestion =
    { creationTime : Time.Posix
    , content : String
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
    = LocalMsgRequest QnaSessionId LocalQnaMsg
    | GetQnaSession QnaSessionId
    | CreateQnaSession NonemptyString


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = ServerMsgResponse QnaSessionId ServerQnaMsg
    | GetQnaSessionResponse QnaSessionId (Result () QnaSession)
    | CreateQnaSessionResponse QnaSessionId
