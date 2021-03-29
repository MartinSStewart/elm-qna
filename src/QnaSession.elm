module QnaSession exposing (..)

import AssocList as Dict exposing (Dict)
import Id exposing (UserId(..))
import Lamdera exposing (ClientId, SessionId)
import Question exposing (BackendQuestion, Question, QuestionId)
import String.Nonempty exposing (NonemptyString)
import Time


type alias QnaSession =
    { questions : Dict QuestionId Question
    , name : NonemptyString
    , userId : UserId
    , isHost : Bool
    }


type alias BackendQnaSession =
    { questions : Dict QuestionId BackendQuestion
    , host : SessionId
    , creationTime : Time.Posix
    , name : NonemptyString
    , connections : Dict ClientId UserId
    , connectionCounter : Int
    }


init : NonemptyString -> Bool -> QnaSession
init name isHost =
    { questions = Dict.empty
    , name = name
    , userId = UserId 0
    , isHost = isHost
    }


initBackend : SessionId -> ClientId -> Time.Posix -> NonemptyString -> BackendQnaSession
initBackend hostSessionId hostClientId creationTime name =
    { questions = Dict.empty
    , host = hostSessionId
    , creationTime = creationTime
    , name = name
    , connections = Dict.singleton hostClientId (UserId 0)
    , connectionCounter = 1
    }


lastActivity : BackendQnaSession -> Time.Posix
lastActivity qnaSession =
    List.maximum
        (Time.posixToMillis qnaSession.creationTime
            :: List.map (.creationTime >> Time.posixToMillis) (Dict.values qnaSession.questions)
        )
        |> Maybe.map Time.millisToPosix
        |> Maybe.withDefault qnaSession.creationTime


backendToFrontend : SessionId -> UserId -> BackendQnaSession -> QnaSession
backendToFrontend sessionId userId qnaSession =
    { questions = Dict.map (\_ question -> Question.backendToFrontend sessionId question) qnaSession.questions
    , name = qnaSession.name
    , userId = userId
    , isHost = qnaSession.host == sessionId
    }
