module QnaSession exposing (..)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time as Time
import Id exposing (CryptographicKey, HostSecret, UserId(..))
import Question exposing (BackendQuestion, Question, QuestionId)
import String.Nonempty exposing (NonemptyString)


type alias QnaSession =
    { questions : Dict QuestionId Question
    , name : NonemptyString
    , userId : UserId
    , closingTime : Maybe Time.Posix
    }


type HostStatus
    = IsHostButLoading
    | IsHost (CryptographicKey HostSecret)
    | IsNotHost


type alias BackendQnaSession =
    { questions : Dict QuestionId BackendQuestion
    , host : Set SessionId
    , hostSecret : CryptographicKey HostSecret
    , creationTime : Time.Posix
    , name : NonemptyString
    , connections : Set ClientId
    , userIds : Dict SessionId UserId
    , connectionCounter : Int
    , closingTime : Maybe Time.Posix
    }


init : Maybe Time.Posix -> NonemptyString -> QnaSession
init closingTime name =
    { questions = Dict.empty
    , name = name
    , closingTime = closingTime
    , userId = UserId 0
    }


initBackend : SessionId -> ClientId -> CryptographicKey HostSecret -> Time.Posix -> NonemptyString -> BackendQnaSession
initBackend hostSessionId hostClientId hostSecret creationTime name =
    { questions = Dict.empty
    , host = Set.singleton hostSessionId
    , hostSecret = hostSecret
    , creationTime = creationTime
    , name = name
    , connections = Set.singleton hostClientId
    , userIds = Dict.singleton hostSessionId (UserId 0)
    , connectionCounter = 1
    , closingTime = Nothing
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
    , closingTime = qnaSession.closingTime
    }
