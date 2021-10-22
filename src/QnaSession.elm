module QnaSession exposing (..)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time
import Id exposing (CryptographicKey, HostSecret, UserId(..))
import Question exposing (BackendQuestion, Question, QuestionId)
import String.Nonempty exposing (NonemptyString)


type alias QnaSession =
    { questions : Dict QuestionId Question
    , name : NonemptyString
    , userId : UserId
    }


type HostStatus
    = IsHostButLoading
    | IsHost (CryptographicKey HostSecret)
    | IsNotHost


type alias BackendQnaSession =
    { questions : Dict QuestionId BackendQuestion
    , host : Set SessionId
    , hostSecret : CryptographicKey HostSecret
    , creationTime : Effect.Time.Posix
    , name : NonemptyString
    , connections : Set ClientId
    , userIds : Dict SessionId UserId
    , connectionCounter : Int
    }


init : NonemptyString -> QnaSession
init name =
    { questions = Dict.empty
    , name = name
    , userId = UserId 0
    }


initBackend : SessionId -> ClientId -> CryptographicKey HostSecret -> Effect.Time.Posix -> NonemptyString -> BackendQnaSession
initBackend hostSessionId hostClientId hostSecret creationTime name =
    { questions = Dict.empty
    , host = Set.singleton hostSessionId
    , hostSecret = hostSecret
    , creationTime = creationTime
    , name = name
    , connections = Set.singleton hostClientId
    , userIds = Dict.singleton hostSessionId (UserId 0)
    , connectionCounter = 1
    }


lastActivity : BackendQnaSession -> Effect.Time.Posix
lastActivity qnaSession =
    List.maximum
        (Effect.Time.posixToMillis qnaSession.creationTime
            :: List.map (.creationTime >> Effect.Time.posixToMillis) (Dict.values qnaSession.questions)
        )
        |> Maybe.map Effect.Time.millisToPosix
        |> Maybe.withDefault qnaSession.creationTime


backendToFrontend : SessionId -> UserId -> BackendQnaSession -> QnaSession
backendToFrontend sessionId userId qnaSession =
    { questions = Dict.map (\_ question -> Question.backendToFrontend sessionId question) qnaSession.questions
    , name = qnaSession.name
    , userId = userId
    }
