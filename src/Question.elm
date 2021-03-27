module Question exposing (..)

import Lamdera exposing (SessionId)
import Set exposing (Set)
import String.Nonempty exposing (NonemptyString)
import Time


type alias Question =
    { creationTime : Time.Posix
    , content : NonemptyString
    , isRead : Bool
    , otherVotes : Int
    , isUpvoted : Bool
    , isNewQuestion : Bool
    }


type alias BackendQuestion =
    { creationTime : Time.Posix
    , content : NonemptyString
    , isRead : Bool
    , votes : Set SessionId
    }


votes : Question -> Int
votes question =
    if question.isUpvoted then
        question.otherVotes + 1

    else
        question.otherVotes


backendToFrontend : SessionId -> Bool -> BackendQuestion -> Question
backendToFrontend sessionId isNewQuestion backendQuestion =
    { creationTime = backendQuestion.creationTime
    , content = backendQuestion.content
    , isRead = backendQuestion.isRead
    , otherVotes = Set.remove sessionId backendQuestion.votes |> Set.size
    , isUpvoted = Set.member sessionId backendQuestion.votes
    , isNewQuestion = isNewQuestion
    }
