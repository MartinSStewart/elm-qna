module Question exposing (..)

import AssocSet as Set exposing (Set)
import Effect.Lamdera exposing (SessionId)
import Effect.Time
import Id exposing (UserId(..))
import String.Nonempty exposing (NonemptyString)


type alias Question =
    { creationTime : Effect.Time.Posix
    , content : NonemptyString
    , isPinned : Maybe Effect.Time.Posix
    , otherVotes : Int
    , isUpvoted : Bool
    }


type QuestionId
    = QuestionId UserId Int


type alias BackendQuestion =
    { creationTime : Effect.Time.Posix
    , content : NonemptyString
    , isPinned : Maybe Effect.Time.Posix
    , votes : Set SessionId
    }


votes : Question -> Int
votes question =
    if question.isUpvoted then
        question.otherVotes + 1

    else
        question.otherVotes


backendToFrontend : SessionId -> BackendQuestion -> Question
backendToFrontend sessionId backendQuestion =
    { creationTime = backendQuestion.creationTime
    , content = backendQuestion.content
    , isPinned = backendQuestion.isPinned
    , otherVotes = Set.remove sessionId backendQuestion.votes |> Set.size
    , isUpvoted = Set.member sessionId backendQuestion.votes
    }


isNewQuestion : Effect.Time.Posix -> Question -> Bool
isNewQuestion currentTime question =
    Effect.Time.posixToMillis question.creationTime + 1600 > Effect.Time.posixToMillis currentTime


isCreator : UserId -> QuestionId -> Bool
isCreator userId (QuestionId userId_ _) =
    userId == userId_


questionIdToString : QuestionId -> String
questionIdToString (QuestionId (UserId userId) questionIndex) =
    String.fromInt userId ++ " " ++ String.fromInt questionIndex
