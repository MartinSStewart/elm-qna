module Evergreen.V21.Question exposing (..)

import AssocSet
import Effect.Lamdera
import Effect.Time
import Evergreen.V21.Id
import String.Nonempty


type QuestionId
    = QuestionId Evergreen.V21.Id.UserId Int


type alias Question =
    { creationTime : Effect.Time.Posix
    , content : String.Nonempty.NonemptyString
    , isPinned : Maybe Effect.Time.Posix
    , otherVotes : Int
    , isUpvoted : Bool
    }


type alias BackendQuestion =
    { creationTime : Effect.Time.Posix
    , content : String.Nonempty.NonemptyString
    , isPinned : Maybe Effect.Time.Posix
    , votes : AssocSet.Set Effect.Lamdera.SessionId
    }
