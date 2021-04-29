module Evergreen.V18.Question exposing (..)

import Evergreen.V18.Id
import Lamdera
import Set
import String.Nonempty
import Time


type QuestionId
    = QuestionId Evergreen.V18.Id.UserId Int


type alias Question =
    { creationTime : Time.Posix
    , content : String.Nonempty.NonemptyString
    , isPinned : Maybe Time.Posix
    , otherVotes : Int
    , isUpvoted : Bool
    }


type alias BackendQuestion =
    { creationTime : Time.Posix
    , content : String.Nonempty.NonemptyString
    , isPinned : Maybe Time.Posix
    , votes : Set.Set Lamdera.SessionId
    }
