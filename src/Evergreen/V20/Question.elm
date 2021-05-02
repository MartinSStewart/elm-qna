module Evergreen.V20.Question exposing (..)

import AssocSet
import Evergreen.V20.Id
import String.Nonempty
import Time


type QuestionId
    = QuestionId Evergreen.V20.Id.UserId Int


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
    , votes : AssocSet.Set Evergreen.V20.Id.SessionId
    }
