module Evergreen.V3.Question exposing (..)

import Lamdera
import Set
import String.Nonempty
import Time


type alias Question = 
    { creationTime : Time.Posix
    , content : String.Nonempty.NonemptyString
    , isPinned : (Maybe Time.Posix)
    , otherVotes : Int
    , isUpvoted : Bool
    }


type alias BackendQuestion = 
    { creationTime : Time.Posix
    , content : String.Nonempty.NonemptyString
    , isPinned : (Maybe Time.Posix)
    , votes : (Set.Set Lamdera.SessionId)
    }