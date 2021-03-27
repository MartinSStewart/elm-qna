module Evergreen.V1.Question exposing (..)

import Lamdera
import Set
import String.Nonempty
import Time


type alias Question = 
    { creationTime : Time.Posix
    , content : String.Nonempty.NonemptyString
    , isRead : Bool
    , otherVotes : Int
    , isUpvoted : Bool
    , isNewQuestion : Bool
    }


type alias BackendQuestion = 
    { creationTime : Time.Posix
    , content : String.Nonempty.NonemptyString
    , isRead : Bool
    , votes : (Set.Set Lamdera.SessionId)
    }