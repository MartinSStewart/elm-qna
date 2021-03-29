module Evergreen.V12.QnaSession exposing (..)

import AssocList
import Evergreen.V12.Id
import Evergreen.V12.Question
import Lamdera
import String.Nonempty
import Time


type alias QnaSession = 
    { questions : (AssocList.Dict Evergreen.V12.Question.QuestionId Evergreen.V12.Question.Question)
    , name : String.Nonempty.NonemptyString
    , userId : Evergreen.V12.Id.UserId
    , isHost : Bool
    }


type alias BackendQnaSession = 
    { questions : (AssocList.Dict Evergreen.V12.Question.QuestionId Evergreen.V12.Question.BackendQuestion)
    , host : Lamdera.SessionId
    , creationTime : Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : (AssocList.Dict Lamdera.ClientId Evergreen.V12.Id.UserId)
    , connectionCounter : Int
    }