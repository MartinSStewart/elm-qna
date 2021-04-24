module Evergreen.V14.QnaSession exposing (..)

import AssocList
import Evergreen.V14.Id
import Evergreen.V14.Question
import Lamdera
import Set
import String.Nonempty
import Time


type alias QnaSession =
    { questions : AssocList.Dict Evergreen.V14.Question.QuestionId Evergreen.V14.Question.Question
    , name : String.Nonempty.NonemptyString
    , userId : Evergreen.V14.Id.UserId
    }


type alias BackendQnaSession =
    { questions : AssocList.Dict Evergreen.V14.Question.QuestionId Evergreen.V14.Question.BackendQuestion
    , host : Set.Set Lamdera.SessionId
    , hostSecret : Evergreen.V14.Id.CryptographicKey Evergreen.V14.Id.HostSecret
    , creationTime : Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : AssocList.Dict Lamdera.ClientId Evergreen.V14.Id.UserId
    , connectionCounter : Int
    }
