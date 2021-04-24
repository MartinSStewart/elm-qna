module Evergreen.V15.QnaSession exposing (..)

import AssocList
import Evergreen.V15.Id
import Evergreen.V15.Question
import Lamdera
import Set
import String.Nonempty
import Time


type alias QnaSession =
    { questions : AssocList.Dict Evergreen.V15.Question.QuestionId Evergreen.V15.Question.Question
    , name : String.Nonempty.NonemptyString
    , userId : Evergreen.V15.Id.UserId
    }


type alias BackendQnaSession =
    { questions : AssocList.Dict Evergreen.V15.Question.QuestionId Evergreen.V15.Question.BackendQuestion
    , host : Set.Set Lamdera.SessionId
    , hostSecret : Evergreen.V15.Id.CryptographicKey Evergreen.V15.Id.HostSecret
    , creationTime : Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : AssocList.Dict Lamdera.ClientId Evergreen.V15.Id.UserId
    , connectionCounter : Int
    }
