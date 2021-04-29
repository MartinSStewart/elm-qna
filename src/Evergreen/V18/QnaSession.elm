module Evergreen.V18.QnaSession exposing (..)

import AssocList
import Evergreen.V18.Id
import Evergreen.V18.Question
import Lamdera
import Set
import String.Nonempty
import Time


type alias QnaSession =
    { questions : AssocList.Dict Evergreen.V18.Question.QuestionId Evergreen.V18.Question.Question
    , name : String.Nonempty.NonemptyString
    , userId : Evergreen.V18.Id.UserId
    }


type alias BackendQnaSession =
    { questions : AssocList.Dict Evergreen.V18.Question.QuestionId Evergreen.V18.Question.BackendQuestion
    , host : Set.Set Lamdera.SessionId
    , hostSecret : Evergreen.V18.Id.CryptographicKey Evergreen.V18.Id.HostSecret
    , creationTime : Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : AssocList.Dict Lamdera.ClientId Evergreen.V18.Id.UserId
    , connectionCounter : Int
    }
