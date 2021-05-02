module Evergreen.V20.QnaSession exposing (..)

import AssocList
import AssocSet
import Evergreen.V20.Id
import Evergreen.V20.Question
import String.Nonempty
import Time


type alias QnaSession =
    { questions : AssocList.Dict Evergreen.V20.Question.QuestionId Evergreen.V20.Question.Question
    , name : String.Nonempty.NonemptyString
    , userId : Evergreen.V20.Id.UserId
    }


type alias BackendQnaSession =
    { questions : AssocList.Dict Evergreen.V20.Question.QuestionId Evergreen.V20.Question.BackendQuestion
    , host : AssocSet.Set Evergreen.V20.Id.SessionId
    , hostSecret : Evergreen.V20.Id.CryptographicKey Evergreen.V20.Id.HostSecret
    , creationTime : Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : AssocSet.Set Evergreen.V20.Id.ClientId
    , userIds : AssocList.Dict Evergreen.V20.Id.SessionId Evergreen.V20.Id.UserId
    , connectionCounter : Int
    }
