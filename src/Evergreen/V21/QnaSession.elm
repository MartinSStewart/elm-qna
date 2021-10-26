module Evergreen.V21.QnaSession exposing (..)

import AssocList
import AssocSet
import Effect.Lamdera
import Effect.Time
import Evergreen.V21.Id
import Evergreen.V21.Question
import String.Nonempty


type alias QnaSession =
    { questions : AssocList.Dict Evergreen.V21.Question.QuestionId Evergreen.V21.Question.Question
    , name : String.Nonempty.NonemptyString
    , userId : Evergreen.V21.Id.UserId
    , closingTime : Maybe Effect.Time.Posix
    }


type alias BackendQnaSession =
    { questions : AssocList.Dict Evergreen.V21.Question.QuestionId Evergreen.V21.Question.BackendQuestion
    , host : AssocSet.Set Effect.Lamdera.SessionId
    , hostSecret : Evergreen.V21.Id.CryptographicKey Evergreen.V21.Id.HostSecret
    , creationTime : Effect.Time.Posix
    , name : String.Nonempty.NonemptyString
    , connections : AssocSet.Set Effect.Lamdera.ClientId
    , userIds : AssocList.Dict Effect.Lamdera.SessionId Evergreen.V21.Id.UserId
    , connectionCounter : Int
    , closingTime : Maybe Effect.Time.Posix
    }
