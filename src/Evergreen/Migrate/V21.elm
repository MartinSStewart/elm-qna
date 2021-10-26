module Evergreen.Migrate.V21 exposing (..)

import AssocList as Dict
import AssocSet as Set
import Effect.Lamdera
import Effect.Time as Time
import Evergreen.V20.Id
import Evergreen.V20.QnaSession
import Evergreen.V20.Question
import Evergreen.V20.Types as Old
import Evergreen.V21.Id
import Evergreen.V21.QnaSession
import Evergreen.V21.Question
import Evergreen.V21.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated ( migrateBackendModel old, Cmd.none )


migrateBackendModel : Old.BackendModel -> New.BackendModel
migrateBackendModel old =
    { qnaSessions = migrateDict migrateCryptographicKey migrateBackendQnaSession old.qnaSessions
    , keyCounter = old.keyCounter
    }


migrateCryptographicKey : Evergreen.V20.Id.CryptographicKey a -> Evergreen.V21.Id.CryptographicKey b
migrateCryptographicKey (Evergreen.V20.Id.CryptographicKey old) =
    Evergreen.V21.Id.CryptographicKey old


migrateBackendQnaSession : Evergreen.V20.QnaSession.BackendQnaSession -> Evergreen.V21.QnaSession.BackendQnaSession
migrateBackendQnaSession old =
    { questions = migrateDict migrateQuestionId migrateBackendQuestion old.questions
    , host = Set.map migrateSessionId old.host
    , hostSecret = migrateCryptographicKey old.hostSecret
    , creationTime = old.creationTime
    , name = old.name
    , connections = Set.empty
    , userIds = migrateDict migrateSessionId migrateUserId old.userIds
    , connectionCounter = old.connectionCounter
    , closingTime = Just (Time.millisToPosix 1635328800000)
    }


migrateBackendQuestion : Evergreen.V20.Question.BackendQuestion -> Evergreen.V21.Question.BackendQuestion
migrateBackendQuestion old =
    { creationTime = old.creationTime
    , content = old.content
    , isPinned = old.isPinned
    , votes = Set.map migrateSessionId old.votes
    }


migrateQuestionId (Evergreen.V20.Question.QuestionId id index) =
    Evergreen.V21.Question.QuestionId (migrateUserId id) index


migrateSessionId (Evergreen.V20.Id.SessionId old) =
    Effect.Lamdera.sessionIdFromString old


migrateUserId (Evergreen.V20.Id.UserId old) =
    Evergreen.V21.Id.UserId old


migrateDict : (a -> k) -> (b -> v) -> Dict.Dict a b -> Dict.Dict k v
migrateDict migrateKey migrateValue dict =
    Dict.toList dict
        |> List.map (Tuple.mapBoth migrateKey migrateValue)
        |> Dict.fromList


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged
