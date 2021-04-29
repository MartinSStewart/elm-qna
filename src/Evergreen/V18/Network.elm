module Evergreen.V18.Network exposing (..)


type ChangeId
    = ChangeId Int


type NetworkModel local confirmLocal server model
    = NetworkModel
        { localMsgs : List ( ChangeId, local )
        , serverState : model
        }
