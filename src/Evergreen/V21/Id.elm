module Evergreen.V21.Id exposing (..)


type HostSecret
    = HostSecret Never


type CryptographicKey a
    = CryptographicKey String


type QnaSessionId
    = QnaSessionId Never


type UserId
    = UserId Int
