module Id exposing (CryptographicKey(..), HostSecret, QnaSessionId, UserId(..), crytographicKeyToString, getShortCryptographicKey)

import Env
import Sha256


type QnaSessionId
    = QnaSessionId Never


type HostSecret
    = HostSecret Never


type CryptographicKey a
    = CryptographicKey String


type UserId
    = UserId Int


getShortCryptographicKey : { a | keyCounter : Int } -> ( { a | keyCounter : Int }, CryptographicKey b )
getShortCryptographicKey model =
    ( { model | keyCounter = model.keyCounter + 1 }
    , Env.secretKey ++ String.fromInt model.keyCounter |> Sha256.sha224 |> String.left 8 |> CryptographicKey
    )


crytographicKeyToString : CryptographicKey a -> String
crytographicKeyToString (CryptographicKey key) =
    key
