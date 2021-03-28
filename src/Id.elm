module Id exposing (..)

import Env
import Sha256


type QnaSessionId
    = QnaSessionId Never


type CryptographicKey a
    = CryptographicKey String


type UserId
    = UserId Int


getShortCryptographicKey : { a | keyCounter : Int } -> ( { a | keyCounter : Int }, CryptographicKey b )
getShortCryptographicKey model =
    ( { model | keyCounter = model.keyCounter + 1 }
    , Env.secretKey ++ String.fromInt model.keyCounter |> Sha256.sha224 |> String.left 8 |> CryptographicKey
    )
