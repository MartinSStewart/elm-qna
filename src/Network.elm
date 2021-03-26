module Network exposing (..)

import List.Extra


type alias NetworkModel msg model =
    { localMsgs : List msg, serverState : model }


init : model -> NetworkModel msg model
init model =
    { localMsgs = [], serverState = model }


updateFromUser : msg -> NetworkModel msg model -> NetworkModel msg model
updateFromUser msg localModel =
    { localMsgs = localModel.localMsgs ++ [ msg ]
    , serverState = localModel.serverState
    }


localState : (msg -> model -> model) -> NetworkModel msg model -> model
localState updateFunc localModel =
    List.foldl updateFunc localModel.serverState localModel.localMsgs


updateFromBackend : (msg -> model -> model) -> msg -> NetworkModel msg model -> NetworkModel msg model
updateFromBackend updateFunc msg localModel =
    { localMsgs = List.Extra.remove msg localModel.localMsgs
    , serverState = updateFunc msg localModel.serverState
    }
