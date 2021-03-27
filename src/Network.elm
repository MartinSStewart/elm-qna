module Network exposing (..)


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


updateFromBackend : (msg -> msg -> Bool) -> (msg -> model -> model) -> msg -> NetworkModel msg model -> NetworkModel msg model
updateFromBackend msgsAreEqual updateFunc msg localModel =
    { localMsgs = List.filter (\localMsg -> msgsAreEqual localMsg msg |> not) localModel.localMsgs
    , serverState = updateFunc msg localModel.serverState
    }
