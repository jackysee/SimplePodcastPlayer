module FloatPlanel exposing (hideItemDropdown, initAddPanel)

import Models exposing (..)

hideItemDropdown: FloatPanel -> FloatPanel
hideItemDropdown source =
    case source of
        ItemDropdown url ->
            Hidden
        _ ->
            source

initAddPanel : List feed -> FloatPanel
initAddPanel feeds =
    if List.length feeds  == 0 then
        AddPanel
    else
        Hidden
