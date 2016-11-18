module FloatPlanel exposing (hideItemDropdown)

import Models exposing (..)


hideItemDropdown : FloatPanel -> FloatPanel
hideItemDropdown source =
    case source of
        ItemDropdown url ->
            Hidden

        _ ->
            source
