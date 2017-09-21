module FloatPlanel exposing (updateFloatPanel, hideItemDropdown)

import Models exposing (..)
import Msgs exposing (..)
import Return exposing (Return)
import Storage exposing (saveView)


updateFloatPanel : FloatPanelMsg -> Model -> Return Msg Model
updateFloatPanel msg model =
    case msg of
        ShowItemDropdown url ->
            model
                |> updateView (\v -> { v | floatPanel = ItemDropdown url })
                |> Return.singleton

        HideItemDropdown ->
            model
                |> updateView (\v -> { v | floatPanel = hideItemDropdown model.view.floatPanel })
                |> Return.singleton

        SetFloatPanel panel ->
            model
                |> updateView (\v -> { v | floatPanel = panel, urlToAdd = "" })
                |> Return.singleton
                |> Return.effect_ saveView


hideItemDropdown : FloatPanel -> FloatPanel
hideItemDropdown source =
    case source of
        ItemDropdown url ->
            Hidden

        _ ->
            source
