module Shortcut exposing (keyMap, selectNext, selectPrev)

import Msgs exposing (..)
import Models exposing (..)
import ListUtil exposing (dropWhile, takeWhile)

keyMap: Model -> String -> Msg
keyMap model key =
    let
        a = Debug.log "key" key
    in
        case key of
            "j" ->
                SelectNext
            "k" ->
                SelectPrev

            "space" ->
                case getSelectedItem model of
                    Just item ->
                        case model.playerState of
                            Playing ->
                                Pause item

                            Paused ->
                                Play item

                            Stopped ->
                                Play item

                            _ ->
                                NoOp

                    Nothing ->
                        NoOp

            _ ->
                NoOp


selectNext : Model -> Model
selectNext model =
    let
        (list, more) = itemList model
    in
        case model.itemSelected of
            Just url ->
                list
                    |> dropWhile (\(feed, item) -> item.url /= Just url)
                    |> List.map snd
                    |> List.take 2
                    |> List.reverse
                    |> List.head
                    |> selectItem model

            Nothing ->
                list
                    |> List.map snd
                    |> List.head
                    |> selectItem model


selectPrev: Model -> (Model, Cmd Msg)
selectPrev model =
    let
        (list, more) = itemList model
    in
        case model.itemSelected of
            Just url ->
                let
                    item = list
                            |> takeWhile (\(feed, item) -> item.url /= Just url)
                            |> List.map snd
                            |> List.reverse
                            |> List.head
                in
                    (selectItem model item, Cmd.none)

            Nothing ->
                let
                    item = list
                            |> List.map snd
                            |> List.reverse
                            |> List.head
                in
                    (selectItem model item, Cmd.none)


selectItem:  Model -> Maybe Item -> Model
selectItem model item =
    case item of
        Just item' ->
            { model | itemSelected = item'.url }

        Nothing ->
            model
