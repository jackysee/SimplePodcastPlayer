port module Shortcut exposing (keyMap, selectNext, selectPrev)

import Msgs exposing (..)
import Models exposing (..)
import ListUtil exposing (dropWhile, takeWhile)
-- import Maybe.Extra exposing (join)

keyMap: Model -> String -> Msg
keyMap model key =
    let
        a = Debug.log "key" key
    in
        case key of
            "j" ->
                SelectNext

            "down" ->
                SelectNext

            "k" ->
                SelectPrev
            
            "up" ->
                SelectPrev

            "o" ->
                case getSelectedItem model of
                    Just item ->
                        case item.link of
                            Just link' ->
                                OpenNewLink link'

                            Nothing ->
                                NoOp

                    Nothing ->
                        NoOp

            "space" ->
                case getSelectedItem model of
                    Just item ->
                        if Just item.url /= model.currentItemUrl then
                            Play item
                        else
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

            "n" ->
                ShowAddPanel

            "esc" ->
                HideAddPanel

            _ ->
                NoOp


selectNext : Model -> (Model, Cmd Msg)
selectNext model =
    let
        (list, more) = itemList model
    in
        case model.itemSelected of
            Just url ->
                list
                    |> List.indexedMap (,)
                    |> dropWhile (\(index, (feed, item)) -> item.url /= url)
                    |> List.map (\(index, (feed, item)) -> (index, item))
                    |> List.take 2
                    |> List.reverse
                    |> List.head
                    |> selectItem model

            Nothing ->
                list
                    |> List.indexedMap (,)
                    |> List.map (\(index, (feed, item)) -> (index, item))
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
                            |> List.indexedMap (,)
                            |> takeWhile (\(index, (feed, item)) -> item.url /= url)
                            |> List.map (\(index, (feed, item)) -> (index, item))
                            |> List.reverse
                            |> List.head
                in
                    selectItem model item

            Nothing ->
                let
                    item = list
                            |> List.indexedMap (,)
                            |> List.map (\(index, (feed, item)) -> (index, item))
                            |> List.reverse
                            |> List.head
                in
                    selectItem model item


selectItem:  Model -> Maybe (Int, Item) -> (Model, Cmd Msg)
selectItem model item =
    case item of
        Just (index, item') ->
            ({ model | itemSelected = Just item'.url }, scrollToElement ("item-" ++ toString index))

        Nothing ->
            (model, Cmd.none)


port scrollToElement: String -> Cmd msg
