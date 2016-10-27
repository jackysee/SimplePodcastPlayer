port module Shortcut exposing (keyMap, selectNext, selectPrev)

import Msgs exposing (..)
import Models exposing (..)
import ListUtil exposing (dropWhile, takeWhile)
-- import Maybe.Extra exposing (join)

keyMap: Model -> String -> Msg
keyMap model key =
    let
        a = Debug.log "key" key
        selectedItem = getSelectedItem model
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
                case selectedItem of
                    Just item ->
                        case item.link of
                            Just link' ->
                                OpenNewLink link'

                            Nothing ->
                                NoOp

                    Nothing ->
                        NoOp

            "space" ->
                case selectedItem of
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

            "u" ->
                if model.itemFilter == Queued then
                    case selectedItem of
                        Just item ->
                            MoveQueuedItemUp item.url

                        Nothing  ->
                            NoOp
                else
                    NoOp

            "d" ->
                if model.itemFilter == Queued then
                    case selectedItem of
                        Just item ->
                            MoveQueuedItemDown item.url

                        _ ->
                            NoOp
                else
                    NoOp

            "e" ->
                case selectedItem of
                    Just item ->
                        Enqueue item.url
                    _ ->
                        NoOp

            {--
            "d" ->
                case selectedItem of
                    Just item ->
                        Dequeue item.url
                    _ ->
                        NoOp
            --}

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
                    |> getNext (\(index, (feed, item)) -> item.url == url)
                    |> Maybe.map (\(index, (feed, item)) -> (index, item))
                    |> selectItem model

            Nothing ->
                list
                    |> List.indexedMap (,)
                    |> List.map (\(index, (feed, item)) -> (index, item))
                    |> List.head
                    |> selectItem model


getNext: (a -> Bool) -> List a -> Maybe a
getNext predicate list =
    case list of
        x::y::xs ->
            if predicate x then
                Just y
            else
                getNext predicate (y::xs)

        _ ->
            Nothing


selectPrev: Model -> (Model, Cmd Msg)
selectPrev model =
    let
        (list, more) = itemList model
    in
        case model.itemSelected of
            Just url ->
                list
                    |> List.indexedMap (,)
                    |> List.reverse
                    |> getNext (\(index, (feed, item)) -> item.url == url)
                    |> Maybe.map (\(index, (feed, item)) -> (index, item))
                    |> selectItem model

            Nothing ->
                list
                    |> List.indexedMap (,)
                    |> List.map (\(index, (feed, item)) -> (index, item))
                    |> List.reverse
                    |> List.head
                    |> selectItem model
            

selectItem:  Model -> Maybe (Int, Item) -> (Model, Cmd Msg)
selectItem model item =
    case item of
        Just (index, item') ->
            ({ model | itemSelected = Just item'.url }, scrollToElement ("item-" ++ toString index))

        Nothing ->
            (model, Cmd.none)


port scrollToElement: String -> Cmd msg
