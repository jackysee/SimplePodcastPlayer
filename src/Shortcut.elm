port module Shortcut exposing (keyMap, selectNext, selectPrev)

import Msgs exposing (..)
import Models exposing (..)
import Feed exposing (markListenedMsg)
import ListUtil exposing (getNext)

keyMap: Model -> String -> Msg
keyMap model key =
    let
        a = Debug.log "key" key
        selectedItem = getSelectedItem model
        gotoMsgs =
            if model.shortcutGoTo then
                case key of
                    "u" ->
                        [ SetItemFilter Unlistened
                        , ToggleShortcutGoto False
                        ]
                    "q" ->
                        [ SetItemFilter Queued
                        , ToggleShortcutGoto False
                        ]
                    _ ->
                        [ ToggleShortcutGoto False ]
            else
                []

        msgs =
            if List.length gotoMsgs == 0 then
                case key of
                "j" ->
                    [ SelectNext ]

                "down" ->
                    [ SelectNext ]

                "k" ->
                    [ SelectPrev ]

                "up" ->
                    [ SelectPrev ]

                "o" ->
                    case selectedItem of
                        Just item ->
                            case item.link of
                                Just link' ->
                                    [ OpenNewLink link' ]

                                Nothing ->
                                    [ NoOp ]

                        Nothing ->
                            [ NoOp ]

                "p" ->
                    case selectedItem of
                        Just item ->
                            if Just item.url /= model.currentItemUrl then
                                [ Play item ]
                            else
                                case model.playerState of
                                    Playing ->
                                        [ Pause item ]

                                    Paused ->
                                        [ Play item ]

                                    Stopped ->
                                        [ Play item ]

                                    _ ->
                                        [ NoOp ]

                        Nothing ->
                            [ NoOp ]

                "n" ->
                    [ ShowAddPanel ]

                "esc" ->
                    [ HideAddPanel ]

                "u" ->
                    if model.itemFilter == Queued then
                        case selectedItem of
                            Just item ->
                                [ MoveQueuedItemUp item.url ]

                            Nothing  ->
                                [ NoOp ]
                    else
                        [ NoOp ]

                "d" ->
                    if model.itemFilter == Queued then
                        case selectedItem of
                            Just item ->
                                [ MoveQueuedItemDown item.url ]

                            _ ->
                                [ NoOp ]
                    else
                        [ NoOp ]

                "q" ->
                    case selectedItem of
                        Just item ->
                            if List.member item.url model.playList then
                                [ Dequeue item.url ]
                            else
                                [ Enqueue item.url ]
                        _ ->
                            [ NoOp ]

                "g" ->
                    if model.shortcutGoTo then
                        [ ToggleShortcutGoto False ]
                    else
                        [ ToggleShortcutGoto True ]

                "m" ->
                    case selectedItem of
                        Just item ->
                            [ markListenedMsg item ]
                        _ ->
                            [ NoOp ]

                _ ->
                    [ NoOp ]
            else
                []
    in
        MsgBatch <| gotoMsgs ++ msgs


selectNext : Model -> Maybe (Model, Cmd Msg)
selectNext model =
    let
        (list, more) = itemList model
        listHasUrl = List.any (\(feed, item) -> Just item.url == model.itemSelected) list
        url_ = 
            if listHasUrl then 
                model.itemSelected 
            else 
                Nothing
        next =
            case url_ of
                Just url ->
                    list
                        |> List.indexedMap (,)
                        |> getNext (\(index, (feed, item)) -> item.url == url)
                        |> Maybe.map (\(index, (feed, item)) -> (index, item))

                Nothing ->
                    list
                        |> List.indexedMap (,)
                        |> List.map (\(index, (feed, item)) -> (index, item))
                        |> List.head
    in
        if next == Nothing && more then
            Nothing
        else
            Just (selectItem model next)


selectPrev: Model -> (Model, Cmd Msg)
selectPrev model =
    let
        (list, more) = itemList model
        listHasUrl = List.any (\(feed, item) -> Just item.url == model.itemSelected) list
        url_ = if listHasUrl then model.itemSelected else Nothing
    in
        case url_ of
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
