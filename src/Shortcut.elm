port module Shortcut exposing (keyMap, selectNext, selectPrev)

import Msgs exposing (..)
import Models exposing (..)
import Feed exposing (markListenedMsg)
import ListUtil exposing (getNext, findFirst)

keyMap: Model -> String -> Msg
keyMap model key =
    let
        a = Debug.log "key" key
        selectedItem = getSelectedItem model
        gotoMsgs =
            if model.shortcutGoTo then
                let
                    msg =
                        case key of
                            "u" ->
                                [ SetItemFilter Unlistened ]
                            "q" ->
                                [ SetItemFilter Queued ]
                            "f" ->
                                model.itemSelected
                                    |> Maybe.map (\itemUrl ->
                                        getItemByUrl model itemUrl
                                            |> Maybe.map (\(feed, item) ->
                                                [ ShowFeed feed.url ]
                                            )
                                            |> Maybe.withDefault []
                                    )
                                    |> Maybe.withDefault []

                            "a" ->
                                [ HideFeed ]

                            _ ->
                                []
                in
                    msg ++ [ ToggleShortcutGoto False ]
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
                    selectedItem
                        |> Maybe.map (\item ->
                            item.link
                                |> Maybe.map (\link -> [ OpenNewLink link ])
                                |> Maybe.withDefault [ NoOp ]
                        )
                        |> Maybe.withDefault [ NoOp ]

                "p" ->
                    model.currentItemUrl
                        |> Maybe.map (\url ->
                            getItemByUrl model url
                                |> Maybe.map (\(feed, item) ->
                                    case model.playerState of
                                        Playing ->
                                            [ Pause item ]

                                        Paused ->
                                            [ Play item ]

                                        Stopped ->
                                            [ Play item ]

                                        _ ->
                                            [ NoOp ]
                                )
                                |> Maybe.withDefault [ NoOp ]
                        )
                        |> Maybe.withDefault [ NoOp ]

                "right" ->
                    selectedItem
                        |> Maybe.map (\item -> [ Play item ] )
                        |> Maybe.withDefault [ NoOp ]

                "n" ->
                    [ ShowAddPanel ]

                "esc" ->
                    [ HideAddPanel ]

                "u" ->
                    if model.itemFilter == Queued then
                        selectedItem
                            |> Maybe.map (\item -> [ MoveQueuedItemUp item.url ])
                            |> Maybe.withDefault [ NoOp ]
                    else
                        [ NoOp ]

                "d" ->
                    if model.itemFilter == Queued then
                        selectedItem
                            |> Maybe.map (\item -> [ MoveQueuedItemDown item.url ])
                            |> Maybe.withDefault [ NoOp ]
                    else
                        [ NoOp ]

                "q" ->
                    selectedItem
                        |> Maybe.map
                            (\item ->
                                if List.member item.url model.playList then
                                    [ Dequeue item.url ]
                                else
                                    [ Enqueue item.url ]
                            )
                        |> Maybe.withDefault [ NoOp ]

                "g" ->
                    if model.shortcutGoTo then
                        [ ToggleShortcutGoto False ]
                    else
                        [ ToggleShortcutGoto True ]

                "m" ->
                    selectedItem
                        |> Maybe.map (\item -> [ markListenedMsg item ])
                        |> Maybe.withDefault [ NoOp ]

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
