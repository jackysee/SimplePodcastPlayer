port module Shortcut exposing (keyMap, selectNext, selectPrev, scrollToIndex)

import Msgs exposing (..)
import Models exposing (..)
import Feed exposing (markListenedMsg)
import ListUtil exposing (getNext, findFirst)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


shortcuts : List ( List String, Model -> Msg )
shortcuts =
    [ [ "g", "u" ]
        => \_ -> ItemList <| SetItemFilter Unlistened
    , [ "g", "q" ]
        => \_ -> ItemList <| SetListView Queued
    , [ "g", "f" ]
        => \model ->
            getSelectedItem model
                |> Maybe.map
                    (\item_ ->
                        ItemList <| SetListView <| ViewFeed item_.feedUrl
                    )
                |> Maybe.withDefault NoOp
    , [ "g", "a" ]
        => \_ -> ItemList HideFeed
    , [ "j" ]
        => \_ -> UpdateItem SelectNext
    , [ "down" ]
        => \_ -> UpdateItem SelectNext
    , [ "k" ]
        => \_ -> UpdateItem SelectPrev
    , [ "up" ]
        => \_ -> UpdateItem SelectPrev
    , [ "o" ]
        => \model ->
            getSelectedItem model
                |> Maybe.map
                    (\item_ ->
                        item_.link
                            |> Maybe.map (\link -> OpenNewLink link)
                            |> Maybe.withDefault NoOp
                    )
                |> Maybe.withDefault NoOp
    , [ "p" ]
        => \model ->
            model.view.currentItem
                |> Maybe.map
                    (\currentItem ->
                        getItemByUrl model currentItem
                            |> Maybe.map
                                (\( feed, item ) ->
                                    case model.view.playerState of
                                        Playing ->
                                            Player <| Pause item

                                        Paused ->
                                            Player <| Play item

                                        Stopped ->
                                            Player <| Play item

                                        _ ->
                                            NoOp
                                )
                            |> Maybe.withDefault NoOp
                    )
                |> Maybe.withDefault NoOp
    , [ "enter" ]
        => \model ->
            getSelectedItem model
                |> Maybe.map (\item -> Player <| Play item)
                |> Maybe.withDefault NoOp
    , [ "n" ]
        => \_ -> AddFeed ShowAddPanel
    , [ "esc" ]
        => \_ -> AddFeed HideAddPanel
    , [ "u" ]
        => \model ->
            if model.view.listView == Queued then
                getSelectedItem model
                    |> Maybe.map (\item -> UpdateItem <| MoveQueuedItemUp item)
                    |> Maybe.withDefault NoOp
            else
                NoOp
    , [ "d" ]
        => \model ->
            if model.view.listView == Queued then
                getSelectedItem model
                    |> Maybe.map (\item -> UpdateItem <| MoveQueuedItemDown item)
                    |> Maybe.withDefault NoOp
            else
                NoOp
    , [ "q" ]
        => \model ->
            getSelectedItem model
                |> Maybe.map
                    (\item ->
                        if inPlayList item model then
                            UpdateItem <| Dequeue item
                        else
                            UpdateItem <| Enqueue item
                    )
                |> Maybe.withDefault NoOp
    , [ "m" ]
        => \model ->
            getSelectedItem model
                |> Maybe.map (\item -> markListenedMsg item)
                |> Maybe.withDefault NoOp
    , [ "shift-<" ]
        => \_ -> SetFloatPanel (About Settings)
    , [ "shift-?" ]
        => \_ -> SetFloatPanel (About Shortcut)
    , [ "r", "r" ]
        => \model ->
            case model.view.listView of
                ViewFeed url ->
                    model.feeds
                        |> findFirst (\feed -> feed.url == url)
                        |> Maybe.map (\feed -> UpdateFeed (UpdateFeeds [] feed))
                        |> Maybe.withDefault NoOp

                _ ->
                    UpdateFeed UpdateAllFeed
    , [ "shift-a" ]
        => \model -> UpdateItem MarkAllItemsAsListened
    ]


keyMap : Model -> String -> Msg
keyMap model key =
    let
        keys =
            model.view.shortcutKeys ++ [ key ]
    in
        getActions model shortcuts keys


getActions : Model -> List ( List String, Model -> Msg ) -> List String -> Msg
getActions model list keys =
    case list of
        [] ->
            SetShortcutKeys []

        ( shortcut, createMsg ) :: xs ->
            if listStartsWith keys [] shortcut then
                if shortcut /= keys then
                    SetShortcutKeys keys
                else
                    MsgBatch
                        [ createMsg model
                        , SetShortcutKeys []
                        ]
            else
                getActions model xs keys


listStartsWith : List String -> List String -> List String -> Bool
listStartsWith toMatch list1 list2 =
    if list1 == toMatch then
        True
    else
        case list2 of
            [] ->
                False

            x :: xs ->
                listStartsWith toMatch (list1 ++ [ x ]) xs


selectNext : Model -> Maybe ( Model, Cmd Msg )
selectNext model =
    let
        ( list, more ) =
            model.view.items

        next =
            model.view.itemSelected + 1

        endIndex =
            List.length list - 1
    in
        if next == endIndex && more then
            Nothing
        else if next > endIndex then
            Just ( model, Cmd.none )
        else
            Just (selectItem model next)


selectPrev : Model -> ( Model, Cmd Msg )
selectPrev model =
    let
        ( list, more ) =
            model.view.items

        prev =
            model.view.itemSelected - 1
    in
        if prev < 0 then
            ( model, Cmd.none )
        else
            selectItem model prev


selectItem : Model -> Int -> ( Model, Cmd Msg )
selectItem model index =
    let
        view =
            model.view
    in
        ( { model | view = { view | itemSelected = index } }
        , scrollToElement ("item-" ++ toString index)
        )


scrollToIndex : Int -> Cmd Msg
scrollToIndex index =
    scrollToElement ("item-" ++ toString index)


port scrollToElement : String -> Cmd msg
