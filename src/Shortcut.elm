port module Shortcut exposing (keyMap, selectNext, selectPrev)

import Msgs exposing (..)
import Models exposing (..)
import Feed exposing (markListenedMsg)
import ListUtil exposing (getNext, findFirst)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


shortcuts : List ( List String, Model -> Msg )
shortcuts =
    [ [ "g", "u" ] => \_ -> SetItemFilter Unlistened
    , [ "g", "q" ] => \_ -> SetListView Queued
    , [ "g", "f" ]
        => \model ->
            model.view.itemSelected
                |> Maybe.map
                    (\( itemUrl, feedUrl ) ->
                        SetListView (ViewFeed feedUrl)
                    )
                |> Maybe.withDefault NoOp
    , [ "g", "a" ] => \_ -> HideFeed
    , [ "j" ] => \_ -> SelectNext
    , [ "down" ] => \_ -> SelectNext
    , [ "k" ] => \_ -> SelectPrev
    , [ "up" ] => \_ -> SelectPrev
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
                                            Pause item

                                        Paused ->
                                            Play item

                                        Stopped ->
                                            Play item

                                        _ ->
                                            NoOp
                                )
                            |> Maybe.withDefault NoOp
                    )
                |> Maybe.withDefault NoOp
    , [ "enter" ]
        => \model ->
            getSelectedItem model
                |> Maybe.map (\item -> Play item)
                |> Maybe.withDefault NoOp
    , [ "n" ] => \_ -> ShowAddPanel
    , [ "esc" ] => \_ -> HideAddPanel
    , [ "u" ]
        => \model ->
            if model.view.listView == Queued then
                getSelectedItem model
                    |> Maybe.map (\item -> MoveQueuedItemUp item)
                    |> Maybe.withDefault NoOp
            else
                NoOp
    , [ "d" ]
        => \model ->
            if model.view.listView == Queued then
                getSelectedItem model
                    |> Maybe.map (\item -> MoveQueuedItemDown item)
                    |> Maybe.withDefault NoOp
            else
                NoOp
    , [ "q" ]
        => \model ->
            getSelectedItem model
                |> Maybe.map
                    (\item ->
                        if inPlayList item model then
                            Dequeue item
                        else
                            Enqueue item
                    )
                |> Maybe.withDefault NoOp
    , [ "m" ]
        => \model ->
            getSelectedItem model
                |> Maybe.map (\item -> markListenedMsg item)
                |> Maybe.withDefault NoOp
    , [ "shift-<" ] => \_ -> SetFloatPanel (About Settings)
    , [ "shift-?" ] => \_ -> SetFloatPanel (About Shortcut)
    , [ "r", "r" ]
        => \model ->
            case model.view.listView of
                ViewFeed url ->
                    model.feeds
                        |> findFirst (\feed -> feed.url == url)
                        |> Maybe.map (\feed -> UpdateFeeds [] feed)
                        |> Maybe.withDefault NoOp

                _ ->
                    UpdateAllFeed
    , [ "shift-a" ] => \model -> MarkAllItemsAsListened
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
            itemList model

        listHasUrl =
            List.any (\( feed, item ) -> isItemEqual model.view.itemSelected item) list

        selected =
            if listHasUrl then
                model.view.itemSelected
            else
                Nothing

        next =
            case selected of
                Just selected_ ->
                    list
                        |> List.indexedMap (,)
                        |> getNext (\( index, ( feed, item ) ) -> isItemEqual (Just selected_) item)
                        |> Maybe.map (\( index, ( feed, item ) ) -> ( index, item ))

                Nothing ->
                    list
                        |> List.indexedMap (,)
                        |> List.map (\( index, ( feed, item ) ) -> ( index, item ))
                        |> List.head
    in
        if next == Nothing && more then
            Nothing
        else
            Just (selectItem model next)


selectPrev : Model -> ( Model, Cmd Msg )
selectPrev model =
    let
        ( list, more ) =
            itemList model

        listHasUrl =
            List.any (\( feed, item ) -> isItemEqual model.view.itemSelected item) list

        selected =
            if listHasUrl then
                model.view.itemSelected
            else
                Nothing
    in
        case selected of
            Just selected_ ->
                list
                    |> List.indexedMap (,)
                    |> List.reverse
                    |> getNext (\( index, ( feed, item ) ) -> isItemEqual (Just selected_) item)
                    |> Maybe.map (\( index, ( feed, item ) ) -> ( index, item ))
                    |> selectItem model

            Nothing ->
                list
                    |> List.indexedMap (,)
                    |> List.map (\( index, ( feed, item ) ) -> ( index, item ))
                    |> List.reverse
                    |> List.head
                    |> selectItem model


selectItem : Model -> Maybe ( Int, Item ) -> ( Model, Cmd Msg )
selectItem model item =
    let
        view =
            model.view
    in
        case item of
            Just ( index, item_ ) ->
                ( { model | view = { view | itemSelected = Just ( item_.url, item_.feedUrl ) } }
                , scrollToElement ("item-" ++ toString index)
                )

            Nothing ->
                ( model, Cmd.none )


port scrollToElement : String -> Cmd msg
