port module Shortcut exposing (keyMap, selectNext, selectPrev)

import Msgs exposing (..)
import Models exposing (..)
import Feed exposing (markListenedMsg)
import ListUtil exposing (getNext, findFirst)

(=>): a -> b -> (a, b)
(=>) = (,)

shortcuts =
    [ ["g", "u"] => \_ -> SetItemFilter Unlistened
    , ["g", "q"] => \_ -> SetListView Queued
    , ["g", "f"] =>
        \model ->
            model.view.itemSelected
                |> Maybe.map (\itemUrl ->
                    getItemByUrl model itemUrl
                        |> Maybe.map (\(feed, item) ->
                            SetListView (ViewFeed feed.url)
                        )
                        |> Maybe.withDefault NoOp
                )
                |> Maybe.withDefault NoOp

    , ["g", "a"] => \_ -> HideFeed
    , ["j"] => \_ -> SelectNext
    , ["down"] => \_ -> SelectNext
    , ["k"] => \_ -> SelectPrev
    , ["up"] => \_ -> SelectPrev
    , ["o"] =>
        \model ->
           getSelectedItem model
                |> Maybe.map (\item_ ->
                    item_.link
                        |> Maybe.map (\link ->  OpenNewLink link)
                        |> Maybe.withDefault  NoOp
                )
                |> Maybe.withDefault NoOp

    , ["p"] =>
        \model ->
            model.view.currentItemUrl
                |> Maybe.map (\url ->
                    getItemByUrl model url
                        |> Maybe.map (\(feed, item) ->
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

    , ["enter"] =>
        \model ->
            getSelectedItem model
                |> Maybe.map (\item ->  Play item  )
                |> Maybe.withDefault  NoOp

    , ["n"] => \_ -> ShowAddPanel
    , ["esc"] => \_ -> HideAddPanel
    , ["u"] =>
        \model ->
            if model.view.listView == Queued then
                getSelectedItem model
                    |> Maybe.map (\item ->  MoveQueuedItemUp item.url )
                    |> Maybe.withDefault  NoOp
            else
                NoOp

    , ["d"] =>
        \model ->
            if model.view.listView == Queued then
                getSelectedItem model
                    |> Maybe.map (\item -> MoveQueuedItemDown item.url)
                    |> Maybe.withDefault NoOp
            else
                NoOp

    , ["q"] =>
        \model ->
            getSelectedItem model
                |> Maybe.map
                    (\item ->
                        if List.member item.url model.view.playList then
                            Dequeue item.url
                        else
                            Enqueue item.url
                    )
                |> Maybe.withDefault NoOp

    , ["m"] =>
        \model ->
            getSelectedItem model
                |> Maybe.map (\item -> markListenedMsg item)
                |> Maybe.withDefault NoOp

    , ["ctrl-,"] => \_ -> SetFloatPanel (About Settings)
    , ["shift-/"] => \_ -> SetFloatPanel (About Shortcut)
    , ["r", "r"] => \model ->
        case model.view.listView of
            ViewFeed url ->
                model.feeds
                    |> findFirst (\feed -> feed.url == url )
                    |> Maybe.map (\feed -> UpdateFeeds [] feed)
                    |> Maybe.withDefault NoOp
            _ ->
                UpdateAllFeed
    , ["shift-a"] => \model -> MarkAllItemsAsListened

    ]


keyMap: Model -> String -> Msg
keyMap model key =
    let
        a = Debug.log "key" key
        keys = model.view.shortcutKeys ++ [key]
    in
        getActions model shortcuts keys


getActions: Model -> List (List String, (Model -> Msg)) -> List String -> Msg
getActions model list keys =
    case list of
        [] ->
            SetShortcutKeys []

        (shortcut, createMsg)::xs ->
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

            x::xs ->
                listStartsWith toMatch (list1 ++ [x]) xs


selectNext : Model -> Maybe (Model, Cmd Msg)
selectNext model =
    let
        (list, more) = itemList model
        listHasUrl = List.any (\(feed, item) -> Just item.url == model.view.itemSelected) list
        url_ =
            if listHasUrl then
                model.view.itemSelected
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
        listHasUrl = List.any (\(feed, item) -> Just item.url == model.view.itemSelected) list
        url_ = if listHasUrl then model.view.itemSelected else Nothing
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
    let
        view = model.view
    in
        case item of
            Just (index, item_) ->
                ({ model | view = { view | itemSelected = Just item_.url }}
                , scrollToElement ("item-" ++ toString index)
                )

            Nothing ->
                (model, Cmd.none)


port scrollToElement: String -> Cmd msg
