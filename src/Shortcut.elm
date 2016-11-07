port module Shortcut exposing (keyMap, selectNext, selectPrev)

import Msgs exposing (..)
import Models exposing (..)
import Feed exposing (markListenedMsg)
import ListUtil exposing (getNext, findFirst)

(=>) = (,)

shortcuts =
    [ ["g", "u"] => \_ -> SetItemFilter Unlistened
    , ["g", "q"] => \_ -> SetItemFilter Queued
    , ["g", "f"] =>
        \model ->
            model.itemSelected
                |> Maybe.map (\itemUrl ->
                    getItemByUrl model itemUrl
                        |> Maybe.map (\(feed, item) ->
                            ShowFeed feed.url
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
            model.currentItemUrl
                |> Maybe.map (\url ->
                    getItemByUrl model url
                        |> Maybe.map (\(feed, item) ->
                            case model.playerState of
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

    , ["right"] =>
        \model ->
            getSelectedItem model
                |> Maybe.map (\item ->  Play item  )
                |> Maybe.withDefault  NoOp

    , ["n"] => \_ -> ShowAddPanel
    , ["esc"] => \_ -> HideAddPanel
    , ["u"] =>
        \model ->
            if model.itemFilter == Queued then
                getSelectedItem model
                    |> Maybe.map (\item ->  MoveQueuedItemUp item.url )
                    |> Maybe.withDefault  NoOp
            else
                NoOp

    , ["d"] =>
        \model ->
            if model.itemFilter == Queued then
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
                        if List.member item.url model.playList then
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

    , ["s"] => \_ -> SetFloatPanel (About Settings)
    , ["/"] => \_ -> SetFloatPanel (About Shortcut)
    ]


keyMap: Model -> String -> Msg
keyMap model key =
    let
        a = Debug.log "key" key
        keys = model.shortcutKeys ++ [key]
        getActions =
            (\list ->
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
                            getActions xs
            )
    in
        -- Debug.log "result" <|
        getActions shortcuts


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
