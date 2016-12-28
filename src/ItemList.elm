port module ItemList
    exposing
        ( updateItemList
        , updateUpdateItem
        )

import Models exposing (..)
import Msgs exposing (..)
import Return exposing (Return)
import Storage exposing (saveItems, saveView)
import FloatPlanel exposing (hideItemDropdown)
import Dict
import ListUtil exposing (dropWhile, swapUp, swapDown)
import Feed exposing (markItemsListened)
import Shortcut exposing (selectNext, selectPrev, scrollToIndex)


updateItemList : ItemListMsg -> Model -> Return Msg Model
updateItemList msg model =
    case msg of
        SetListView listView ->
            { model | items = flushPlayCount model.items }
                |> updateView
                    (\v ->
                        { v
                            | listView = listView
                            , floatPanel = Hidden
                            , itemsToShow = defaultModel.view.itemsToShow
                            , itemSelected = -1
                        }
                    )
                |> Return.singleton
                |> Return.map updateViewItems
                |> Return.effect_ saveView
                |> Return.effect_ (\model -> saveItems model.items)

        HideFeed ->
            { model | items = flushPlayCount model.items }
                |> updateView (\v -> { v | listView = AllFeed })
                |> Return.singleton
                |> Return.map updateViewItems
                |> Return.effect_ saveView
                |> Return.effect_ (\model -> saveItems model.items)

        SetItemFilter filter ->
            { model | items = flushPlayCount model.items }
                |> updateView
                    (\v ->
                        { v
                            | itemFilter = filter
                            , itemsToShow = defaultModel.view.itemsToShow
                            , itemSelected = -1
                        }
                    )
                |> Return.singleton
                |> Return.map updateViewItems
                |> Return.effect_ saveView
                |> Return.effect_ (.items >> saveItems)

        SetItemSortLatest flag ->
            updateView (\v -> { v | itemSortLatest = flag }) model
                |> Return.singleton
                |> Return.map updateViewItems
                |> Return.effect_ saveView

        ShowMoreItem ->
            Return.singleton model
                |> Return.map (updateView (\v -> { v | itemsToShow = v.itemsToShow + defaultModel.view.itemsToShow }))
                |> Return.map updateViewItems


flushPlayCount : List Item -> List Item
flushPlayCount list =
    List.map
        (\item ->
            if item.markPlayCount /= -1 then
                { item
                    | playCount = item.markPlayCount
                    , markPlayCount = -1
                }
            else
                item
        )
        list


updateUpdateItem : ItemMsg -> Model -> Return Msg Model
updateUpdateItem msg model =
    case msg of
        MarkPlayCount item playCount ->
            model
                |> updateItem
                    (\item -> { item | markPlayCount = playCount })
                    (Just ( item.url, item.feedUrl ))
                |> updateView
                    (\v -> { v | floatPanel = hideItemDropdown v.floatPanel })
                |> Return.singleton
                |> Return.map updateViewItems
                |> Return.effect_ saveView
                |> Return.command (saveItems [ item ])

        MarkItemsBelowListened url ->
            let
                toUpdate =
                    Dict.fromList
                        (itemListAll False model
                            |> Tuple.first
                            |> dropWhile (\( feed, item ) -> item.url /= url)
                            |> List.map (\( feed, item ) -> ( item.url, True ))
                        )
            in
                { model | items = markItemsListened toUpdate model.items }
                    |> updateView
                        (\v ->
                            { v | floatPanel = hideItemDropdown model.view.floatPanel }
                        )
                    |> Return.singleton
                    |> Return.map updateViewItems
                    |> Return.effect_ saveView
                    |> Return.effect_
                        (\m ->
                            m.items
                                |> List.filter (\item -> Dict.member item.url toUpdate)
                                |> saveItems
                        )

        MarkAllItemsAsListened ->
            let
                toUpdate =
                    Dict.fromList
                        (itemListAll False model
                            |> Tuple.first
                            |> List.map (\( feed, item ) -> ( item.url, True ))
                        )
            in
                { model | items = markItemsListened toUpdate model.items }
                    |> Return.singleton
                    |> Return.map updateViewItems
                    |> Return.effect_
                        (\m ->
                            m.items
                                |> List.filter (\item -> Dict.member item.url toUpdate)
                                |> saveItems
                        )

        SelectItem index ->
            model
                |> updateView (\v -> { v | itemSelected = index })
                |> Return.singleton

        SelectNext ->
            case selectNext model of
                Just ( model_, cmd ) ->
                    Return.return model_ cmd

                Nothing ->
                    Return.singleton model
                        |> Return.andThen (updateItemList ShowMoreItem)
                        |> Return.andThen (updateUpdateItem SelectNext)

        SelectPrev ->
            selectPrev model

        Enqueue item ->
            model
                |> updateView
                    (\v ->
                        { v
                            | playList =
                                if inPlayList item model then
                                    model.view.playList
                                else
                                    model.view.playList ++ [ ( item.url, item.feedUrl ) ]
                            , floatPanel = hideItemDropdown model.view.floatPanel
                        }
                    )
                |> Return.singleton
                |> Return.map updateViewItems
                |> Return.effect_ saveView

        Dequeue item ->
            let
                isDequeued =
                    (\item_ -> isItemEqual (Just item_) item)
            in
                model
                    |> updateView
                        (\v ->
                            { v
                                | playList = List.filter (not << isDequeued) model.view.playList
                                , floatPanel = hideItemDropdown model.view.floatPanel
                                , itemSelected =
                                    if model.view.listView == Queued then
                                        max 0 (model.view.itemSelected - 1)
                                    else
                                        model.view.itemSelected
                            }
                        )
                    |> Return.singleton
                    |> Return.map updateViewItems
                    |> Return.effect_ saveView

        MoveQueuedItemUp item ->
            model
                |> updateView
                    (\v ->
                        { v
                            | playList = swapUp ( item.url, item.feedUrl ) model.view.playList
                            , itemSelected = max 0 (model.view.itemSelected - 1)
                        }
                    )
                |> Return.singleton
                |> Return.map updateViewItems
                |> Return.effect_ saveView
                |> Return.effect_ (\model -> scrollToIndex model.view.itemSelected)

        MoveQueuedItemDown item ->
            model
                |> updateView
                    (\v ->
                        { v
                            | playList = swapDown ( item.url, item.feedUrl ) model.view.playList
                            , itemSelected = min (model.view.itemSelected + 1) (List.length model.view.playList - 1)
                        }
                    )
                |> Return.singleton
                |> Return.map updateViewItems
                |> Return.effect_ saveView
                |> Return.effect_ (\model -> scrollToIndex model.view.itemSelected)

        OpenNewLink url ->
            ( model, openNewLink url )


port openNewLink : String -> Cmd msg
