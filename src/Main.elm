port module Main exposing (..)

import Html as App
import Task exposing (Task)
import Time
import ListUtil exposing (dropWhile, swapDown, swapUp, getNext, getPrev)
import Dom
import Dict
import Json.Decode
import Models exposing (..)
import Msgs exposing (..)
import View exposing (view)
import Feed exposing (loadFeed, updateFeed, updateModelFeed, updateFeedItems, markItemsListened)
import Shortcut exposing (keyMap, selectNext, selectPrev)
import FloatPlanel exposing (hideItemDropdown)
import DecodeStoreModel exposing (decodeStoreValue)
import Return exposing (Return)


main : Program (Maybe Json.Decode.Value) Model Msg
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe Json.Decode.Value -> ( Model, Cmd Msg )
init storeValue =
    case decodeStoreValue storeValue of
        Just storeModel ->
            let
                model =
                    fromStoreModel storeModel
            in
                model
                    ! [ updateCurrentTime
                      , updateFeeds model.feeds
                      ]

        Nothing ->
            defaultModel
                ! [ saveSetting defaultModel
                  , saveView defaultModel
                  , updateCurrentTime
                  ]


updateCurrentTime : Cmd Msg
updateCurrentTime =
    Task.perform UpdateCurrentTime Time.now


updateFeeds : List Feed -> Cmd Msg
updateFeeds feeds =
    case feeds of
        [] ->
            Cmd.none

        feed :: feeds ->
            Task.attempt
                (\result ->
                    result
                        |> Result.map (UpdateFeeds feeds)
                        |> Result.withDefault NoOp
                )
                (Task.succeed feed)


update : Msg -> Model -> Return Msg Model
update msg model =
    (case msg of
        NoOp ->
            Return.singleton model

        UpdateCurrentTime time ->
            let
                view =
                    model.view
            in
                { model | view = { view | currentTime = time } }
                    |> Return.singleton

        SetUrl value ->
            model
                |> updateView
                    (\view ->
                        { view
                            | urlToAdd = value
                            , loadFeedState = Empty
                        }
                    )
                |> Return.singleton

        AddFeed ->
            if List.any (\feed -> feed.url == model.view.urlToAdd) model.feeds then
                model
                    |> updateView (\view -> { view | loadFeedState = AlreadyExist })
                    |> Return.singleton
            else
                model
                    |> updateView (\view -> { view | loadFeedState = Loading })
                    |> Return.singleton
                    |> Return.command
                        (loadFeed
                            model.setting.fallbackRssServiceUrl
                            model.view.urlToAdd
                        )

        ShowMoreItem ->
            let
                view =
                    model.view
            in
                { model
                    | view =
                        { view | itemsToShow = view.itemsToShow + defaultModel.view.itemsToShow }
                }
                    |> Return.singleton

        FetchFeedSucceed ( feed, items ) ->
            let
                view =
                    model.view
            in
                { model
                    | feeds = model.feeds ++ [ feed ]
                    , items = model.items ++ items
                    , view =
                        { view
                            | loadFeedState = Empty
                            , urlToAdd = ""
                            , floatPanel = Hidden
                            , listView = ViewFeed feed.url
                            , itemFilter = Unlistened
                        }
                }
                    |> Return.singleton
                    |> Return.command (noOpTask (Dom.blur "add-feed"))
                    |> Return.effect_ saveView
                    |> Return.command (saveFeeds [ feed ])
                    |> Return.command (saveItems items)

        FetchFeedFail error ->
            let
                e =
                    Debug.log "error" error
            in
                model
                    |> updateView (\v -> { v | loadFeedState = Error })
                    |> Return.singleton
                    |> Return.command (noOpTask (Dom.focus "add-feed"))

        Play item ->
            model
                |> updateView
                    (\view ->
                        { view
                            | currentItem = Just ( item.url, item.feedUrl )
                            , playerState = SoundLoading
                        }
                    )
                |> Return.singleton
                |> Return.command
                    (play
                        { url = item.url
                        , seek = item.progress
                        , rate = model.view.playerRate
                        , vol = model.view.playerVol
                        }
                    )
                |> Return.effect_ saveView

        SoundLoaded loaded ->
            model
                |> updateView (\v -> { v | playerState = Playing })
                |> Return.singleton

        Pause item ->
            model
                |> updateView (\v -> { v | playerState = Paused })
                |> Return.singleton
                |> Return.command (pause "")

        Stop item ->
            model
                |> updateView (\v -> { v | playerState = Stopped })
                |> Return.singleton
                |> Return.command (stop "")

        UpdateProgress progress ->
            Return.singleton model
                |> Return.map
                    (updateCurrentItem
                        (\item ->
                            { item
                                | duration = progress.duration
                                , progress = progress.progress
                            }
                        )
                    )
                |> Return.command
                    (getCurrentItem model
                        |> Maybe.map (\item -> saveItems [ item ])
                        |> Maybe.withDefault Cmd.none
                    )

        UpdateAllFeed ->
            Return.singleton model |> Return.command (updateFeeds model.feeds)

        UpdateFeeds feeds feed ->
            Return.singleton model
                |> Return.map (updateModelFeed { feed | state = Refreshing })
                |> Return.command
                    (updateFeed model.setting.fallbackRssServiceUrl feed feeds)

        UpdateFeedFail feeds feed error ->
            let
                e =
                    Debug.log "error" error

                cmd =
                    if List.length feeds > 0 then
                        updateFeeds feeds
                    else
                        Cmd.none
            in
                Return.singleton model
                    |> Return.map (updateModelFeed { feed | state = RefreshError })
                    |> Return.command cmd

        UpdateFeedSucceed feeds ( feed, items ) ->
            let
                ( model_, items_ ) =
                    updateFeedItems model feed items

                cmd =
                    if List.length feeds > 0 then
                        updateFeeds feeds
                    else
                        Cmd.none
            in
                Return.singleton model
                    |> Return.command (saveItems items_)
                    |> Return.command cmd

        SetProgress current ->
            case getCurrentItem model of
                Nothing ->
                    Return.singleton model

                Just item_ ->
                    let
                        item__ =
                            { item_ | progress = current }
                    in
                        Return.singleton model
                            |> Return.map (updateCurrentItem (\item -> item__))
                            |> Return.command (seek current)
                            |> Return.command (saveItems [ item__ ])

        ShowAddPanel ->
            Return.singleton model
                |> Return.map (updateView (\v -> { v | floatPanel = AddPanel }))
                |> Return.command (noOpTask (Dom.focus "add-feed"))

        HideAddPanel ->
            ( model
                |> updateView
                    (\v ->
                        { v
                            | floatPanel = Hidden
                            , urlToAdd = ""
                        }
                    )
            , noOpTask (Dom.blur "add-feed")
            )

        SetListView listView ->
            { model | items = flushPlayCount model.items }
                |> updateView
                    (\v ->
                        { v
                            | listView = listView
                            , floatPanel = Hidden
                            , itemsToShow = defaultModel.view.itemsToShow
                            , itemSelected = Nothing
                        }
                    )
                |> Return.singleton
                |> Return.effect_ saveView
                |> Return.effect_ (\model -> saveItems model.items)

        HideFeed ->
            { model | items = flushPlayCount model.items }
                |> updateView (\v -> { v | listView = AllFeed })
                |> Return.singleton
                |> Return.effect_ saveView
                |> Return.effect_ (\model -> saveItems model.items)

        ShowConfirmDeleteFeed feed ->
            updateModelFeed { feed | showConfirmDelete = True } model
                |> Return.singleton

        HideConfirmDeleteFeed feed ->
            updateModelFeed { feed | showConfirmDelete = False } model
                |> Return.singleton

        ConfirmDeleteFeed feed ->
            let
                feeds =
                    List.filter (\f -> f.url /= feed.url) model.feeds

                items =
                    List.filter (\i -> i.feedUrl /= feed.url) model.items

                currentItemDeleted =
                    not (List.any (\item -> isCurrent item model) items)

                currentItem =
                    if currentItemDeleted then
                        Nothing
                    else
                        model.view.currentItem

                itemUrls =
                    List.map (\item -> ( item.url, item.feedUrl )) items

                playList =
                    List.filter
                        (\playListItem -> not (List.member playListItem itemUrls))
                        model.view.playList
            in
                { model
                    | feeds = feeds
                    , items = items
                }
                    |> updateView
                        (\v ->
                            { v
                                | listView = AllFeed
                                , playList = playList
                                , currentItem = currentItem
                            }
                        )
                    |> Return.singleton
                    |> Return.effect_ saveView
                    |> Return.command (deleteFeed <| toStoreFeed feed)
                    |> Return.command
                        (if currentItemDeleted then
                            stop ""
                         else
                            Cmd.none
                        )

        ClosePlayer ->
            model
                |> updateView
                    (\v ->
                        { v
                            | playerState = Paused
                            , currentItem = Nothing
                        }
                    )
                |> Return.singleton
                |> Return.command (pause "")
                |> Return.effect_ saveView

        PlayEnd url ->
            let
                model_ =
                    model
                        |> updateCurrentItem
                            (\item ->
                                { item
                                    | progress = 0
                                    , markPlayCount = item.playCount + 1
                                }
                            )

                nextInQueue =
                    oneOfMaybe
                        [ getNext (\( url_, feedUrl ) -> url == url_) model.view.playList
                        , List.head model.view.playList
                        ]
                        |> Maybe.map (getItemByUrl model)
                        |> Maybe.withDefault Nothing

                nextItem =
                    oneOfMaybe
                        [ nextInQueue
                        , itemList model
                            |> Tuple.first
                            |> getNext (\( feed, item ) -> item.url == url)
                        ]
            in
                case nextItem of
                    Just ( feed, item_ ) ->
                        model_
                            |> Return.singleton
                            |> Return.andThen (update <| Play item_)
                            |> Return.andThen
                                (getCurrentItem model
                                    |> Maybe.map (\item -> update <| Dequeue item)
                                    |> Maybe.withDefault (Return.singleton)
                                )

                    Nothing ->
                        model_
                            |> updateView (\v -> { v | currentItem = Nothing })
                            |> Return.singleton
                            |> Return.effect_ saveView

        PlayError url ->
            model
                |> updateView (\v -> { v | playerState = SoundError })
                |> Return.singleton

        ToggleRate ->
            let
                rate =
                    [ 1, 1.12, 1.2, 1.5, 2.0 ]
                        |> dropWhile (\r -> r <= model.view.playerRate)
                        |> List.head
                        |> Maybe.withDefault 1
            in
                model
                    |> updateView (\v -> { v | playerRate = rate })
                    |> Return.singleton
                    |> Return.command (setRate rate)
                    |> Return.effect_ saveView

        OpenNewLink url ->
            ( model, openNewLink url )

        SetVol percentage ->
            model
                |> updateView (\v -> { v | playerVol = percentage })
                |> Return.singleton
                |> Return.command (setVol percentage)
                |> Return.effect_ saveView

        SetItemFilter filter ->
            { model | items = flushPlayCount model.items }
                |> updateView
                    (\v ->
                        { v
                            | itemFilter = filter
                            , itemsToShow = defaultModel.view.itemsToShow
                            , itemSelected = Nothing
                        }
                    )
                |> Return.singleton
                |> Return.effect_ saveView
                |> Return.effect_ (.items >> saveItems)

        ShowItemDropdown url ->
            model
                |> updateView (\v -> { v | floatPanel = ItemDropdown url })
                |> Return.singleton

        HideItemDropdown ->
            model
                |> updateView (\v -> { v | floatPanel = hideItemDropdown model.view.floatPanel })
                |> Return.singleton

        SelectItem item ->
            model
                |> updateView (\v -> { v | itemSelected = Just ( item.url, item.feedUrl ) })
                |> Return.singleton

        MarkPlayCount item playCount ->
            model
                |> updateItem
                    (\item -> { item | markPlayCount = playCount })
                    (Just ( item.url, item.feedUrl ))
                |> updateView
                    (\v -> { v | floatPanel = hideItemDropdown v.floatPanel })
                |> Return.singleton
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
                    |> Return.effect_ saveView
                    |> Return.effect_ (\m -> saveItems <| List.filter (\item -> Dict.member item.url toUpdate) m.items)

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
                    |> Return.effect_ (\m -> saveItems <| List.filter (\item -> Dict.member item.url toUpdate) m.items)

        SelectNext ->
            case selectNext model of
                Just ( model_, cmd ) ->
                    Return.return model_ cmd

                Nothing ->
                    Return.singleton model
                        |> Return.andThen (update ShowMoreItem)
                        |> Return.andThen (update SelectNext)

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
                                        oneOfMaybe
                                            [ getNext isDequeued model.view.playList
                                            , getPrev isDequeued model.view.playList
                                            ]
                                    else
                                        model.view.itemSelected
                            }
                        )
                    |> Return.singleton
                    |> Return.effect_ saveView

        MoveQueuedItemUp item ->
            model
                |> updateView
                    (\v ->
                        { v | playList = swapUp ( item.url, item.feedUrl ) model.view.playList }
                    )
                |> Return.singleton
                |> Return.effect_ saveView

        MoveQueuedItemDown item ->
            model
                |> updateView
                    (\v ->
                        { v | playList = swapDown ( item.url, item.feedUrl ) model.view.playList }
                    )
                |> Return.singleton
                |> Return.effect_ saveView

        SetShortcutKeys keys ->
            updateView (\v -> { v | shortcutKeys = keys }) model
                |> Return.singleton
                |> Return.effect_ saveView

        SetFloatPanel panel ->
            updateView (\v -> { v | floatPanel = panel }) model
                |> Return.singleton
                |> Return.effect_ saveView

        MsgBatch list ->
            List.foldl
                Return.andThen
                (Return.singleton model)
                (List.map update list)

        SetItemSortLatest flag ->
            updateView (\v -> { v | itemSortLatest = flag }) model
                |> Return.singleton
                |> Return.effect_ saveView

        SetFallbackRssServiceUrl url ->
            updateSetting
                (\s ->
                    { s
                        | fallbackRssServiceUrl =
                            if url /= "" then
                                Just url
                            else
                                Nothing
                    }
                )
                model
                |> Return.singleton
                |> Return.effect_ saveSetting

        SetFontSize fontSize ->
            updateSetting (\s -> { s | fontSize = fontSize }) model
                |> Return.singleton
                |> Return.effect_ saveSetting

        SetPlayerShowTimeLeft show ->
            updateView (\v -> { v | playerShowTimeLeft = show }) model
                |> Return.singleton
                |> Return.effect_ saveView

        SetTheme theme ->
            updateSetting (\s -> { s | theme = theme }) model
                |> Return.singleton
                |> Return.effect_ saveSetting

        SetEditingFeedTitle feedTitle ->
            model
                |> updateView (\v -> { v | editingFeedTitle = feedTitle })
                |> Return.singleton
                |> Return.command
                    (case feedTitle of
                        Just feedTitle_ ->
                            if model.view.editingFeedTitle == Nothing then
                                noOpTask (Dom.focus "input-feed-title")
                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none
                    )

        SetFeedTitle feed title ->
            let
                feed_ =
                    { feed | title = title }
            in
                updateModelFeed feed_ model
                    |> Return.singleton
                    |> Return.command (saveFeeds [ feed_ ])

        PlayerPaused stopped ->
            getCurrentItem model
                |> Maybe.map (\item -> update (Pause item) model)
                |> Maybe.withDefault (Return.singleton model)
    )
        |> Return.command (updateCurrentTimeCmd msg)


updateCurrentTimeCmd : Msg -> Cmd Msg
updateCurrentTimeCmd msg =
    case msg of
        NoOp ->
            Cmd.none

        UpdateCurrentTime t ->
            Cmd.none

        _ ->
            updateCurrentTime


oneOfMaybe : List (Maybe a) -> Maybe a
oneOfMaybe list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Just x_ ->
                    Just x_

                Nothing ->
                    oneOfMaybe xs


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


noOpTask : Task x a -> Cmd Msg
noOpTask task =
    Task.attempt (\_ -> NoOp) task


saveSetting : Model -> Cmd Msg
saveSetting model =
    model.setting |> toStoreSetting |> storeSetting


saveView : Model -> Cmd Msg
saveView model =
    model.view |> toStoreView |> storeView


saveFeeds : List Feed -> Cmd Msg
saveFeeds feeds =
    feeds
        |> List.map toStoreFeed
        |> storeFeeds


saveItems : List Item -> Cmd Msg
saveItems items =
    storeItems items


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ updateProgress UpdateProgress
        , soundLoaded SoundLoaded
        , playEnd PlayEnd
        , keyUp <| keyMap model
        , playError PlayError
        , paused PlayerPaused
        ]


port play : PlayLoad -> Cmd msg


port stop : String -> Cmd msg


port pause : String -> Cmd msg


port updateProgress : (Progress -> msg) -> Sub msg


port soundLoaded : (Bool -> msg) -> Sub msg


port storeSetting : StoreSetting -> Cmd msg


port storeView : StoreView -> Cmd msg


port storeFeeds : List StoreFeed -> Cmd msg


port storeItems : List Item -> Cmd msg


port deleteFeed : StoreFeed -> Cmd msg


port seek : Float -> Cmd msg


port playEnd : (String -> msg) -> Sub msg


port setRate : Float -> Cmd msg


port openNewLink : String -> Cmd msg


port setVol : Float -> Cmd msg


port keyUp : (String -> msg) -> Sub msg


port playError : (String -> msg) -> Sub msg


port paused : (Bool -> msg) -> Sub msg
