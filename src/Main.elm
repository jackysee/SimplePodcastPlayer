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
import Feed exposing
    ( loadFeed, updateFeed, updateModelFeed, updateFeedItems, markItemsListened )
import Shortcut exposing (keyMap, selectNext, selectPrev)
import FloatPlanel exposing (hideItemDropdown)
import DecodeStoreModel exposing (decodeStoreValue)


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
                model = fromStoreModel storeModel
            in
                model ! [ updateCurrentTime
                        , updateFeeds model.feeds
                        ]
        Nothing ->
            defaultModel !
                [ saveSetting defaultModel
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
                    case result of
                        Ok feed ->
                            UpdateFeeds feeds feed
                        Err err ->
                            NoOp
                )
                (Task.succeed feed)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        (model_, cmds) = updateModel msg model []
    in
        case msg of
            NoOp ->
                model_ ! []

            UpdateCurrentTime time ->
                model_ ! []

            _ ->
                model_ !
                    ( [ updateCurrentTime ]
                    ++ cmds
                    -- ++ [ saveModel model ]
                    )


updateModel : Msg -> Model -> List (Cmd Msg) -> (Model, List (Cmd Msg))
updateModel msg model cmds =
    case msg of
        NoOp ->
            (model, [])

        UpdateCurrentTime time ->
            let
                view = model.view
            in
                ({ model | view = { view | currentTime = time }}, [])

        SetUrl value ->
            ( model
                |> updateView (\view ->
                    { view
                        | urlToAdd = value
                        , loadFeedState = Empty
                    })
            , cmds
            )

        AddFeed ->
            if List.any (\feed -> feed.url == model.view.urlToAdd) model.feeds then
                ( model |> updateView (\view -> { view | loadFeedState = AlreadyExist })
                , cmds)
            else
                ( model |> updateView (\view -> { view | loadFeedState = Loading })
                , [ loadFeed
                        model.setting.fallbackRssServiceUrl
                        model.view.urlToAdd
                  ]
                  ++ cmds
                )

        ShowMoreItem ->
            let
                view = model.view
            in
                ({ model |
                    view =
                        { view | itemsToShow = view.itemsToShow + defaultModel.view.itemsToShow }
                 }
                , cmds
                )

        FetchFeedSucceed (feed, items) ->
            let
                view = model.view
                model_ =
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
            in
                ( model_
                , [ noOpTask (Dom.blur "add-feed")
                  , saveView model_
                  , saveFeeds [feed]
                  , saveItems items
                  ]
                )

        FetchFeedFail error ->
            let
                e = Debug.log "error" error
            in
                ( model |> updateView (\v -> { v | loadFeedState = Error })
                , [ noOpTask (Dom.focus "add-feed") ] ++ cmds
                )


        Play item ->
            let
                model_ = model
                    |> updateView
                        (\view ->
                            { view
                                | currentItem = Just (item.url, item.feedUrl)
                                , playerState = SoundLoading
                            }
                        )
            in
                ( model_
                , [ play
                    { url = item.url
                    , seek = item.progress
                    , rate = model.view.playerRate
                    , vol = model.view.playerVol
                    }
                  , saveView model_
                  ]
                )

        SoundLoaded loaded ->
            ( model |> updateView (\v -> { v | playerState = Playing })
            , cmds)

        Pause item ->
            ( model |> updateView (\v -> { v | playerState = Paused })
            , [ pause  "" ] ++ cmds)

        Stop item ->
            ( model |> updateView (\v -> { v | playerState = Stopped })
            , [ stop "" ] ++ cmds)

        UpdateProgress progress ->
            let
                model_ = updateCurrentItem
                            (\item ->
                                { item
                                    | duration = progress.duration
                                    , progress = progress.progress
                                }
                            )
                            model
                cmd_ = getCurrentItem model
                        |> Maybe.map (\item -> [saveItems [item]])
                        |> Maybe.withDefault []

            in
                ( model_
                --  [ saveModel model_ ]
                , cmd_ ++ cmds
                )

        UpdateAllFeed ->
            ( model, [ updateFeeds model.feeds ] ++ cmds)

        UpdateFeeds feeds feed ->
            let
                model_ = updateModelFeed { feed | state = Refreshing } model
            in
                (model_
                , [ updateFeed
                        model.setting.fallbackRssServiceUrl
                        feed
                        feeds
                  ] ++ cmds
                )

        UpdateFeedFail feeds feed error ->
            let
                e = Debug.log "error" error
                cmds_ =
                    if List.length feeds > 0 then
                        [ updateFeeds feeds ]
                    else
                        []
            in
                (updateModelFeed { feed | state = RefreshError } model
                , cmds_ ++ cmds)

        UpdateFeedSucceed feeds (feed, items) ->
            let
                (model_, items_) = updateFeedItems model feed items
                cmds_ =
                    if List.length feeds > 0 then
                        [ updateFeeds feeds ]
                    else
                        []
            in
                (model_
                , [ saveItems items_ ]
                    ++ cmds_
                    ++ cmds
                )

        SetProgress current ->
            case getCurrentItem model of
                Nothing ->
                    ( model, cmds )

                Just item_ ->
                    let
                        item__ = { item_ | progress = current }
                        model_ = updateCurrentItem (\item -> item__ ) model
                    in
                        ( model_
                        , [ seek current
                          --, saveModel model_
                          , saveItems [item__]
                          ]
                          ++ cmds
                        )

        ShowAddPanel ->
            ( model |> updateView (\v -> { v | floatPanel = AddPanel })
            , [ noOpTask (Dom.focus "add-feed") ] ++ cmds
            )

        HideAddPanel ->
            ( model
                |> updateView
                    (\v ->
                        { v
                            | floatPanel = Hidden
                            , urlToAdd = ""
                        }
                    )
            , [ noOpTask (Dom.blur "add-feed") ] ++ cmds
            )

        SetListView listView ->
            let
                view = model.view
                view_ =
                    { view
                        | listView = listView
                        , floatPanel = Hidden
                        , itemsToShow = defaultModel.view.itemsToShow
                        , itemSelected = Nothing
                    }
                items = flushPlayCount model.items
                model_ =
                    { model
                        | view = view_
                        , items = items
                    }
            in
                ( model_
                , [ saveView model_
                  , saveItems items
                  ]
                    ++ cmds
                )

        HideFeed ->
            let
                items = flushPlayCount model.items
                model_ = model
                    |> updateView (\v -> { v | listView = AllFeed })
                    |> (\model__ -> { model__ | items = items })
            in
                ( model_
                , [ saveView model_
                  , saveItems items
                  ] ++ cmds
                )

        ShowConfirmDeleteFeed feed ->
            ( updateModelFeed { feed | showConfirmDelete = True } model
            , cmds
            )

        HideConfirmDeleteFeed feed ->
            ( updateModelFeed { feed | showConfirmDelete = False } model
            , cmds
            )

        ConfirmDeleteFeed feed ->
            let
                feeds = List.filter (\f -> f.url /= feed.url ) model.feeds
                items = List.filter (\i -> i.feedUrl /= feed.url) model.items
                currentItemDeleted = not (List.any (\item -> isCurrent item model) items)
                currentItem =
                    if currentItemDeleted then
                        Nothing
                    else
                        model.view.currentItem
                cmds_ =
                    if currentItemDeleted then
                        [ stop "" ]
                    else
                        []
                itemUrls = List.map (\item -> (item.url, item.feedUrl)) items
                playList = List.filter
                    (\playListItem -> not (List.member playListItem itemUrls))
                    model.view.playList
                model_ =
                    { model | feeds = feeds , items = items }
                        |> updateView
                            (\v ->
                                { v
                                    | listView = AllFeed
                                    , playList = playList
                                    , currentItem = currentItem
                                }
                            )
            in
                ( model_
                , [ saveView model_
                  , deleteFeed <| toStoreFeed feed
                  ]
                    ++ cmds_
                    ++ cmds
                )

        ClosePlayer ->
            let
                model_ =
                    model
                        |> updateView (\v ->
                            { v
                                | playerState = Paused
                                , currentItem = Nothing
                            }
                        )
            in
                ( model_
                , [ pause  ""
                  , saveView model_
                  ]
                  ++ cmds
                )

        PlayEnd url ->
            let
                model_ = model
                    |> updateCurrentItem
                            (\item ->
                                { item
                                    | progress = 0
                                    , markPlayCount = item.playCount + 1
                                }
                            )
                nextInQueue =
                    oneOfMaybe
                        [ getNext (\(url_, feedUrl) -> url == url_) model.view.playList
                        , List.head model.view.playList
                        ]
                        |> Maybe.map (getItemByUrl model)
                        |> Maybe.withDefault Nothing
                nextItem =
                    oneOfMaybe
                        [ nextInQueue
                        , itemList model
                            |> Tuple.first
                            |> getNext (\(feed, item) -> item.url == url)
                        ]
            in
                case nextItem of
                    Just (feed, item_) ->
                        updateModel
                            (MsgBatch <|
                                [ Play item_ ]
                                ++
                                    (getCurrentItem model
                                        |> Maybe.map (\item -> [ Dequeue item ])
                                        |> Maybe.withDefault []
                                    )
                            )
                            model_ cmds

                    Nothing ->
                        let
                            model__ = model_
                                |> updateView (\v ->{ v | currentItem = Nothing })
                        in
                            ( model__
                            , [ saveView model__ ]
                                ++ cmds
                            )

        PlayError url ->
            ( model |> updateView (\v -> { v | playerState = SoundError }), cmds)

        ToggleRate ->
            let
                rate =
                    [1, 1.12, 1.2, 1.5, 2.0]
                        |> dropWhile (\r -> r <= model.view.playerRate)
                        |> List.head
                        |> Maybe.withDefault 1
                model_ = model
                    |> updateView (\v -> { v | playerRate = rate })
            in
                ( model_
                , [ setRate rate
                  , saveView model_
                  ]
                  ++ cmds
                )

        OpenNewLink url ->
            (model , [ openNewLink url ] ++ cmds)

        SetVol percentage ->
            let
                model_ = updateView (\v -> { v | playerVol = percentage }) model
            in
                ( model_
                , [ setVol percentage
                  , saveView model_
                  ]
                  ++ cmds
                )

        SetItemFilter filter ->
            let
                items = flushPlayCount model.items
                model_ =
                    { model | items = items }
                        |> updateView (\v->
                                { v
                                    | itemFilter = filter
                                    , itemsToShow = defaultModel.view.itemsToShow
                                    , itemSelected = Nothing
                                }
                            )
            in
                ( model_
                , [ saveView model_ , saveItems items ] ++ cmds
                )

        ShowItemDropdown url  ->
            ( updateView (\v -> { v | floatPanel = ItemDropdown url }) model
            , cmds )

        HideItemDropdown ->
            ( updateView (\v -> { v | floatPanel = hideItemDropdown model.view.floatPanel }) model
            , cmds )

        SelectItem item ->
            ( updateView (\v -> { v | itemSelected = Just (item.url, item.feedUrl) }) model
            , cmds )

        MarkPlayCount item playCount ->
            let
                cmd_ = [ saveItems [item] ]
                model_ = model
                    |> updateItem
                        (\item -> { item | markPlayCount = playCount })
                        (Just (item.url, item.feedUrl))
                    |> updateView
                        (\v -> { v | floatPanel = hideItemDropdown v.floatPanel })
            in
                ( model_
                , [ saveView model_ ]
                      ++ cmd_
                      ++ cmds
                )

        MarkItemsBelowListened url ->
            let
                toUpdate =
                    Dict.fromList
                        (itemListAll False model
                            |> Tuple.first
                            |> dropWhile (\(feed, item) -> item.url /= url)
                            |> List.map (\(feed, item) -> (item.url, True))
                        )
                model_ =
                    model
                        |> (\model_ ->
                            { model_ | items = markItemsListened toUpdate model.items }
                        )
                        |> updateView (\v ->
                            { v | floatPanel = hideItemDropdown model.view.floatPanel }
                        )
                items = List.filter (\item -> Dict.member item.url toUpdate) model_.items
            in
                ( model_
                , [ saveView model_
                  , saveItems items
                  ]
                    ++ cmds
                )

        MarkAllItemsAsListened ->
            let
                toUpdate =
                    Dict.fromList
                        (itemListAll False model
                            |> Tuple.first
                            |> List.map (\(feed, item) -> (item.url, True))
                        )
                model_ = { model | items = markItemsListened toUpdate model.items }
                items = List.filter (\item -> Dict.member item.url toUpdate) model_.items
            in
                ( model_
                , [ saveItems items ] ++ cmds
                )

        SelectNext ->
            case selectNext model of
                Just (model_, cmd) ->
                    (model_, [cmd] ++ cmds)

                Nothing ->
                    updateModel
                        ( MsgBatch
                            [ ShowMoreItem
                            , SelectNext
                            ]
                        )
                        model
                        cmds

        SelectPrev ->
            let
                (model_, cmd) = selectPrev model
            in
                (model_, [cmd] ++ cmds)

        Enqueue item ->
            let
                model_ =
                    model
                        |> updateView
                            (\v ->
                                { v
                                    | playList =
                                        if inPlayList item model then
                                            model.view.playList
                                        else
                                            model.view.playList ++ [(item.url, item.feedUrl)]
                                    , floatPanel = hideItemDropdown model.view.floatPanel
                                 }
                            )
            in
                ( model_
                , [ saveView model_ ] ++ cmds
                )

        Dequeue item ->
            let
                isDequeued = (\item_ -> isItemEqual (Just item_) item)
                model_ =
                    model
                        |> updateView (\v ->
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
            in
                ( model_
                , [ saveView model_ ] ++ cmds
                )

        MoveQueuedItemUp item ->
            let
                model_ = model
                    |> updateView (\v ->
                        { v | playList = swapUp (item.url, item.feedUrl) model.view.playList }
                    )
            in
                (model_, [ saveView model_ ] ++ cmds)

        MoveQueuedItemDown item ->
            let
                model_ = model
                    |> updateView (\v ->
                        { v | playList = swapDown (item.url, item.feedUrl) model.view.playList }
                    )
            in
                (model_, [ saveView model_ ] ++ cmds)

        SetShortcutKeys keys ->
            let
                model_ =  updateView (\v -> { v | shortcutKeys = keys } ) model
            in
                ( model_
                , [ saveView model_ ] ++ cmds
                )

        SetFloatPanel panel ->
            let
                model_ =  updateView (\v -> { v | floatPanel = panel} ) model
            in
            ( model_
            , [ saveView model_ ] ++ cmds
            )

        MsgBatch list ->
            List.foldl
                (\msg (model, cmds) -> updateModel msg model cmds)
                (model, cmds)
                list

        SetItemSortLatest flag ->
            let
                model_ = updateView (\v -> { v | itemSortLatest = flag }) model
            in
                (model_, [ saveView model_ ] ++ cmds)

        SetFallbackRssServiceUrl url ->
            let
                model_ =
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
            in
                (model_, [ saveSetting model_ ] ++ cmds)

        SetFontSize fontSize ->
            let
                model_ = updateSetting (\s -> { s | fontSize = fontSize }) model
            in
                (model_, [ saveSetting model_ ] ++ cmds)

        SetPlayerShowTimeLeft show ->
            let
                model_ = updateView (\v -> { v | playerShowTimeLeft = show }) model
            in
                (model_, [ saveView model_ ] ++ cmds)

        SetTheme theme ->
            let
                model_ = updateSetting (\s -> { s | theme = theme }) model
            in
                (model_, [ saveSetting model_ ] ++ cmds)


        SetEditingFeedTitle feedTitle ->
            let
                cmd_ =
                    case feedTitle of
                        Just feedTitle_ ->
                            if model.view.editingFeedTitle == Nothing then
                                [ noOpTask (Dom.focus "input-feed-title") ]
                            else
                                []
                        Nothing ->
                            []
                model_ = model
                    |> updateView (\v -> { v | editingFeedTitle = feedTitle })
            in
                ( model_
                , cmd_ ++ cmds )

        SetFeedTitle feed title ->
            let
                feed_ = { feed | title = title }
                model_ = updateModelFeed feed_ model
            in
                (model_, [ saveFeeds [feed_] ] ++ cmds)


oneOfMaybe : List (Maybe a) -> Maybe a
oneOfMaybe list =
    case list of
        [] ->
            Nothing

        x::xs ->
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


saveFeeds: List Feed -> Cmd Msg
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
        ]


port play : PlayLoad -> Cmd msg
port stop : String -> Cmd msg
port pause : String -> Cmd msg
port updateProgress : (Progress -> msg) -> Sub msg
port soundLoaded : (Bool -> msg) -> Sub msg
port storeSetting: StoreSetting -> Cmd msg
port storeView: StoreView-> Cmd msg
port storeFeeds : List StoreFeed -> Cmd msg
port storeItems : List Item -> Cmd msg
port deleteFeed : StoreFeed -> Cmd msg
port seek : Float -> Cmd msg
port playEnd: (String -> msg) -> Sub msg
port setRate : Float -> Cmd msg
port openNewLink : String -> Cmd msg
port setVol : Float -> Cmd msg
port keyUp: (String -> msg) -> Sub msg
port playError: (String -> msg) -> Sub msg
