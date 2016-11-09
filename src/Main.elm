port module Main exposing (..)

import Html.App as App
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


main : Program (Maybe Json.Decode.Value)
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
        Just storeModel_ ->
            let
                model = fromStoreModel storeModel_
            in
                model ! [ updateCurrentTime
                        , updateFeeds model.list
                        ]
        Nothing ->
            defaultModel ! [ updateCurrentTime ]


updateCurrentTime : Cmd Msg
updateCurrentTime =
    Task.perform (\_ -> NoOp) UpdateCurrentTime Time.now


updateFeeds : List Feed -> Cmd Msg
updateFeeds feeds =
    case feeds of
        [] ->
            Cmd.none

        feed :: feeds ->
            Task.perform
                (\_ -> NoOp)
                (UpdateFeeds feeds)
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
                    ++ [ saveModel model ]
                    )


updateModel : Msg -> Model -> List (Cmd Msg) -> (Model, List (Cmd Msg))
updateModel msg model cmds =
    case msg of
        NoOp ->
            (model, [])

        UpdateCurrentTime time ->
            ({ model | currentTime = time }, [])

        SetUrl value ->
            ({ model
                | urlToAdd = value
                , loadFeedState = Empty
            }
            , cmds)

        AddFeed ->
            if List.any (\feed -> feed.url == model.urlToAdd) model.list then
                ({ model | loadFeedState = AlreadyExist }, cmds)
            else
                ({ model | loadFeedState = Loading }
                , [ loadFeed model.fallbackRssServiceUrl model.urlToAdd ] ++ cmds
                )

        ShowMoreItem ->
            ({ model | itemsToShow = model.itemsToShow + defaultModel.itemsToShow }
            , cmds
            )

        FetchFeedSucceed feed ->
            ({ model
                | list =
                    model.list
                        ++ [ { feed | items = feed.items } ]
                , loadFeedState = Empty
                , urlToAdd = ""
                , floatPanel = Hidden
                , listView = ViewFeed feed.url
                , itemFilter = Unlistened
            }
            , [ noOpTask (Dom.blur "add-feed") ] ++ cmds
            )

        FetchFeedFail error ->
            let
                e = Debug.log "error" error
            in
                ({ model | loadFeedState = Error }
                , [ noOpTask (Dom.focus "add-feed") ] ++ cmds
                )


        Play item ->
            ({ model
                | currentItemUrl = Just item.url
                , playerState = SoundLoading
            }
            , [ play
                { url = item.url
                , seek = item.progress
                , rate = model.playerRate
                , vol = model.playerVol
                }
              ] ++ cmds)

        SoundLoaded loaded ->
            ({ model | playerState = Playing } , cmds)

        Pause item ->
            ({ model | playerState = Paused } , [ pause  "" ] ++ cmds)

        Stop item ->
            ({ model | playerState = Stopped } , [ stop "" ] ++ cmds)

        UpdateProgress progress ->
            ( model
                |> updateCurrentItem
                    (\item ->
                        { item
                            | duration = progress.duration
                            , progress = progress.progress })
            , cmds
            )

        UpdateAllFeed ->
            ( model, [ updateFeeds model.list ] ++ cmds)

        UpdateFeeds feeds feed ->
            let
                model' = updateModelFeed { feed | state = Refreshing } model
            in
                (model', [ updateFeed model.fallbackRssServiceUrl feed feeds ] ++ cmds)

        UpdateFeedFail feeds feed error ->
            let
                e = Debug.log "error" error
                cmds' =
                    if List.length feeds > 0 then
                        [ updateFeeds feeds ]
                    else
                        []
            in
                (updateModelFeed { feed | state = RefreshError } model
                , cmds' ++ cmds)

        UpdateFeedSucceed feeds feed ->
            let
                cmds' =
                    if List.length feeds > 0 then
                        [ updateFeeds feeds ]
                    else
                        []
            in
                (updateFeedItems model feed, cmds' ++ cmds)

        SetProgress current ->
            case getCurrentItem model of
                Nothing ->
                    ( model, cmds )
                Just item' ->
                    ( updateCurrentItem
                        (\item -> { item | progress = current })
                        model
                    , [seek current] ++ cmds
                    )

        ShowAddPanel ->
            ( { model | floatPanel = AddPanel }
            , [ noOpTask (Dom.focus "add-feed") ] ++ cmds
            )

        HideAddPanel ->
            ({ model
                | floatPanel = Hidden
                , urlToAdd = ""
             }
            , [ noOpTask (Dom.blur "add-feed") ] ++ cmds
            )

        SetListView listView ->
            ({ model
                | listView = listView
                , list = flushPlayCount model.list
                , floatPanel = Hidden
                , itemsToShow = defaultModel.itemsToShow
             }
            , cmds)

        HideFeed ->
            ({ model
                | listView = AllFeed
                , list = flushPlayCount model.list
             }
            , cmds)

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
                list = List.filter (\f -> f.url /= feed.url ) model.list
                currentItemDeleted = List.any (\item -> isCurrent item.url model) feed.items
                currentItemUrl =
                    if currentItemDeleted then
                        Nothing
                    else
                        model.currentItemUrl
                cmds' =
                    if currentItemDeleted then
                        [ stop "" ]
                    else
                        []
                itemUrls = List.map (\item -> item.url) feed.items
                playList = List.filter
                    (\url -> not (List.member url itemUrls))
                    model.playList
            in
                ({ model
                    | list = list
                    , currentItemUrl = currentItemUrl
                    , listView = AllFeed
                    , playList = playList
                 }
                , cmds' ++ cmds
                )

        ClosePlayer ->
            ( { model
                | playerState = Paused
                , currentItemUrl = Nothing
              }
            , [ pause  "" ] ++ cmds
            )

        PlayEnd url ->
            let
                model_ = updateCurrentItem
                            (\item ->
                                { item
                                    | progress = 0
                                    , markPlayCount = item.playCount + 1
                                }
                            )
                            model
                nextInQueue =
                    Maybe.oneOf
                        [ getNext (\url_ -> url == url_) model.playList
                        , List.head model.playList
                        ]
                        |> Maybe.map (getItemByUrl model)
                        |> Maybe.withDefault Nothing
                nextItem =
                    Maybe.oneOf
                        [ nextInQueue
                        , itemList model
                            |> fst
                            |> getNext (\(feed, item) -> item.url == url)
                        ]
            in
                case nextItem of
                    Just (feed, item_) ->
                        updateModel
                            (MsgBatch
                                [ Play item_
                                , Dequeue url
                                ]
                            )
                            model_
                            cmds

                    Nothing ->
                        ({ model_ | currentItemUrl = Nothing }, cmds)

        PlayError url ->
            ({ model | playerState = SoundError }, cmds)

        ToggleRate ->
            let
                rate = [1, 1.2, 1.5, 2.0]
                    |> dropWhile (\r -> r <= model.playerRate)
                    |> List.head
                    |> Maybe.withDefault 1
            in
                ({ model | playerRate = rate }, [ setRate rate ] ++ cmds)

        OpenNewLink url ->
            (model , [ openNewLink url ] ++ cmds)

        SetVol percentage ->
            ({ model | playerVol = percentage }
            , [ setVol percentage ] ++ cmds)

        SetItemFilter filter ->
            ({ model
                | itemFilter = filter
                , list = flushPlayCount model.list
                , itemsToShow = defaultModel.itemsToShow
             }
            , cmds
            )

        ShowItemDropdown url  ->
            ({ model | floatPanel = ItemDropdown url } , cmds )

        HideItemDropdown ->
            ({ model | floatPanel = hideItemDropdown model.floatPanel }
            , cmds )

        SelectItem item ->
            ({ model | itemSelected = Just item.url } , cmds )

        UnselectItem item ->
            (model, cmds)

        MarkPlayCount url playCount ->
            let
                model' = model
                    |> updateItem
                        (\item -> { item | markPlayCount = playCount })
                        (Just url)
                    |> (\model_ ->
                        { model_ | floatPanel = hideItemDropdown model_.floatPanel }
                       )
            in
                (model' , cmds)

        MarkItemsBelowListened url ->
            let
                toUpdate =
                    Dict.fromList
                        (itemListAll False model
                            |> fst
                            |> dropWhile (\(feed, item) -> item.url /= url)
                            |> List.map (\(feed, item) -> (item.url, True))
                        )
                model' =
                    { model
                        | floatPanel = hideItemDropdown model.floatPanel
                        , list = markItemsListened toUpdate model.list
                    }
            in
                (model', cmds)

        MarkAllItemsAsListened ->
            let
                toUpdate =
                    Dict.fromList
                        (itemListAll False model
                            |> fst
                            |> List.map (\(feed, item) -> (item.url, True))
                        )
                model' =
                    { model | list = markItemsListened toUpdate model.list }
            in
                (model', cmds)

        SelectNext ->
            case selectNext model of
                Just (model', cmd) ->
                    (model', [cmd] ++ cmds)

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
                (model', cmd) = selectPrev model
            in
                (model', [cmd] ++ cmds)

        Enqueue url ->
            ({ model
                | playList =
                    if List.member url model.playList then
                        model.playList
                    else
                        model.playList ++ [url]
                , floatPanel = hideItemDropdown model.floatPanel
             }
            , cmds)

        Dequeue url ->
            let
                isDequeued = (\url_ -> url == url_)
            in
                ({ model
                    | playList = List.filter (not << isDequeued) model.playList
                    , floatPanel = hideItemDropdown model.floatPanel
                    , itemSelected =
                        if model.listView == Queued then
                            Maybe.oneOf
                                [ getNext isDequeued model.playList
                                , getPrev isDequeued model.playList
                                ]
                        else
                            model.itemSelected

                 }
                , cmds)

        MoveQueuedItemUp url ->
            ({ model | playList = swapUp url model.playList }
            , cmds)

        MoveQueuedItemDown url ->
            ({ model | playList = swapDown url model.playList }
            , cmds)

        ToggleShortcutGoto flag ->
            ({ model | shortcutGoTo = flag }, cmds)

        SetShortcutKeys keys ->
            ({ model | shortcutKeys = keys }, cmds)

        SetFloatPanel panel ->
            ({ model | floatPanel = panel }, cmds)

        MsgBatch list ->
            List.foldl
                (\msg (model, cmds) -> updateModel msg model cmds)
                (model, cmds)
                list

        SetItemSortLatest flag ->
            ({ model | itemSortLatest = flag }, cmds)

        SetFallbackRssServiceUrl url ->
            ({ model | fallbackRssServiceUrl =
                    if url /= "" then
                        Just url
                    else
                        Nothing
             }
            , cmds)

        SetFontSize fontSize ->
            ({ model | fontSize = fontSize }, cmds)

        SetPlayerShowTimeLeft show ->
            ({ model | playerShowTimeLeft = show }, cmds)

        SetTheme theme ->
            ({ model | theme = theme }, cmds)

        SetEditingFeedTitle editing ->
            let
                cmd_ =
                    if editing then
                        [ noOpTask (Dom.focus "input-feed-title") ]
                    else
                        []
            in
                ({ model | editingFeedTitle = editing } , cmd_ ++ cmds )

        SetFeedTitle feed title ->
            ( updateModelFeed { feed | title = title } model
            , cmds
            )


flushPlayCount : List Feed -> List Feed
flushPlayCount list =
    List.map (\feed ->
        { feed
            | items = List.map (\item ->
                if item.markPlayCount /= -1 then
                    { item
                        | playCount = item.markPlayCount
                        , markPlayCount = -1
                    }
                else
                    item
              ) feed.items
        }
    ) list


noOpTask : Task x a -> Cmd Msg
noOpTask task =
    Task.perform (\_ -> NoOp) (\_ -> NoOp) task


saveModel : Model -> Cmd Msg
saveModel model =
    model
        |> toStoreModel
        |> storeModel


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
port storeModel : StoreModel -> Cmd msg
port seek : Float -> Cmd msg
port playEnd: (String -> msg) -> Sub msg
port setRate : Float -> Cmd msg
port openNewLink : String -> Cmd msg
port setVol : Float -> Cmd msg
port keyUp: (String -> msg) -> Sub msg
port playError: (String -> msg) -> Sub msg
