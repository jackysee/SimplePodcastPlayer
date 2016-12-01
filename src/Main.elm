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
    let
        ( model_, cmds ) =
            updateModel msg model
    in
        case msg of
            NoOp ->
                Return.singleton model

            UpdateCurrentTime time ->
                let
                    view =
                        model.view
                in
                    { model | view = { view | currentTime = time } }
                        |> Return.singleton

            _ ->
                Return.singleton model
                    |> Return.command updateCurrentTime



--List (Cmd Msg) -> ( Model, List (Cmd Msg) )


updateModel : Msg -> Model -> Return Msg Model
updateModel msg model =
    case msg of
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
            let
                view =
                    model.view

                view_ =
                    { view
                        | listView = listView
                        , floatPanel = Hidden
                        , itemsToShow = defaultModel.view.itemsToShow
                        , itemSelected = Nothing
                    }

                items =
                    flushPlayCount model.items

                model_ =
                    { model
                        | view = view_
                        , items = items
                    }
            in
                model_
                    ! [ saveView model_
                      , saveItems items
                      ]

        HideFeed ->
            let
                items =
                    flushPlayCount model.items

                model_ =
                    model
                        |> updateView (\v -> { v | listView = AllFeed })
                        |> (\model__ -> { model__ | items = items })
            in
                model_
                    ! [ saveView model_
                      , saveItems items
                      ]

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

                cmds_ =
                    if currentItemDeleted then
                        [ stop "" ]
                    else
                        []

                itemUrls =
                    List.map (\item -> ( item.url, item.feedUrl )) items

                playList =
                    List.filter
                        (\playListItem -> not (List.member playListItem itemUrls))
                        model.view.playList

                model_ =
                    { model | feeds = feeds, items = items }
                        |> updateView
                            (\v ->
                                { v
                                    | listView = AllFeed
                                    , playList = playList
                                    , currentItem = currentItem
                                }
                            )
            in
                model_
                    ! ([ saveView model_
                       , deleteFeed <| toStoreFeed feed
                       ]
                        ++ cmds_
                      )

        ClosePlayer ->
            let
                model_ =
                    model
                        |> updateView
                            (\v ->
                                { v
                                    | playerState = Paused
                                    , currentItem = Nothing
                                }
                            )
            in
                model_
                    ! [ pause ""
                      , saveView model_
                      ]

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
                        Return.singleton model_
                            |> Return.andThen
                                (updateModel
                                    (MsgBatch <|
                                        [ Play item_ ]
                                            ++ (getCurrentItem model
                                                    |> Maybe.map (\item -> [ Dequeue item ])
                                                    |> Maybe.withDefault []
                                               )
                                    )
                                )

                    Nothing ->
                        let
                            model__ =
                                model_
                                    |> updateView (\v -> { v | currentItem = Nothing })
                        in
                            model__ ! [ saveView model__ ]

        PlayError url ->
            ( model |> updateView (\v -> { v | playerState = SoundError }), Cmd.none )

        ToggleRate ->
            let
                rate =
                    [ 1, 1.12, 1.2, 1.5, 2.0 ]
                        |> dropWhile (\r -> r <= model.view.playerRate)
                        |> List.head
                        |> Maybe.withDefault 1

                model_ =
                    model
                        |> updateView (\v -> { v | playerRate = rate })
            in
                model_
                    ! [ setRate rate
                      , saveView model_
                      ]

        OpenNewLink url ->
            ( model, openNewLink url )

        SetVol percentage ->
            let
                model_ =
                    updateView (\v -> { v | playerVol = percentage }) model
            in
                model_
                    ! [ setVol percentage
                      , saveView model_
                      ]

        SetItemFilter filter ->
            let
                items =
                    flushPlayCount model.items

                model_ =
                    { model | items = items }
                        |> updateView
                            (\v ->
                                { v
                                    | itemFilter = filter
                                    , itemsToShow = defaultModel.view.itemsToShow
                                    , itemSelected = Nothing
                                }
                            )
            in
                model_
                    ! [ saveView model_, saveItems items ]

        ShowItemDropdown url ->
            ( updateView (\v -> { v | floatPanel = ItemDropdown url }) model
            , Cmd.none
            )

        HideItemDropdown ->
            ( updateView (\v -> { v | floatPanel = hideItemDropdown model.view.floatPanel }) model
            , Cmd.none
            )

        SelectItem item ->
            ( updateView (\v -> { v | itemSelected = Just ( item.url, item.feedUrl ) }) model
            , Cmd.none
            )

        MarkPlayCount item playCount ->
            let
                cmd_ =
                    [ saveItems [ item ] ]

                model_ =
                    model
                        |> updateItem
                            (\item -> { item | markPlayCount = playCount })
                            (Just ( item.url, item.feedUrl ))
                        |> updateView
                            (\v -> { v | floatPanel = hideItemDropdown v.floatPanel })
            in
                model_
                    ! ([ saveView model_ ]
                        ++ cmd_
                      )

        MarkItemsBelowListened url ->
            let
                toUpdate =
                    Dict.fromList
                        (itemListAll False model
                            |> Tuple.first
                            |> dropWhile (\( feed, item ) -> item.url /= url)
                            |> List.map (\( feed, item ) -> ( item.url, True ))
                        )

                model_ =
                    model
                        |> (\model_ ->
                                { model_ | items = markItemsListened toUpdate model.items }
                           )
                        |> updateView
                            (\v ->
                                { v | floatPanel = hideItemDropdown model.view.floatPanel }
                            )

                items =
                    List.filter (\item -> Dict.member item.url toUpdate) model_.items
            in
                model_
                    ! [ saveView model_
                      , saveItems items
                      ]

        MarkAllItemsAsListened ->
            let
                toUpdate =
                    Dict.fromList
                        (itemListAll False model
                            |> Tuple.first
                            |> List.map (\( feed, item ) -> ( item.url, True ))
                        )

                model_ =
                    { model | items = markItemsListened toUpdate model.items }

                items =
                    List.filter (\item -> Dict.member item.url toUpdate) model_.items
            in
                ( model_, saveItems items )

        SelectNext ->
            case selectNext model of
                Just ( model_, cmd ) ->
                    ( model_, cmd )

                Nothing ->
                    Return.singleton model
                        |> Return.andThen
                            (updateModel
                                (MsgBatch
                                    [ ShowMoreItem
                                    , SelectNext
                                    ]
                                )
                            )

        SelectPrev ->
            selectPrev model

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
                                            model.view.playList ++ [ ( item.url, item.feedUrl ) ]
                                    , floatPanel = hideItemDropdown model.view.floatPanel
                                }
                            )
            in
                ( model_, saveView model_ )

        Dequeue item ->
            let
                isDequeued =
                    (\item_ -> isItemEqual (Just item_) item)

                model_ =
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
            in
                ( model_, saveView model_ )

        MoveQueuedItemUp item ->
            let
                model_ =
                    model
                        |> updateView
                            (\v ->
                                { v | playList = swapUp ( item.url, item.feedUrl ) model.view.playList }
                            )
            in
                ( model_, saveView model_ )

        MoveQueuedItemDown item ->
            let
                model_ =
                    model
                        |> updateView
                            (\v ->
                                { v | playList = swapDown ( item.url, item.feedUrl ) model.view.playList }
                            )
            in
                ( model_, saveView model_ )

        SetShortcutKeys keys ->
            let
                model_ =
                    updateView (\v -> { v | shortcutKeys = keys }) model
            in
                ( model_, saveView model_ )

        SetFloatPanel panel ->
            let
                model_ =
                    updateView (\v -> { v | floatPanel = panel }) model
            in
                ( model_, saveView model_ )

        MsgBatch list ->
            List.foldl
                Return.andThen
                (Return.singleton model)
                (List.map updateModel list)

        SetItemSortLatest flag ->
            let
                model_ =
                    updateView (\v -> { v | itemSortLatest = flag }) model
            in
                ( model_, saveView model_ )

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
                ( model_, saveSetting model_ )

        SetFontSize fontSize ->
            let
                model_ =
                    updateSetting (\s -> { s | fontSize = fontSize }) model
            in
                ( model_, saveSetting model_ )

        SetPlayerShowTimeLeft show ->
            let
                model_ =
                    updateView (\v -> { v | playerShowTimeLeft = show }) model
            in
                ( model_, saveView model_ )

        SetTheme theme ->
            let
                model_ =
                    updateSetting (\s -> { s | theme = theme }) model
            in
                ( model_, saveSetting model_ )

        SetEditingFeedTitle feedTitle ->
            let
                cmd_ =
                    case feedTitle of
                        Just feedTitle_ ->
                            if model.view.editingFeedTitle == Nothing then
                                noOpTask (Dom.focus "input-feed-title")
                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none

                model_ =
                    model
                        |> updateView (\v -> { v | editingFeedTitle = feedTitle })
            in
                ( model_, cmd_ )

        SetFeedTitle feed title ->
            let
                feed_ =
                    { feed | title = title }

                model_ =
                    updateModelFeed feed_ model
            in
                ( model_, saveFeeds [ feed_ ] )

        _ ->
            Return.singleton model


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
