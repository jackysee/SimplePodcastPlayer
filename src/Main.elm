port module Main exposing (..)

import Html exposing (div, text, input, Html, span, ul, li, button, img, node)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, src, style, classList, type', checked, title, property)
import Task exposing (Task)
import Time exposing (Time)
import ListUtil exposing (dropWhile, takeWhile, swapDown, swapUp, getNext)
import Dom
import Dict

import Models exposing (..)
import Msgs exposing (..)
import Feed exposing
    ( loadFeed, updateFeed, updateModelFeed, updateFeedItems
    , viewFeedTitle, viewItem )
import AddFeed exposing (viewAddFeed, addFeedButton)
import Player exposing (viewPlayer)
import Shortcut exposing (keyMap, selectNext, selectPrev)
import Events exposing (onScroll)
import About exposing (viewAbout, viewAboutButton)
import FloatPlanel exposing (hideItemDropdown, initAddPanel)
import Json.Encode


main : Program (Maybe StoreModel)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe StoreModel -> ( Model, Cmd Msg )
init storeModel =
    case storeModel of
        Just m ->
            let
                feeds = List.map toFeed m.list
            in
                { defaultModel
                    | floatPanel = initAddPanel feeds
                    , urlToAdd = m.urlToAdd
                    , list = feeds
                    , currentItemUrl = m.currentItemUrl
                    , playerRate = m.playerRate
                    , playerVol = m.playerVol
                    , itemFilter = toItemFilter m.itemFilter
                    , playList = m.playList
                    , fallbackRssServiceUrl = m.fallbackRssServiceUrl
                    , fontSize = toFontSize m.fontSize
                }
                    ! [ updateCurrentTime
                      , updateFeeds feeds
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
                model_ ! ([ updateCurrentTime ]
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
            ({ model | itemsToShow = model.itemsToShow + 30 }
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
                , showFeedUrl = Just feed.url
            }
            , [ noOpTask (Dom.blur "add-feed") ] ++ cmds
            )

        FetchFeedFail error ->
            let
                e = Debug.log "error" error
            in
                ({ model | loadFeedState = Error }, cmds)


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
            ({ model | floatPanel = Hidden }
            , [ noOpTask (Dom.blur "add-feed") ] ++ cmds
            )

        ShowFeed url ->
            ({ model
                | showFeedUrl = Just url
                , list = flushPlayCount model.list
                , floatPanel = Hidden
             }
            , cmds)

        HideFeed ->
            ({ model
                | showFeedUrl = Nothing
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
            in
                ({ model
                    | list = list
                    , currentItemUrl = currentItemUrl
                    , showFeedUrl = Nothing
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
                , itemsToShow = 30
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
                        , list = List.map
                            (\feed ->
                                { feed | items = List.map
                                    (\item ->
                                        if Dict.member item.url toUpdate then
                                            { item | markPlayCount =
                                                if item.markPlayCount == -1 then
                                                    item.playCount + 1
                                                else
                                                    item.markPlayCount
                                            }
                                        else
                                            item
                                    )
                                    feed.items
                                }
                            )
                            model.list
                }
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
            ({ model
                | playList = List.filter (\url_ -> url /= url_) model.playList
                , floatPanel = hideItemDropdown model.floatPanel
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


view : Model -> Html Msg
view model =
    let
        feed' = model.list
            |> List.filter (\f -> Just f.url == model.showFeedUrl)
            |> List.head
        filterBar =
            if List.length model.list > 0 then
                div
                    [ class "feed-filter" ]
                    [ filterButton "Unlistened" Unlistened model.itemFilter
                    , filterButton
                        (let
                            playListLen = List.length model.playList
                            playListLen_ = if playListLen > 0 then
                                " " ++ toString playListLen
                                else ""
                        in
                            "Queued" ++ playListLen_
                        )
                        Queued model.itemFilter
                    , filterButton "All" All model.itemFilter
                    ]
            else
                text ""
    in
        div [ class "app-wrap" ]
            [ viewFontSizeStyle model.fontSize
            , viewAddFeed model
            , viewAbout model
            , div
                [ class "wrap"
                , onClick (SetFloatPanel Hidden)
                ]
                <|
                    [ div
                        [ class "top-bar-wrap" ]
                        [ div
                            [ class "top-bar"]
                            [ viewTitle model feed'
                            , filterBar
                            , topLeftBar model
                            ]
                        ]
                    ]
                    ++ (viewItemList model feed')
                    ++ [ viewPlayer model ]
            ]


viewFontSizeStyle : FontSize -> Html Msg
viewFontSizeStyle fontSize =
    node "style"
        [ property "innerHTML" <|
            Json.Encode.string
                ("html{font-size:" ++ getFontSizePx fontSize ++ "}")
        ]
        []


viewTitle : Model -> Maybe Feed -> Html Msg
viewTitle model feed' =
    case feed' of
        Just feed ->
            viewFeedTitle model feed

        Nothing ->
            let
                isRefreshing = model.list
                    |> List.filter (\feed -> feed.state == Refreshing )
                    |> List.length
                    |> (<) 0
            in
                div
                    [ class "feed-header" ]
                    [ addFeedButton
                    , div
                        [ class "feed-title" ]
                        [ if List.length model.list == 0 then
                            span
                                [ class "feed-empty" ]
                                [ text "← Click to add feed" ]
                          else
                            text "All Podcasts"
                        ]
                    , if isRefreshing then
                        div
                            [ class "feed-state" ]
                            [ img [ src "assets/loading-spin.svg" ] []
                            , viewStatus model
                            ]
                      else
                        text ""

                    , if not isRefreshing && List.length model.list > 0 then
                        button
                            [ class "btn btn-icon feed-control feed-refresh"
                            , onClick UpdateAllFeed
                            , title "Refresh all feeds"
                            ]
                            [ img [ src "assets/refresh.svg" ] [] ]
                      else
                          text ""
                    ]


filterButton : String -> ItemFilter -> ItemFilter -> Html Msg
filterButton label filter modelItemFilter =
    button
        [ classList
            [ ("btn btn-text" , True)
            , ("is-active", modelItemFilter == filter)
            ]
        , onClick (SetItemFilter filter)
        ]
        [ text label ]


topLeftBar : Model -> Html Msg
topLeftBar model =
    div
        [ class "top-left-bar" ]
        [ viewAboutButton ]


viewStatus : Model -> Html Msg
viewStatus model =
    let
        txt = model.list
            |> List.filter (\f -> f.state == Refreshing)
            |> List.map (\f -> "Refreshing feed " ++ f.title ++ "...")
            |> List.head
            |> Maybe.withDefault ""
    in
        div
            [ class "feed-status" ]
            [ text txt ]


viewItemList : Model -> Maybe Feed -> List (Html Msg)
viewItemList model feed_ =
    let
        (list, hasMoreItem) = itemList model
        itemList_ =
            case feed_ of
                Just feed ->
                    ul
                        [ class "item-list" ]
                        (list
                            |> List.map snd
                            |> List.indexedMap (,)
                            |> List.map (viewItem model Nothing)
                        )
                Nothing ->
                    if List.length list == 0 && List.length model.list > 0 then
                        div
                            [ class "item-empty" ]
                            [ text "This list is empty." ]
                    else
                        ul
                            [ class "item-list"]
                            ( list
                                |> List.indexedMap (,)
                                |> List.map (\(index, (feed, item)) ->
                                     viewItem model (Just feed) (index, item)
                                  )
                            )

        sortBar =
            if List.length list > 0 then
                viewItemSortLatest model
            else
                Nothing

        showMore =
            if feed_ == Nothing && hasMoreItem then
                div
                    [ class "feed-show-more" ]
                    [ button
                        [ class "btn btn-text"
                        , onClick ShowMoreItem
                        ]
                        [ text "show more"]
                    ]
              else
                  text ""
    in
        [ sortBar |> Maybe.withDefault (text "")
        , div
            [ classList
                [ ("item-list-wrap", True)
                , ("has-sort", sortBar /= Nothing)
                ]
            , onScroll HideItemDropdown
            ]
            [ itemList_
            , showMore
            ]
        ]



viewItemSortLatest : Model -> Maybe (Html Msg)
viewItemSortLatest model =
    if model.itemFilter /= Queued then
        Just <|
            div
                [ class "item-sort" ]
                [ span
                    [ onClick (SetItemSortLatest (not model.itemSortLatest)) ]
                    [ if model.itemSortLatest then
                        text "latest first"
                      else
                        text "oldest first"
                    ]
                ]
    else
        Nothing


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
        , keyUp (keyMap model)
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
