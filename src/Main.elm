port module Main exposing (..)

import Html exposing (div, text, input, Html, span, ul, li, button, img)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, src, style, classList, type', checked)
import Task exposing (Task)
import Time exposing (Time)
import ListUtil exposing (dropWhile, takeWhile)
import Dom
import Dict

import Models exposing (..)
import Msgs exposing (..)
import Feed exposing
    ( loadFeed, updateFeed, updateModelFeed, updateFeedItems
    , viewFeedTitle , viewItem )
import AddFeed exposing (viewAddFeed, addFeedButton)
import Player exposing (viewPlayer)
import Shortcut exposing (keyMap, selectNext, selectPrev)
import Events exposing (onScroll)


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
                { showAddPanel = List.length feeds  == 0
                , urlToAdd = m.urlToAdd
                , list = feeds
                , loadFeedState = Empty
                , currentTime = 0
                , itemsToShow = 30 -- m.itemsToShow
                , currentItemUrl = m.currentItemUrl
                , playerState = Stopped
                , playerRate = m.playerRate
                , playerVol = m.playerVol
                , showFeedUrl = Nothing
                , itemFilter = toItemFilter m.itemFilter
                , itemDropdown = Nothing
                , itemSelected = Nothing
                }
                    ! [ updateCurrentTime
                      , updateFeeds feeds
                      ]
        Nothing ->
            { showAddPanel = False
            , urlToAdd = "" --Maybe.withDefault "" storeModel.url
            , list = []
            , loadFeedState = Empty
            , currentTime = 0
            , itemsToShow = 30
            , currentItemUrl = Nothing
            , playerState = Stopped
            , playerRate = 1
            , playerVol = toFloat 1
            , showFeedUrl = Nothing
            , itemFilter = Unlistened
            , itemDropdown = Nothing
            , itemSelected = Nothing
            }
                ! [ updateCurrentTime ]


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
        cmds = [ updateCurrentTime ]
        (model', cmds') =
            case msg of
                NoOp ->
                    let 
                        a = Debug.log "Noop" "Noop"
                    in
                        (model, [])

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
                        , [ loadFeed model.urlToAdd ] ++ cmds
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
                    }
                    , cmds)

                FetchFeedFail error ->
                    let
                        e = Debug.log "error" error
                    in
                        ({ model | loadFeedState = Error }, cmds)

                UpdateCurrentTime time ->
                    ({ model | currentTime = time }, [])

                Play item ->
                    case item.url of
                        Nothing ->
                            (model, cmds)

                        Just url ->
                            ({ model
                                | currentItemUrl = Just url
                                , playerState = SoundLoading
                            }
                            , [ play
                                { url = url
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
                            (\item -> { item
                                        | duration = progress.duration
                                        , progress = progress.progress })
                    , cmds
                    )

                UpdateFeeds feeds feed ->
                    let
                        model' = updateModelFeed { feed | state = Refreshing } model
                    in
                        (model', [ updateFeed feed feeds ] ++ cmds)

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
                    ( { model | showAddPanel = True }
                    , [ Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus "add-feed") ]
                        ++ cmds
                    )

                HideAddPanel ->
                    ({ model | showAddPanel = False }, cmds)

                ShowFeed url ->
                    ({ model
                        | showFeedUrl = Just url
                        , list = flushPlayCount model.list
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
                        currentItemDeleted = list
                            |> List.concatMap (\feed -> feed.items)
                            |> List.filter (\item -> isCurrent item.url model)
                            |> List.length
                            |> (==) 0
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
                        model' = updateCurrentItem
                                    (\item ->
                                        { item
                                            | progress = 0
                                            , markPlayCount = item.playCount + 1
                                        }
                                    )
                                    model
                        nextItem =
                            itemList model
                                |> fst
                                |> dropWhile (\(feed, item) -> item.url /= Just url)
                                |> List.take 2
                                |> List.reverse
                                |> List.map snd
                                |> List.head
                    in
                        case nextItem of
                            Just item' ->
                                let
                                    (model'', cmd') = update (Play item') model'
                                in
                                    (model'', [cmd'] ++ cmds)

                            Nothing ->
                                ({ model' | currentItemUrl = Nothing }, cmds)

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
                     }
                    , cmds
                    )

                ShowItemDropdown url  ->
                    ({ model | itemDropdown = Just url } , cmds )

                HideItemDropdown ->
                    ({ model | itemDropdown = Nothing } , cmds )

                SelectItem item ->
                    ({ model | itemSelected = item.url } , cmds )

                UnselectItem item ->
                    -- ({ model | itemDropdown = Nothing } , cmds)
                    (model, cmds)

                MarkPlayCount url playCount ->
                    let
                        model' = updateItem
                            (\item -> { item | markPlayCount = playCount })
                            (Just url)
                            model
                        model'' = { model' | itemDropdown = Nothing }
                    in
                        (model'', cmds)

                MarkItemsBelowListened url ->
                    let
                        toUpdate =
                            Dict.fromList
                                (itemList model
                                    |> fst
                                    |> List.filterMap (\(feed, item) -> item.url)
                                    |> dropWhile (\url' -> url' /= url)
                                    |> List.map (\url' -> (url', True))
                                )
                        model' =
                            { model
                                | itemDropdown = Nothing
                                , list = List.map
                                    (\feed ->
                                        { feed | items = List.map
                                            (\item ->
                                                case item.url of
                                                    Just url' ->
                                                        if Dict.member url' toUpdate then
                                                            { item | markPlayCount =
                                                                if item.markPlayCount == -1 then
                                                                    item.playCount + 1
                                                                else
                                                                    item.markPlayCount
                                                            }
                                                        else
                                                            item
                                                    Nothing ->
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
                    let
                        (model', cmd) = selectNext model
                    in
                        (model', [cmd] ++ cmds)

                SelectPrev ->
                    let
                        (model', cmd) = selectPrev model
                    in
                        (model', [cmd] ++ cmds)
                    -- (selectPrev model, cmds)
    in
        model' ! (cmds' ++ [ saveModel model ] )


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
                    , filterButton "Listening" Listening model.itemFilter
                    , filterButton "All" All model.itemFilter
                    ]
            else
                text ""
    in
        div [ class "app-wrap" ]
            [ viewAddFeed model
            -- , viewItemDropDown model.itemDropdown
            , div
                [ class "wrap"
                , onClick HideAddPanel
                ]
                [ div
                    [ class "top-bar-wrap" ]
                    [ div
                        [ class "top-bar"]
                        [ viewTitle model feed'
                        , filterBar
                        , viewStatus model
                        ]
                    ]
                , viewItemList model feed'
                , viewPlayer model
                ]
            ]


viewTitle : Model -> Maybe Feed -> Html Msg
viewTitle model feed' =
    case feed' of
        Just feed ->
            viewFeedTitle model feed

        Nothing ->
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
            [ class "app-status" ]
            [ text txt ]


viewItemList : Model -> Maybe Feed -> Html Msg
viewItemList model feed' =
    let
        (list, hasMoreItem) = itemList model
    in
        case feed' of
            Just feed ->
                div
                    [ class "item-list-wrap" 
                    , onScroll HideItemDropdown
                    ]
                    [ ul [ class "item-list" ]
                        ( list
                            |> List.map snd
                            |> List.indexedMap (,)
                            |> List.map (viewItem model Nothing)
                        )
                    ]

            Nothing ->
                let
                    itemList' =
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
                    showMore =
                        if hasMoreItem then
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
                    div
                        [ class "item-list-wrap" 
                        , onScroll HideItemDropdown
                        ]
                        [ itemList'
                        , showMore
                        ]


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


saveModel : Model -> Cmd Msg
saveModel model =
    model |> toStoreModel |> storeModel


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
