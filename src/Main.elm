port module Main exposing (..)

import Html exposing (div, text, input, Html, span, ul, li, button, img)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, src, style, classList)
import Task exposing (Task)
import Time exposing (Time)
import ListUtil exposing (dropWhile)
import Dom

import Models exposing (..)
import Msgs exposing (..)
import Feed exposing
    ( loadFeed, updateFeed, updateModelFeed, updateFeedItems
    , showMore, resetShowMore, viewFeed, hideItemsUnder )
import AddFeed exposing (viewAddFeed, addFeedButton)
import Player exposing (viewPlayer)


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
                , itemsToShow = m.itemsToShow
                , currentItemUrl = m.currentItemUrl
                , playerState = Stopped
                , playerRate = m.playerRate
                , playerVol = m.playerVol
                , playerMute = m.playerMute
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
            , itemsToShow = 10
            , currentItemUrl = Nothing
            , playerState = Stopped
            , playerRate = 1
            , playerVol = toFloat 1
            , playerMute = False
            }
                ! [ updateCurrentTime ]


toFeed : StoreFeed -> Feed
toFeed storeFeed =
    { url = storeFeed.url
    , title = storeFeed.title
    , items = storeFeed.items
    , state = Normal
    , showConfirmDelete = False
    }


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

                ShowMoreItem feed ->
                    ( updateModelFeed { feed | items = showMore model.itemsToShow feed.items } model
                    , cmds
                    )

                FetchFeedSucceed feed ->
                    ({ model
                        | list =
                            model.list
                                ++ [ { feed | items = showMore model.itemsToShow feed.items } ]
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
                                , seek = item.progress.current
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
                        |> updateCurrentItem (\item -> { item | progress = progress })
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

                SetProgress percentage ->
                    let
                        a = Debug.log "percentage" percentage
                    in
                        case getCurrentItem model of
                            Nothing ->
                                ( model, cmds )
                            Just item' ->
                                let
                                    current = item'.progress.duration * percentage
                                    progress = item'.progress
                                    progress' = { progress | current = current }
                                    model' = model |>
                                        updateCurrentItem (\item -> { item | progress = progress' })
                                in
                                    (model' , [seek current] ++ cmds)

                ShowAddPanel ->
                    ( { model | showAddPanel = True }
                    , [ Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus "add-feed") ]
                        ++ cmds
                    )

                HideAddPanel ->
                    ({ model | showAddPanel = False }, cmds)

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
                                        let
                                            progress = item.progress
                                            progress' = { progress | current = 0 }
                                        in
                                            { item
                                                | progress = progress'
                                                , playCount = item.playCount + 1
                                            }
                                    )
                                    model
                        nextItem = model.list
                             |> List.concatMap (\feed -> feed.items)
                             |> List.filter (\item -> item.show )
                             |> dropWhile (\item -> (Maybe.withDefault "" item.url) /= url)
                             |> List.take 2
                             |> List.reverse
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

                HideAllUnder item ->
                    let
                        list = List.map
                                (\feed -> { feed | items = hideItemsUnder item feed.items })
                                model.list
                        isCurrentItem =  isCurrent item.url model
                        currentItemUrl =
                            if isCurrentItem then
                                Nothing
                            else
                                model.currentItemUrl
                        cmds' =
                            if isCurrentItem then
                                [ stop "" ]
                            else
                                []
                    in
                        ( { model | list = list, currentItemUrl = currentItemUrl }
                        , cmds' ++ cmds
                        )

                TogglePlayerMute ->
                    let
                        muted = not model.playerMute
                    in
                        ({ model | playerMute = muted }
                        , [setMute muted] ++ cmds)

                SetVol percentage ->
                    ({ model | playerVol = percentage}
                    , [setVol percentage] ++ cmds)

    in
        model' ! (cmds' ++ [ saveModel model ] )




view : Model -> Html Msg
view model =
    div [ class "app-wrap" ]
        [ viewAddFeed model
        , div
            [ class "wrap"
            , onClick HideAddPanel
            ]
            [ div
                [ class "top-control-bar"]
                [ addFeedButton
                ]
            , ul [ class "feed" ]
                (model.list
                    |> List.sortWith
                        (\feed1 feed2 ->
                            let
                                getTime = (\feed ->
                                    ((List.head feed.items)
                                        `Maybe.andThen`
                                        (\f -> Just f.pubDate)
                                    ) |> Maybe.withDefault -1
                                )
                            in
                                compare (getTime feed2) (getTime feed1)
                        )
                    |> List.map (viewFeed model)
                )
            , viewPlayer model
            ]
        ]


saveModel : Model -> Cmd Msg
saveModel model =
    storeModel
        { urlToAdd = model.urlToAdd
        , list = List.map saveFeed model.list
        , itemsToShow = model.itemsToShow
        , currentItemUrl = model.currentItemUrl
        , playerRate = model.playerRate
        , playerVol = model.playerVol
        , playerMute = model.playerMute
        }


saveFeed : Feed -> StoreFeed
saveFeed feed =
    { url = feed.url
    , title = feed.title
    , items = feed.items
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ updateProgress UpdateProgress
        , soundLoaded SoundLoaded
        , playEnd PlayEnd
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
port setMute : Bool -> Cmd msg
