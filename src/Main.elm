port module Main exposing (..)

import Html exposing (div, text, input, Html, span, ul, li, button, img)
import Html.App as App
import Html.Events exposing (onInput, on, keyCode, onClick, onWithOptions)
import Html.Attributes exposing (value, placeholder, class, title, src, style, classList, id)
import Json.Decode as Json exposing ((:=))
import Http
import String
import Task exposing (Task)
import Time exposing (Time)
import Models exposing (..)
import DecodeFeed exposing (decodeFeed)
import DateFormat exposing (format, formatDuration)
import ListUtil exposing (takeWhile, dropWhile)
import DecodePosition exposing (decodeLeftPercentage)
import Dom


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


type Msg
    = NoOp
    | SetUrl String
    | AddFeed
    | FetchFeedSucceed Feed
    | FetchFeedFail Http.Error
    | UpdateCurrentTime Time
    | ShowMoreItem String
    | Play Item
    | SoundLoaded Bool
    | Pause Item
    | Stop Item
    | UpdateProgress Progress
    | UpdateFeeds (List Feed) Feed
    | UpdateFeedFail (List Feed) Feed Http.Error
    | UpdateFeedSucceed (List Feed) Feed
    | SetProgress Float
    | ShowAddPanel
    | HideAddPanel
    | ShowConfirmDeleteFeed Feed
    | HideConfirmDeleteFeed Feed
    | ConfirmDeleteFeed Feed
    | ClosePlayer
    | PlayEnd String
    | ToggleRate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        cmds = [ updateCurrentTime ]
        (model', cmds') =
            case msg of
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
                            , [ loadFeed model.urlToAdd ] ++ cmds)

                ShowMoreItem feedUrl ->
                    ({ model
                        | list =
                            List.map
                                (\feed ->
                                    if feed.url == feedUrl then
                                        { feed | items = showMore model.itemsToShow feed.items }
                                    else
                                        feed
                                )
                                model.list
                    }
                    , cmds)

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
                                }
                              ] ++ cmds)

                SoundLoaded loaded ->
                    ({ model | playerState = Playing } , cmds)

                Pause item ->
                    ({ model | playerState = Paused } , [ pause  "" ] ++ cmds)

                Stop item ->
                    ({ model
                        | playerState = Stopped
                    }
                    , [ stop "" ] ++ cmds)

                UpdateProgress progress ->
                    ( model
                        |> updateCurrentItem (\item -> { item | progress = progress })
                    , cmds
                    )

                UpdateFeeds feeds feed ->
                    let
                        model' = updateModelFeed model feed (\feed' -> { feed' | state = Refreshing } )
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
                        (updateModelFeed model feed (\f -> { f | state = RefreshError })
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
                    ({ model | showAddPanel = True }
                    , [ Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus "add-feed") ]
                        ++ cmds)

                HideAddPanel ->
                    ({ model | showAddPanel = False }, cmds)

                ShowConfirmDeleteFeed feed ->
                    ( updateModelFeed model feed (\f -> { f | showConfirmDelete = True })
                    , cmds
                    )

                HideConfirmDeleteFeed feed ->
                    ( updateModelFeed model feed (\f -> { f | showConfirmDelete = False })
                    , cmds
                    )

                ConfirmDeleteFeed feed ->
                    let
                        list = List.filter (\f -> f.url /= feed.url ) model.list
                        currentItemDeleted = list
                            |> List.concatMap (\feed -> feed.items)
                            |> List.filter (\item -> isCurrent item.url model.currentItemUrl)
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
                    ({ model
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
                                            { item | progress = progress' }
                                    )
                                    model
                        nextItem = model.list
                             |> List.concatMap (\feed -> feed.items)
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

                NoOp ->
                    (model, [])

    in
        model' ! (cmds' ++ [ saveModel model ] )


updateCurrentItem : (Item -> Item) -> Model -> Model
updateCurrentItem updater model =
    { model | list =
        List.map (\feed ->
            { feed | items =
                List.map (\item ->
                    if isCurrent item.url model.currentItemUrl then
                        updater item
                    else
                        item
                )
                feed.items
            }
        )
        model.list
    }


updateModelFeed : Model -> Feed -> (Feed -> Feed) -> Model
updateModelFeed model feed update =
    let
        list = List.map (\f ->
            if feed.url == f.url then
                update f
            else
                f
        ) model.list
    in
        { model | list = list }


updateFeedItems : Model -> Feed -> Model
updateFeedItems model feed =
    updateModelFeed model feed
        (\f ->
            let
                head = List.head f.items
                newItems =
                    case head of
                        Nothing ->
                            feed.items

                        Just head' ->
                            feed.items
                                |> takeWhile (\item ->
                                    case Maybe.map2 (/=) item.url head'.url of
                                        Nothing -> False
                                        Just notEqual -> notEqual
                                )
                                |> resetShowMore model.itemsToShow
            in
                { f
                    | items = newItems ++ f.items
                    , state =
                        if List.length newItems > 0 then
                            HasNewItem
                        else
                            Normal
                }
        )


showMore : Int -> List Item -> List Item
showMore num list =
    let
        ( list1, list2 ) =
            List.partition (\item -> item.show) list

        list2' =
            (List.take num list2
                |> List.map (\item -> { item | show = True })
            )
                ++ List.drop num list2
    in
        list1 ++ list2'


resetShowMore : Int -> List Item -> List Item
resetShowMore num list =
    (List.take num list
        |> List.map (\item -> { item | show = True })
    )
    ++
    (List.drop num list
        |> List.map (\item -> { item | show = False })
    )


isCurrent : Maybe String -> Maybe String -> Bool
isCurrent itemUrl currentUrl =
    case itemUrl of
        Nothing -> False
        Just itemUrl' ->
            case currentUrl of
                Nothing -> False
                Just currentUrl' ->
                    itemUrl' == currentUrl'



view : Model -> Html Msg
view model =
    div []
        [ div
            [ classList
                [ ("add-panel", True)
                , ("is-show", model.showAddPanel)
                ]
            ]
            [ button
                [ class "btn add-close"
                , onClick HideAddPanel
                ]
                [ img [ src "assets/close.svg"] [] ]
            , input
                [ id "add-feed"
                , class "add-feed"
                , onInput SetUrl
                , onEnter AddFeed
                , value model.urlToAdd
                , placeholder "Add Feed"
                ]
                []
            , viewLoadFeedState model.loadFeedState
            ]
        , div
            [ class "wrap"
            , onClick HideAddPanel
            ]
            [ div
                [ class "top-control-bar"]
                [ button
                    [ class "btn add-btn"
                    , onInternalClick ShowAddPanel
                    ]
                    [ text "+" ]
                ]
            , ul [ class "feed" ] <|
                List.map (viewFeed model) model.list
            , viewPlayer model
            ]
        ]


viewLoadFeedState : LoadFeedState -> Html Msg
viewLoadFeedState state =
    div [ class "add-feed-state" ]
        [ if state == Loading then
            span [] [ text "Loading feed..." ]
          else if state == Error then
            span [] [ text "Problem loading feed" ]
          else if state == AlreadyExist then
            span [] [ text "The feed is added already." ]
          else
            -- span [] [ text "Loading feed" ]
            text ""
        ]


viewFeed : Model -> Feed -> Html Msg
viewFeed model feed =
    let
        items = List.filter (\item -> item.show) feed.items
        feedState =
            case feed.state of
                Refreshing ->
                    span [ class "feed-state" ]
                        [ img [src  "assets/loading-spin.svg" ] [] ]
                RefreshError ->
                    span
                        [ class "feed-state feed-state-error", title "Error in updating feed" ]
                        [ img [ src "assets/exclamation.svg" ] [] ]
                _ ->
                    text ""
        refreshBtn =
            case feed.state of
                Refreshing -> text ""
                _ ->
                    button
                        [ class "btn feed-control feed-refresh"
                        , onClick (UpdateFeeds [] feed) ]
                        [ img [ src "assets/refresh.svg" ] [] ]
            -- span [ class "feed-state" ] [ text "*" ]
    in
        li []
            [ div [ class "feed-header" ]
                [ span [ class "feed-title" ] [ text feed.title ]
                , feedState
                , refreshBtn
                , button
                    [ classList
                        [ ("btn feed-control feed-trash", True) ]
                    , onClick (ShowConfirmDeleteFeed feed)
                    ]
                    [ img [ src "assets/trash.svg"] [] ]
                , if feed.showConfirmDelete then
                    div [ class "confirm-delete feed-control" ]
                        [ span [] [ text "Delete?" ]
                        , button
                            [ class "btn btn-text"
                            , onClick (ConfirmDeleteFeed feed)
                            ]
                            [ text "Yes "]
                        , button
                            [ class "btn btn-text"
                            , onClick (HideConfirmDeleteFeed feed)
                            ]
                            [ text "No" ]
                        ]
                  else
                      text ""
                -- , span [ class "feed-state" ] [ text feedState ]
                ]
            , ul [ class "item-list" ] <|
                (List.map (viewItem model) items)
                ++
                    if List.length items < List.length feed.items then
                        [ li [ class "item item-more"]
                            [ button
                                [ class "feed-show-more"
                                , onClick (ShowMoreItem feed.url)
                                ]
                                [ text "...more" ]
                            ]
                        ]
                    else
                      []
            ]


viewItem : Model -> Item -> Html Msg
viewItem model item =
    li
        [ classList
            [ ("item", True)
            , ("is-current", isCurrent item.url model.currentItemUrl)
            , ("is-error", item.url == Nothing)
            , ("is-unplayed", item.progress.current == -1)
            ]
        , toggleItem model item
        ]
        [ renderItemState item model.currentItemUrl model.playerState
        , div [ class "item-desp" ]
            [ div
                [ class "item-title", title item.title ]
                [ text item.title ]
            ]
        , div [ class "item-date" ]
            [ text <| format item.pubDate model.currentTime ]
        , div [ class "item-progress" ]
            [ text <| formatDuration item.progress.duration ]
        ]


toggleItem : Model -> Item -> Html.Attribute Msg
toggleItem model item =
    if isCurrent item.url model.currentItemUrl then
        if model.playerState == Playing then
            onClick (Pause item)
        else
            onClick (Play item)
    else
        onClick (Play item)


renderItemState: Item -> Maybe String -> PlayerState -> Html Msg
renderItemState item currentItemUrl playerState =
    if isCurrent item.url currentItemUrl then
        div
            [ class "item-state" ]
            [ if playerState == SoundLoading then
                img [ src "assets/loading-spin.svg" ] []
              else if playerState == Playing then
                img [ src "assets/equalizer-playing.svg" ] []
              else
                img [ src "assets/equalizer-stop.svg" ] []
            ]
    else
        case item.url of
            Just url' ->
                div
                    [ class "item-state" ]
                    [ img [ src "assets/play.svg" ] []
                    ]

            Nothing ->
                 div
                    [ class "item-state item-not-found"
                    , title "no file found "
                    ]
                    [ text "!" ]


progressBar : Progress -> Html Msg
progressBar p =
    if p.duration == -1 then
        text ""
    else
        div
            [ class "progress-bar"
            , onSetProgress SetProgress
            ]
            [ div
                [ class "progress-bar-value"
                , style [("width", toString (p.current * 100 / p.duration) ++ "%" )]
                ]
                []
            ]


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    on "keydown" <|
        Json.map
            (\code ->
                if code == 13 then
                    msg
                else
                    NoOp
            )
            keyCode


onSetProgress : (Float -> Msg) -> Html.Attribute Msg
onSetProgress tagger =
    on "mouseup" <|
        Json.map tagger decodeLeftPercentage


onInternalClick : Msg -> Html.Attribute Msg
onInternalClick msg =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.succeed msg)


viewPlayer : Model -> Html Msg
viewPlayer model =
    let
        item = getCurrentItem model
        playerClass = if item /= Nothing then "is-show" else ""
    in
        div
            [ class <| "player-wrap "  ++ playerClass ]
            [ div
                [ class "player"] <|
                case item of
                    Just item' ->
                        [ div
                            [ class "player-control "]
                            [ div
                                [ class "player-buttons" ]
                                [
                                    if model.playerState == SoundLoading then
                                        div [ class "btn player-btn" ]
                                            [ img [ src "assets/loading-spin.svg" ] [] ]
                                    else if (model.playerState == Stopped || model.playerState == Paused) then
                                        button
                                            [ class "btn player-btn"
                                            , onClick (Play item')
                                            ]
                                            [ img [ src "assets/play.svg" ] [] ]
                                    else
                                        button
                                            [ class "btn player-btn"
                                            , onClick (Pause item')
                                            ]
                                            [ img [ src "assets/pause.svg" ] [] ]
                                ]
                            , div [ class "progress" ]
                                [ div
                                    [ class "player-title" ]
                                    [ marquee item'.title (model.playerState == Playing)
                                    ]
                                , progressBar item'.progress
                                ]
                            , div [ class "player-rate" ]
                                [ button
                                    [ class "btn"
                                    , onClick ToggleRate
                                    ]
                                    [ text <| (toString model.playerRate) ++ "X" ]
                                ]
                            , div
                                [ class "player-progress" ]
                                [ text <|
                                    formatDuration item'.progress.current
                                    ++ "/"
                                    ++ formatDuration item'.progress.duration
                                ]
                            , div
                                [ class "player-close" ]
                                [ button
                                    [ class "btn"
                                    , onClick ClosePlayer
                                    ]
                                    [ img [ src "assets/close.svg"] [] ]
                                ]
                            -- , div
                            --     [ class "player-title "]
                            --     [ text item'.title ]
                            ]
                        ]
                    Nothing ->
                        [ text "" ]

            ]


showIf : Bool -> Html Msg -> Html Msg
showIf flag html =
    if flag then
        html
    else
        text ""


getCurrentItem : Model -> Maybe Item
getCurrentItem model =
    model.list
        |> List.concatMap (\feed -> feed.items)
        |> List.filter (\item ->
                Maybe.map2 (==) item.url model.currentItemUrl
                    |> Maybe.withDefault False
            )
        |> List.head


marquee : String -> Bool -> Html Msg
marquee txt isPlaying =
    div
        [ class "player-title-text" ]
        [ div
            [ classList
                [ ("marquee-text", True)
                , ("is-playing", isPlaying) ] ]
            [ text txt ]
        ]


yqlUrl: String -> String -- Task Http.Error Feed
yqlUrl url =
    String.join ""
        [ "//query.yahooapis.com/v1/public/yql?q="
        , Http.uriEncode ("select * from xml where url = \"" ++ url ++ "\" ")
        , "&format=json"
        ]


loadFeed : String -> Cmd Msg
loadFeed url =
    Http.get (decodeFeed url) (yqlUrl url)
        |> Task.perform FetchFeedFail FetchFeedSucceed


updateFeed : Feed -> List Feed -> Cmd Msg
updateFeed feed feeds =
    Http.get (decodeFeed feed.url) (yqlUrl feed.url)
        |> Task.perform
            (UpdateFeedFail feeds feed)
            (UpdateFeedSucceed feeds)


saveModel : Model -> Cmd Msg
saveModel model =
    storeModel
        { urlToAdd = model.urlToAdd
        , list = List.map saveFeed model.list
        , itemsToShow = model.itemsToShow
        , currentItemUrl = model.currentItemUrl
        , playerRate = model.playerRate
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
