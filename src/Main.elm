port module Main exposing (..)

import Html exposing (div, text, input, Html, span, ul, li, button, img)
import Html.App as App
import Html.Events exposing (onInput, on, keyCode, onClick)
import Html.Attributes exposing (value, placeholder, class, title, src, style, classList)
import Json.Decode as Json exposing ((:=))
import Http
import String
import Task exposing (Task)
import Time exposing (Time)
import Models exposing (..)
import DecodeFeed exposing (decodeFeed)
import DateFormat exposing (format, formatDuration)
import ListUtil exposing (takeWhile)
import DecodePosition exposing (decodeLeftPercentage)


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
                { urlToAdd = m.urlToAdd
                , list = feeds
                , loadFeedState = Empty
                , currentTime = 0
                , itemsToShow = m.itemsToShow
                , currentItemUrl = m.currentItemUrl
                , playerState = Stopped
                }
                    ! [ updateCurrentTime
                      , updateFeeds feeds
                      ]
        Nothing ->
            { urlToAdd = "" --Maybe.withDefault "" storeModel.url
            , list = []
            , loadFeedState = Empty
            , currentTime = 0
            , itemsToShow = 10
            , currentItemUrl = Nothing
            , playerState = Stopped
            }
                ! [ updateCurrentTime ]


toFeed : StoreFeed -> Feed
toFeed storeFeed =
    { url = storeFeed.url
    , title = storeFeed.title
    , items = storeFeed.items
    , state = Normal
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
    | Pause Item
    | Stop Item
    | UpdateProgress Progress
    | UpdateFeeds (List Feed) Feed
    | UpdateFeedFail (List Feed) Feed Http.Error
    | UpdateFeedSucceed (List Feed) Feed
    | SetProgress Float


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
                                , playerState = Playing
                            }
                            , [ play { url = url, seek = item.progress.current} ] ++ cmds)

                Pause item ->
                    ({ model | playerState = Paused } , [ pause  "" ] ++ cmds)

                Stop item ->
                    ({ model
                        | playerState = Stopped
                    }
                    , [ stop "" ] ++ cmds)

                UpdateProgress progress ->
                    let
                        list =
                            List.map (\feed ->
                                { feed | items =
                                    List.map (\item ->
                                        if isCurrent item.url model.currentItemUrl then
                                            { item | progress = progress }
                                        else
                                            item
                                    )
                                    feed.items
                                }
                            )
                            model.list
                    in
                        ({ model | list = list }, cmds)

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
                        ( updateModelFeed model feed (\f -> { f | state = RefreshError })
                        , cmds' ++ cmds)

                UpdateFeedSucceed feeds feed ->
                    let
                        cmds' =
                            if List.length feeds > 0 then
                                [ updateFeeds feeds ]
                            else
                                []
                    in
                        ( updateFeedItems model feed, cmds' ++ cmds)

                SetProgress percentage ->
                    case getCurrentItem model of
                        Nothing ->
                            (model, cmds)

                        Just item' ->
                            (model, [seek (item'.progress.duration * percentage) ])

                NoOp ->
                    (model, [])

    in
        model' ! (cmds' ++ [ saveModel model ] )


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
    div [ class "wrap" ]
        [ div []
            [ input
                [ class "add-feed"
                , onInput SetUrl
                , onEnter AddFeed
                , value model.urlToAdd
                , placeholder "Add Feed"
                ]
                []
            , viewLoadFeedState model.loadFeedState
            ]
        , ul [ class "feed" ] <|
            List.map (viewFeed model) model.list
        , viewPlayer model
        ]


viewLoadFeedState : LoadFeedState -> Html Msg
viewLoadFeedState state =
    div []
        [ if state == Loading then
            span [] [ text "Loading feed..." ]
          else if state == Error then
            span [] [ text "Problem loading feed" ]
          else if state == AlreadyExist then
            span [] [ text "The feed is added already." ]
          else
            text ""
        ]


viewFeed : Model -> Feed -> Html Msg
viewFeed model feed =
    let
        items = List.filter (\item -> item.show) feed.items
        feedState =
            case feed.state of
                Normal ->
                    text ""
                Refreshing ->
                    span [ class "feed-state" ]  [ div [ class "loading dots" ] [] ]
                RefreshError ->
                    span
                        [ class "feed-state feed-state-error", title "Error in updating feed" ]
                        [ img [ src "assets/exclamation.svg" ] [] ]
                HasNewItem ->
                    span [ class "feed-state" ] [ text "*" ]
    in
        li []
            [ div [ class "feed-header" ]
                [ span [ class "feed-title" ] [ text feed.title ]
                , feedState
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
            , ("is-unplayed", item.progress.duration == -1)
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
            [ if playerState == Playing then
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
                                [ showIf (model.playerState == Stopped || model.playerState == Paused) <|
                                    button
                                        [ class "player-btn"
                                        , onClick (Play item')
                                        ]
                                        [ img [ src "assets/play.svg" ] [] ]
                                , showIf (model.playerState == Playing) <|
                                    div []
                                        [ button
                                            [ class "player-btn"
                                            , onClick (Pause item')
                                            ]
                                            [ img [ src "assets/pause.svg" ] [] ]
                                        -- , button
                                        --     [ class "player-btn"
                                        --     , onClick (Stop item')
                                        --     ]
                                        --     [ img [ src "assets/stop.svg" ] [] ]
                                        ]
                                ]
                            , div [ class "progress" ]
                                [ div
                                    [ class "player-title" ]
                                    [ marquee item'.title (model.playerState == Playing)
                                    ]
                                , progressBar item'.progress
                                ]
                            , div
                                [ class "player-progress" ]
                                [ text <|
                                    formatDuration item'.progress.current
                                    ++ "/"
                                    ++ formatDuration item'.progress.duration
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
        }


saveFeed : Feed -> StoreFeed
saveFeed feed =
    { url = feed.url
    , title = feed.title
    , items = feed.items
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    updateProgress UpdateProgress


port play : PlayLoad -> Cmd msg
port stop : String -> Cmd msg
port pause : String -> Cmd msg
port updateProgress : (Progress -> msg) -> Sub msg
port storeModel : StoreModel -> Cmd msg
port seek : Float -> Cmd msg
