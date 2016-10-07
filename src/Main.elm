port module Main exposing (..)

import Html exposing (div, text, input, Html, span, ul, li, button, img)
import Html.App as App
import Html.Events exposing (onInput, on, keyCode, onClick)
import Html.Attributes exposing (value, placeholder, class, title, src, style)
import Json.Decode as Json exposing ((:=))
import Http
import String
import Task exposing (Task)
import Time exposing (Time)
import Models exposing (..)
import DecodeFeed exposing (decodeFeed)
import DateFormat exposing (format, formatDuration)


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
            { urlToAdd = m.urlToAdd
            , list = m.list
            , loadFeedState = Empty
            , currentTime = 0
            , itemsToShow = m.itemsToShow
            , currentItemUrl = m.currentItemUrl
            , playerState = Stopped
            }
                ! [ updateCurrentTime ]
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



updateCurrentTime : Cmd Msg
updateCurrentTime =
    Task.perform (\_ -> NoOp) UpdateCurrentTime Time.now


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
    | UpdateProgress Progress


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
                            , [ play url ] ++ cmds)

                Pause item ->
                    ({ model | playerState = Paused } , [ pause  "" ] ++ cmds)

                UpdateProgress progress ->
                    let
                        list = List.map
                            (\feed ->
                                { feed | items = List.map
                                    (\item ->
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

                NoOp ->
                    (model, [])

    in
        model' ! (cmds' ++ [ saveModel model ] )


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
        items =
            List.filter (\item -> item.show) feed.items
    in
        li []
            [ div [ class "feed-title" ] [ text feed.title ]
            , ul [] <|
                List.map (viewItem model) items
            , if List.length items < List.length feed.items then
                button
                    [ class "feed-show-more"
                    , onClick (ShowMoreItem feed.url)
                    ]
                    [ text "...more" ]
              else
                text ""
            ]


viewItem : Model -> Item -> Html Msg
viewItem model item =
    li
        [ class ("item" ++ currentItemClass (isCurrent item.url model.currentItemUrl))
        ]
        [ div [ class "item-desp" ]
            [ renderControl item model.currentItemUrl model.playerState
            , span
                [ class "item-title", title item.title ]
                [ viewProgress item.progress
                , progressBar item.progress
                , text item.title
                ]
            , span [ class "item-date" ]
                [ text <| format item.pubDate model.currentTime
                ]
            ]
        -- , div []
        --     [ text <|
        --         case item.enclosure of
        --             Just enclosure ->
        --                 enclosure.url
        --
        --             Nothing ->
        --                 "not found"
        --     ]
        ]


currentItemClass : Bool -> String
currentItemClass flag =
    if flag then
        " is-current"
    else
        ""


renderControl: Item -> Maybe String -> PlayerState -> Html Msg
renderControl item currentItemUrl playerState =
    span
        [ class "item-control" ]
        [ case item.url of
            Just url ->
                case currentItemUrl of
                    Nothing ->
                        controlButton (Play item) "assets/play.svg"

                    Just currentItemUrl'->
                        if url == currentItemUrl' && playerState == Playing then
                            controlButton (Pause item) "assets/pause.svg"
                        else
                            controlButton (Play item) "assets/play.svg"

            Nothing ->
                 div
                    [ class "item-control-btn item-not-found"
                    , title "no file found "
                    ]
                    [ text "!" ]
        ]


viewProgress : Progress -> Html Msg
viewProgress progress =
    if progress.duration == -1 then
        text ""
    else
        span
            [ class "item-progress" ]
            [ text <| formatDuration progress.current
                ++ "/"
                ++ formatDuration progress.duration
            ]


progressBar : Progress -> Html Msg
progressBar p =
    if p.duration == -1 then
        text ""
    else
        div
            [ class "progress-bar" ]
            [ div
                [ class "progress-bar-value"
                , style [("width", toString (p.current * 100 / p.duration) ++ "%" )]
                ]
                []
            ]


controlButton : Msg -> String -> Html Msg
controlButton msg url =
    button
        [ class "item-control-btn"
        , onClick msg
        ]
        [ img [ src url ] []
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


loadFeed : String -> Cmd Msg
loadFeed url =
    let
        req =
            String.join ""
                [ "//query.yahooapis.com/v1/public/yql?q="
                , Http.uriEncode ("select * from xml where url = \"" ++ url ++ "\" ")
                , "&format=json"
                ]
    in
        req
            |> Http.get (decodeFeed url)
            |> Task.perform FetchFeedFail FetchFeedSucceed


saveModel : Model -> Cmd Msg
saveModel model =
    storeModel
        { urlToAdd = model.urlToAdd
        , list = model.list
        , itemsToShow = model.itemsToShow
        , currentItemUrl = model.currentItemUrl
        }



subscriptions : Model -> Sub Msg
subscriptions model =
    updateProgress UpdateProgress


port play : String -> Cmd msg
port stop : String -> Cmd msg
port pause : String -> Cmd msg
port updateProgress : (Progress -> msg) -> Sub msg
port storeModel : StoreModel -> Cmd msg
