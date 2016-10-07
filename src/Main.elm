port module Main exposing (..)

import Html exposing (div, text, input, Html, span, ul, li, button, img)
import Html.App as App
import Html.Events exposing (onInput, on, keyCode, onClick)
import Html.Attributes exposing (value, placeholder, class, title, src)
import Json.Decode as Json exposing ((:=))
import Http
import String
import Task exposing (Task)
import Time exposing (Time)
import Models exposing (..)
import DecodeFeed exposing (decodeFeed)
import DateFormat exposing (format)


main : Program (Maybe String)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe String -> ( Model, Cmd Msg )
init url =
    { urlToAdd = Maybe.withDefault "" url
    , list = []
    , loadFeedState = Empty
    , currentTime = 0
    , itemsToShow = 10
    , currentItem = Nothing
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        cmds =
            [ updateCurrentTime ]
    in
        case msg of
            SetUrl value ->
                { model
                    | urlToAdd = value
                    , loadFeedState = Empty
                }
                    ! cmds

            AddFeed ->
                if List.any (\feed -> feed.url == model.urlToAdd) model.list then
                    { model | loadFeedState = AlreadyExist } ! cmds
                else
                    { model | loadFeedState = Loading }
                        ! ([ loadFeed model.urlToAdd ] ++ cmds)

            ShowMoreItem feedUrl ->
                { model
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
                    ! cmds

            FetchFeedSucceed feed ->
                { model
                    | list =
                        model.list
                            ++ [ { feed | items = showMore model.itemsToShow feed.items } ]
                    , loadFeedState = Empty
                    , urlToAdd = ""
                }
                    ! cmds

            FetchFeedFail error ->
                let
                    e = Debug.log "error" error
                in
                    { model | loadFeedState = Error } ! []

            UpdateCurrentTime time ->
                { model | currentTime = time } ! []

            Play item ->
                case item.enclosure of
                    Nothing ->
                        model ! []

                    Just file ->
                        { model | currentItem = Just item } ! [ play file.url ]

            NoOp ->
                model ! []


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
            List.map (viewFeed model.currentTime model.currentItem) model.list
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


viewFeed : Time -> Maybe Item -> Feed -> Html Msg
viewFeed currentTime currentItem feed =
    let
        items =
            List.filter (\item -> item.show) feed.items
    in
        li []
            [ div [ class "feed-title" ] [ text feed.title ]
            , ul [] <| List.map (viewItem currentTime currentItem) items
            , if List.length items < List.length feed.items then
                button
                    [ class "feed-show-more"
                    , onClick (ShowMoreItem feed.url)
                    ]
                    [ text "...more" ]
              else
                text ""
            ]


viewItem : Time -> Maybe Item -> Item -> Html Msg
viewItem currentTime currentItem item =
    li [ class "item" ]
        [ div [ class "item-desp" ]
            [ span [ class "item-control" ]
                [ playButton item currentItem
                ]
            , span [ class "item-title", title item.title ] [ text item.title ]
            , span [ class "item-date" ] [ text <| format item.pubDate currentTime ]
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


playButton: Item -> Maybe Item -> Html Msg
playButton item currentItem =
    case item.enclosure of
        Just enclosure ->
            button
                [ class "item-control-btn"
                , onClick (Play item)
                ]
                [ case currentItem of
                    Nothing ->
                        img [ src "assets/play.svg" ] []

                    Just currentItem' ->
                        if item == currentItem' then
                            img [ src "assets/stop.svg" ] []
                          else
                            img [ src "assets/play.svg" ] []
                ]

        Nothing ->
             div
                [ class "item-control-btn"
                , title "no file found "
                ]
                [ text "!" ]


-- isCurrent: Item -> Item -> Bool
-- isCurrent item1 item2 =
--     let
--         id1 = Maybe.oneOf [item1.link, item1.enclosure]
--             |> Maybe.withDefault item1.title
--
--         id2 = Maybe.oneOf [item2.link, item2.enclosure]
--             |> Maybe.withDefault item2.title
--     in
--         id1 == id2


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port play : String -> Cmd msg
port stop : String -> Cmd msg
