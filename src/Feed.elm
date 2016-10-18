module Feed exposing
    (loadFeed, updateFeed, updateModelFeed, updateFeedItems
    , viewFeedTitle , viewItem
    -- , showMore
    -- , resetShowMore
    -- , hideItemsUnder
    )

import Task
import Http
import String
import Html exposing (Html, text, button, ul, li, div, span, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, title, src, classList)

import Models exposing (..)
import Msgs exposing (..)
import DecodeFeed exposing (decodeFeed)
import ListUtil exposing (takeWhile, dropWhile)
import DateFormat exposing (formatDuration, format)
import Events exposing (onInternalClick)

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


updateModelFeed : Feed -> Model -> Model
updateModelFeed feed model =
    { model | list =
        List.map (\f ->
            if feed.url == f.url then
                feed
            else
                f
        ) model.list
    }


updateFeedItems : Model -> Feed -> Model
updateFeedItems model newFeed =
    let
        feed = model.list
            |> List.filter (\f -> f.url == newFeed.url)
            |> List.head
    in
        case feed of
            Just feed' ->
                let
                    head = List.head feed'.items
                    newItems =
                        case head of
                            Nothing ->
                                []

                            Just head' ->
                                newFeed.items
                                    |> takeWhile
                                        (\item ->
                                            not <| maybeEqual item.url head'.url
                                        )
                in
                    updateModelFeed
                        { feed'
                            | items = newItems ++ feed'.items
                            , state = Normal
                        }
                        model

            Nothing ->
                model


maybeEqual : Maybe a -> Maybe a -> Bool
maybeEqual a b =
    (Maybe.map2 (==) a b) == Just True


viewFeedTitle : Model -> Feed -> Html Msg
viewFeedTitle model feed =
    let
        items = feed.items -- List.filter (\item -> item.show) feed.items
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
                        [ class "btn btn-icon feed-control feed-refresh"
                        , onClick (UpdateFeeds [] feed) ]
                        [ img [ src "assets/refresh.svg" ] [] ]
            -- span [ class "feed-state" ] [ text "*" ]
    in
        div [ class "feed-header" ]
            [ button
                [ class "btn btn-icon top-bar-outset-btn"
                , onClick HideFeed
                ]
                [ img [ src "assets/arrow-left.svg" ] [] ]
            , span [ class "feed-title" ] [ text feed.title ]
            , feedState
            , refreshBtn
            , button
                [ classList
                    [ ("btn btn-icon feed-control feed-trash", True) ]
                , onClick (ShowConfirmDeleteFeed feed)
                ]
                [ img [ src "assets/trash.svg"] [] ]
            , if feed.showConfirmDelete then
                div
                    [ class "feed-control" ]
                    [ div
                        [ class "confirm-delete feed-control" ]
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
                    ]
              else
                  text ""
            ]


viewItem : Model -> Maybe Feed -> Item -> Html Msg
viewItem model feed item =
    li
        [ classList
            [ ("item", True)
            , ("is-current", maybeEqual item.url model.currentItemUrl)
            , ("is-error", item.url == Nothing)
            , ("is-unplayed", item.progress == -1)
            , ("is-played", item.playCount > 0)
            ]
        , toggleItem model item
        ]
        [ renderItemState item model.currentItemUrl model.playerState
        , case feed of
            Just feed' ->
                div
                    [ class "item-feed-title"
                    , onInternalClick (ShowFeed feed'.url)
                    ]
                    [ text feed'.title ]
            Nothing ->
                text ""
        , div [ class "item-desp" ]
            [ div
                [ class "item-title", title item.title ]
                [ text item.title ]
            ]
        , div [ class "item-control" ]
            [ case item.link of
                Just link ->
                    button
                        [ class "btn btn-icon"
                        , onInternalClick (OpenNewLink link)
                        , title "open link"
                        ]
                        [ img [ src "assets/external-link.svg"] [] ]
                Nothing ->
                    text ""
            ]
        , div [ class "item-date" ]
            [ text <| format item.pubDate model.currentTime ]
        , div [ class "item-progress" ]
            [ text <| formatDuration item.duration ]
        ]

toggleItem : Model -> Item -> Html.Attribute Msg
toggleItem model item =
    if maybeEqual item.url model.currentItemUrl then
        if model.playerState == Playing then
            onClick (Pause item)
        else
            onClick (Play item)
    else
        onClick (Play item)


renderItemState: Item -> Maybe String -> PlayerState -> Html Msg
renderItemState item currentItemUrl playerState =
    if maybeEqual item.url currentItemUrl then
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
