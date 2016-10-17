module Feed exposing
    (loadFeed, updateFeed, updateModelFeed, updateFeedItems
    , viewFeed , viewItem
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
-- import ListUtil exposing (takeWhile, dropWhile)
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
                                    -- |> takeWhile (\item ->
                                    --     case Maybe.map2 (/=) item.url head'.url of
                                    --         Nothing -> False
                                    --         Just notEqual -> notEqual
                                    -- )
                                    -- |> List.map (\item -> { item | show = True })
                in
                    updateModelFeed
                        { feed'
                            | items = newItems ++ feed'.items
                            , state =
                                if List.length newItems > 0 then
                                    HasNewItem
                                else
                                    Normal
                        }
                        model

            Nothing ->
                model


-- showMore : Int -> List Item -> List Item
-- showMore num list =
--     let
--         ( list1, list2 ) =
--             List.partition (\item -> item.show) list
--
--         list2' =
--             (List.take num list2
--                 |> List.map (\item -> { item | show = True })
--             )
--                 ++ List.drop num list2
--     in
--         list1 ++ list2'


-- resetShowMore : Int -> List Item -> List Item
-- resetShowMore num list =
--     (List.take num list
--         |> List.map (\item -> { item | show = True })
--     )
--     ++
--     (List.drop num list
--         |> List.map (\item -> { item | show = False })
--     )
--
--
-- hideItemsUnder : Item -> List Item -> List Item
-- hideItemsUnder item list =
--     let
--         predicate = \i -> not (maybeEqual item.url i.url)
--         list1 = takeWhile predicate list
--         list2 = dropWhile predicate list
--         -- (list1', list2') =
--         --     case list2 of
--         --         [] ->
--         --             (list1, [])
--         --         x::xs ->
--         --             (list1 ++ [x], xs)
--     in
--         -- list1' ++ (List.map (\i -> { i | show = False }) list2')
--         list1 ++ (List.map (\i -> { i | show = False }) list2)


maybeEqual : Maybe a -> Maybe a -> Bool
maybeEqual a b =
    (Maybe.map2 (==) a b) == Just True


viewFeed : Model -> Feed -> Html Msg
viewFeed model feed =
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
                -- , span [ class "feed-state" ] [ text feedState ]
                ]
            , ul [ class "item-list" ] <|
                (List.map (viewItem model Nothing) items)
                ++
                    if List.length items < List.length feed.items then
                        [ li [ class "item item-more"]
                            [ button
                                [ class "btn btn-text feed-show-more"
                                -- , onClick (ShowMoreItem feed)
                                ]
                                [ text "...more" ]
                            ]
                        ]
                    else
                      []
            ]


viewItem : Model -> Maybe String -> Item -> Html Msg
viewItem model feedTitle item =
    li
        [ classList
            [ ("item", True)
            , ("is-current", maybeEqual item.url model.currentItemUrl)
            , ("is-error", item.url == Nothing)
            , ("is-unplayed", item.progress.current == -1)
            ]
        , toggleItem model item
        ]
        [ renderItemState item model.currentItemUrl model.playerState
        , case feedTitle of
            Just title ->
                div
                    [ class "item-feed-title" ]
                    [ text title ]
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
                        [ class "btn"
                        , onInternalClick (OpenNewLink link)
                        , title "open link"
                        ]
                        [ img [ src "assets/external-link.svg"] [] ]
                Nothing ->
                    text ""
            , button
                [ class "btn"
                , title "Hide item and items below"
                -- , onInternalClick (HideAllUnder item)
                ]
                [ img [ src "assets/arrow-down.svg" ] [] ]
            ]
        , div [ class "item-date" ]
            [ text <| format item.pubDate model.currentTime ]
        , div [ class "item-progress" ]
            [ text <| formatDuration item.progress.duration ]
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
