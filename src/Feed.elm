module Feed exposing
    ( loadFeed, updateFeed, updateModelFeed, updateFeedItems
    , viewFeedTitle , viewItem )

import Task
import Http
import String
import Html exposing (Html, text, button, ul, li, div, span, img, a)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Attributes exposing (class, title, src, classList, id)

import Models exposing (..)
import Msgs exposing (..)
import DecodeFeed exposing (decodeFeed)
import ListUtil exposing (takeWhile, dropWhile)
import DateFormat exposing (formatDuration, formatDurationShort, format)
import Events exposing (onInternalClick, onClickPosBottomRight)

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
                    -- a = Debug.log "newFeed.items" newFeed.items
                    newItems =
                        case head of
                            Nothing ->
                                []

                            Just head' ->
                                newFeed.items
                                    |> takeWhile
                                        (\item ->
                                            item.url /= head'.url && item.url /= Nothing
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
                    span
                        [ class "feed-state" ]
                        [ img [src  "assets/loading-spin.svg" ] [] ]
                RefreshError ->
                    span
                        [ class "feed-state feed-state-error"
                        , title "Error in updating feed"
                        ]
                        [ img [ src "assets/exclamation.svg" ] [] ]
                _ ->
                    text ""
        refreshBtn =
            case feed.state of
                Refreshing -> text ""
                _ ->
                    button
                        [ class "btn btn-icon feed-control feed-refresh"
                        , onClick (UpdateFeeds [] feed)
                        ]
                        [ img [ src "assets/refresh.svg" ] [] ]
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


viewItem : Model -> Maybe Feed -> (Int, Item) -> Html Msg
viewItem model feed (index, item) =
    let
        listened =
            if item.markPlayCount == -1 then
                item.playCount > 0
            else
                item.markPlayCount > 0
    in
        li
            [ classList
                [ ("item", True)
                , ("is-current", maybeEqual item.url model.currentItemUrl)
                , ("is-error", item.url == Nothing)
                , ("is-unplayed", item.progress == -1 && not listened)
                , ("is-played", listened)
                , ("is-selected", maybeEqual model.itemSelected item.url)
                ]
            , toggleItem model item
            , onMouseEnter (SelectItem item)
            , onMouseLeave (UnselectItem item)
            , id ("item-" ++ toString index)
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
            , viewItemControl listened model item
            , div [ class "item-date" ]
                [ text <| format item.pubDate model.currentTime ]
            , div [ class "item-progress" ]
                [ text <| formatDurationShort item.duration ]
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


viewItemControl : Bool -> Model -> Item  -> Html Msg
viewItemControl listened model item =
    let
        newLinkItem =
            case item.link of
                Just link ->
                    [ div
                        [ class "dropdown-item"
                        , onInternalClick (OpenNewLink link)
                        ]
                        [  text "Open link" ]
                    ]

                Nothing ->
                    []

        markItem =
            case item.url of
                Just url ->
                    let
                        markPlayCount = if listened then 0 else 1
                    in
                        [ div
                            [ class "dropdown-item"
                            , onInternalClick (MarkPlayCount url markPlayCount)
                            ]
                            [ if listened then
                                  text "Mark as unlistened"
                              else
                                  text "Mark as listened"
                            ]
                        , div
                            [ class "dropdown-item"
                            , onInternalClick (MarkItemsBelowListened url)
                            ]
                            [ text "Mark all items below as listened"]
                        ]

                Nothing ->
                    []

        menusItems = newLinkItem ++ markItem
    in
        if List.length menusItems > 0 then
            div
                [ class "item-control" ]
                [ div
                    [ class "feed-more-btn" ]
                    [ button
                        [ class "btn btn-icon btn-more"
                        , onInternalClick
                            (ShowItemDropdown
                                ( [ item.url, item.link ]
                                     |> Maybe.oneOf
                                     |> Maybe.withDefault ""
                                )
                            )
                        ]
                        [ img [ src "assets/ellipsis-v.svg" ] []
                        ]
                    , if model.itemDropdown == item.url || model.itemDropdown == item.link then
                          div
                              [ class "dropdown-panel feed-more-panel" ]
                              menusItems
                      else
                          text ""
                    ]
                ]
        else
            text ""


