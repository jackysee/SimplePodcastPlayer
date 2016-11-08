module Feed exposing
    ( loadFeed, updateFeed, updateModelFeed, updateFeedItems
    , viewFeedTitle , viewItem, viewConfirmDelete, markListenedMsg, markItemsListened)

import Task exposing (Task)
import Http
import String
import Html exposing (Html, text, button, ul, li, div, span, img, a)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Attributes exposing (class, title, src, classList, id, href, target)
import Dict exposing (Dict)
import Regex
import String

import Models exposing (..)
import Msgs exposing (..)
import DecodeFeed exposing (decodeYqlFeed, decodeCustomFeed)
import DateFormat exposing (formatDuration, formatDurationShort, format)
import Events exposing (onInternalClick, onClickPosBottomRight)
import Icons

yqlUrl: String -> String
yqlUrl url =
    String.join ""
        [ "//query.yahooapis.com/v1/public/yql?q="
        , Http.uriEncode ("select * from xml where url = \"" ++ url ++ "\" ")
        , "&format=json"
        ]


customUrl : String -> String -> String
customUrl serviceUrl url =
    String.join ""
        [ serviceUrl ++ "?url="
        , Http.uriEncode url
        ]


loadFeed : Maybe String -> String -> Cmd Msg
loadFeed fallbackRssServiceUrl url =
    Task.perform
        FetchFeedFail
        FetchFeedSucceed
        (Http.get (decodeYqlFeed url) (yqlUrl url)
            `Task.onError`
            loadFallbackFeed fallbackRssServiceUrl url
        )


updateFeed : Maybe String -> Feed -> List Feed -> Cmd Msg
updateFeed fallbackRssServiceUrl feed feeds =
    Task.perform
        (UpdateFeedFail feeds feed)
        (UpdateFeedSucceed feeds)
        (Http.get (decodeYqlFeed feed.url) (yqlUrl feed.url)
            `Task.onError`
            loadFallbackFeed fallbackRssServiceUrl feed.url
        )


loadFallbackFeed : Maybe String -> String -> (Http.Error -> Task Http.Error Feed)
loadFallbackFeed serviceUrl_ url =
    (\err ->
        case serviceUrl_ of
            Just serviceUrl ->
                Http.get (decodeCustomFeed url) (customUrl serviceUrl url)
            Nothing ->
                Task.fail err
    )


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
                    urls = feed'.items
                        |> List.map (\item -> (item.url, True))
                        |> Dict.fromList
                    newItems =
                        newFeed.items
                            |> List.filter
                                (\item -> not (Dict.member item.url urls))
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
        items = feed.items
        feedState =
            case feed.state of
                Refreshing ->
                    div
                        [ class "feed-state" ]
                        [ Icons.loadingSpin
                        -- , span
                        --     [ class "feed-status" ]
                        --     [ text <| feed.title ++ " is refreshing..." ]
                        ]
                RefreshError ->
                    span
                        [ class "feed-state feed-state-error"
                        , title "Error in updating feed. Please try later"
                        ]
                        [ Icons.exclamation ]
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
                        [ Icons.refresh ]
    in
        div [ class "feed-header" ]
            [ span
                [ class "feed-title"
                , title feed.title
                ]
                [ text feed.title ]
            , feedState
            , refreshBtn
            , if feed.state /= Refreshing then
                  viewConfirmDelete feed
              else
                  text ""
            ]


viewConfirmDelete: Feed -> Html Msg
viewConfirmDelete feed =
    div
        [ class "feed-confirm-delete" ]
        [ if feed.showConfirmDelete then
            div
                [ class "feed-control" ]
                [ div
                    [ class "confirm-delete feed-control" ]
                    --[ span [] [ text "Delete?" ]
                    [ button
                        [ class "btn btn-text"
                        , onClick (HideConfirmDeleteFeed feed)
                        ]
                        [ text "Cancel" ]
                    , span [] [ text "/" ]
                    , button
                        [ class "btn btn-text confirm-delete-btn "
                        , onClick (ConfirmDeleteFeed feed)
                        ]
                        [ text "Delete"]
                    ]
                ]
          else
            button
                [ classList
                    [ ("btn btn-icon feed-control feed-trash", True) ]
                , onClick (ShowConfirmDeleteFeed feed)
                ]
                [ Icons.trash ]
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
                , ("is-current", Just item.url == model.currentItemUrl)
                , ("is-unplayed", item.progress == -1 && not listened)
                , ("is-played", listened)
                , ("is-selected", model.itemSelected == Just item.url)
                , ("is-enqueued", List.member item.url model.playList)
                ]
            , toggleItem model item
            , onMouseEnter (SelectItem item)
            -- , onMouseLeave (UnselectItem item)
            , id ("item-" ++ toString index)
            ]
            [ renderItemState item model.currentItemUrl model.playerState
            , case feed of
                Just feed' ->
                    div
                        [ class "item-feed-title"
                        , onInternalClick (SetListView <| ViewFeed feed'.url)
                        ]
                        [ text feed'.title ]
                Nothing ->
                    text ""
            , div [ class "item-desp" ]
                [ div
                    [ class "item-title", title item.title ]
                    [ text item.title ]
                , div
                    [ class "item-description-text" ]
                    [ text
                        (item.description
                            |> Maybe.map stripHtml
                            |> Maybe.withDefault ""
                            |> String.slice 0 300
                        )
                    ]
                ]
            , viewItemQueued model item
            , renderQueueControl item model.listView
            , viewItemControl listened model item
            , div [ class "item-progress" ]
                [ text <| formatDurationShort item.duration ]
            , div
                [ class "item-date"
                , title <| format item.pubDate model.currentTime True
                ]
                [ text <| format item.pubDate model.currentTime False ]
            ]


toggleItem : Model -> Item -> Html.Attribute Msg
toggleItem model item =
    if Just item.url == model.currentItemUrl then
        if model.playerState == Playing then
            onClick (Pause item)
        else
            onClick (Play item)
    else
        onClick (Play item)


renderItemState: Item -> Maybe String -> PlayerState -> Html Msg
renderItemState item currentItemUrl playerState =
    if Just item.url == currentItemUrl then
        div
            [ class "item-state" ]
            [ if playerState == SoundLoading then
                Icons.loadingSpin
              else if playerState == Playing then
                Icons.equalizerPlaying
              else
                Icons.equalizerStop
            ]
    else
        div
            [ class "item-state" ]
            [ Icons.play ]


stripHtml : String -> String
stripHtml str =
    Regex.replace
        Regex.All
        (Regex.regex "<\\/?([a-z][a-z0-9]*)\\b[^>]*>?")
        (\_ -> "")
        str


renderQueueControl: Item -> ListView -> Html Msg
renderQueueControl item listView =
    if listView == Queued then
        div
            [ class "item-reorder" ]
            [ button
                [ class "btn btn-icon"
                , onInternalClick (MoveQueuedItemUp item.url)
                ]
                [ Icons.arrowUp ]
            , button
                [ class "btn btn-icon"
                , onInternalClick (MoveQueuedItemDown item.url)
                ]
                [ Icons.arrowDown ]
            , button
                [ class "btn btn-icon"
                , onInternalClick (Dequeue item.url)
                ]
                [ Icons.close ]
            ]
    else
        text ""


viewItemQueued: Model -> Item -> Html Msg
viewItemQueued model item =
    if List.member item.url model.playList && model.listView /= Queued then
        div
            [ class "item-queued" ]
            [ text "queued" ]
    else
        text ""


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
            [ div
                [ class "dropdown-item "
                , onInternalClick NoOp
                ]
                [ a
                    [ href item.url
                    , target "_blank"
                    , title "right click save as"
                    ]
                    [ text "Download file" ]

                ]
            , if List.member item.url model.playList then
                div
                    [ class "dropdown-item"
                    , onInternalClick (Dequeue item.url)
                    ]
                    [ text "Dequeue" ]
              else
                div
                    [ class "dropdown-item"
                    , onInternalClick (Enqueue item.url)
                    ]
                    [ text "Enqueue" ]
            , div
                [ class "dropdown-item"
                , onInternalClick (markListenedMsg item)
                ]
                [ if listened then
                      text "Mark as unlistened"
                  else
                      text "Mark as listened"
                ]
            , div
                [ class "dropdown-item"
                , onInternalClick (MarkItemsBelowListened item.url)
                ]
                [ text "Mark item and items below as listened"]

            ]

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
                                ( [ Just item.url, item.link ]
                                     |> Maybe.oneOf
                                     |> Maybe.withDefault ""
                                )
                            )
                        ]
                        [ Icons.ellipsisV ]
                    , case model.floatPanel of
                        ItemDropdown url ->
                            if url == item.url || Just url == item.link then
                                div
                                    [ class "dropdown-panel feed-more-panel" ]
                                    menusItems
                            else
                                text ""

                        _ ->
                            text ""
                    ]
                ]
        else
            text ""


markListenedMsg : Item -> Msg
markListenedMsg item =
    let
        listened =
            if item.markPlayCount == -1 then
                item.playCount > 0
            else
                item.markPlayCount > 0
        markPlayCount = if listened then 0 else 1
    in
        MarkPlayCount item.url markPlayCount


markItemsListened : Dict String Bool -> List Feed -> List Feed
markItemsListened toUpdate list =
    List.map
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
        list
