module Feed
    exposing
        ( loadFeed
        , updateFeed
        , updateModelFeed
        , updateFeedItems
        , updateUpdateFeed
        , viewFeedTitle
        , viewItem
        , markListenedMsg
        , markItemsListened
        , updateFeeds
        )

import Task exposing (Task)
import Http
import String
import Html exposing (Html, text, button, ul, li, div, span, img, a, input)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Attributes exposing (class, title, src, classList, id, href, target, value)
import Dict exposing (Dict)
import Regex
import String
import Models exposing (..)
import Msgs exposing (..)
import DecodeFeed exposing (decodeYqlFeed, decodeCustomFeed)
import DateFormat exposing (formatDuration, formatDurationShort, format)
import Events exposing (onInternalClick, onClickPosBottomRight, onBlurNotEmpty)
import Icons
import Escape
import Return exposing (Return)
import Storage exposing (saveItems, saveView, noOpTask, saveFeeds)


yqlUrl : String -> String
yqlUrl url =
    String.join ""
        [ "//query.yahooapis.com/v1/public/yql?q="
        , Http.encodeUri ("select * from xml where url = \"" ++ url ++ "\" ")
        , "&format=json"
        ]


customUrl : String -> String -> String
customUrl serviceUrl url =
    String.join ""
        [ serviceUrl ++ "?url="
        , Http.encodeUri url
        ]


loadFeed : Maybe String -> String -> Cmd Msg
loadFeed fallbackRssServiceUrl url =
    Task.attempt
        (\result ->
            case result of
                Ok feed ->
                    AddFeed <| FetchFeedSucceed feed

                Err err ->
                    AddFeed <| FetchFeedFail err
        )
        (Http.get (yqlUrl url) (decodeYqlFeed url)
            |> Http.toTask
            |> Task.onError (loadFallbackFeed fallbackRssServiceUrl url)
        )


updateUpdateFeed : UpdateFeedMsg -> Model -> Return Msg Model
updateUpdateFeed msg model =
    case msg of
        UpdateAllFeed ->
            Return.singleton model
                |> Return.command (updateFeeds model.feeds)

        UpdateFeeds feeds feed ->
            Return.singleton model
                |> Return.map (updateModelFeed { feed | state = Refreshing })
                |> Return.command
                    (updateFeed model.setting.fallbackRssServiceUrl feed feeds)

        UpdateFeedFail feeds feed error ->
            let
                e =
                    Debug.log "error" error

                cmd =
                    if List.length feeds > 0 then
                        updateFeeds feeds
                    else
                        Cmd.none
            in
                Return.singleton model
                    |> Return.map (updateModelFeed { feed | state = RefreshError })
                    |> Return.command cmd

        UpdateFeedSucceed feeds ( feed, items ) ->
            let
                ( model_, items_ ) =
                    updateFeedItems model feed items

                cmd =
                    if List.length feeds > 0 then
                        updateFeeds feeds
                    else
                        Cmd.none
            in
                Return.singleton model_
                    |> Return.map updateViewItems
                    |> Return.effect_ saveView
                    |> Return.command (saveItems items_)
                    |> Return.command cmd


updateFeeds : List Feed -> Cmd Msg
updateFeeds feeds =
    case feeds of
        [] ->
            Cmd.none

        feed :: feeds ->
            Task.attempt
                (\result ->
                    result
                        |> Result.map (\feed -> UpdateFeed (UpdateFeeds feeds feed))
                        |> Result.withDefault NoOp
                )
                (Task.succeed feed)


updateFeed : Maybe String -> Feed -> List Feed -> Cmd Msg
updateFeed fallbackRssServiceUrl feed feeds =
    Task.attempt
        (\result ->
            case result of
                Ok feed ->
                    UpdateFeed <| UpdateFeedSucceed feeds feed

                Err err ->
                    UpdateFeed <| UpdateFeedFail feeds feed err
        )
        (Http.get (yqlUrl feed.url) (decodeYqlFeed feed.url)
            |> Http.toTask
            |> Task.onError (loadFallbackFeed fallbackRssServiceUrl feed.url)
        )


loadFallbackFeed : Maybe String -> String -> (Http.Error -> Task Http.Error ( Feed, List Item ))
loadFallbackFeed serviceUrl_ url =
    (\err ->
        case serviceUrl_ of
            Just serviceUrl ->
                Http.get (customUrl serviceUrl url) (decodeCustomFeed url)
                    |> Http.toTask

            Nothing ->
                Task.fail err
    )


updateModelFeed : Feed -> Model -> Model
updateModelFeed feed model =
    { model
        | feeds =
            List.map
                (\f ->
                    if feed.url == f.url then
                        feed
                    else
                        f
                )
                model.feeds
    }


updateFeedItems : Model -> Feed -> List Item -> ( Model, List Item )
updateFeedItems model newFeed items =
    let
        feed =
            getFeedByUrl model newFeed.url

        urls =
            model.items
                |> List.filterMap
                    (\item ->
                        if item.feedUrl == newFeed.url then
                            Just ( item.url, True )
                        else
                            Nothing
                    )
                |> Dict.fromList
    in
        case feed of
            Just feed_ ->
                let
                    newItems =
                        List.filter
                            (\item -> not (Dict.member item.url urls))
                            items
                in
                    ( model
                        |> updateModelFeed { feed_ | state = Normal }
                        |> (\model_ ->
                                { model_ | items = newItems ++ model_.items }
                           )
                    , newItems
                    )

            Nothing ->
                ( model, [] )


maybeEqual : Maybe a -> Maybe a -> Bool
maybeEqual a b =
    (Maybe.map2 (==) a b) == Just True


viewFeedTitle : Model -> Feed -> Html Msg
viewFeedTitle model feed =
    let
        --items = feed.items
        feedState =
            case feed.state of
                Refreshing ->
                    div
                        [ class "feed-state" ]
                        [ Icons.loadingSpin ]

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
                Refreshing ->
                    text ""

                _ ->
                    button
                        [ class "btn btn-icon feed-control feed-refresh"
                        , onClick <| UpdateFeed (UpdateFeeds [] feed)
                        ]
                        [ Icons.refresh ]
    in
        div [ class "feed-header" ]
            [ div
                [ class "feed-header-title" ]
                [ span
                    [ class "feed-title"
                    , title feed.title
                    ]
                    [ text feed.title ]
                ]
            , case feed.link of
                Just link ->
                    a
                        [ class "btn btn-icon feed-control-btn"
                        , href link
                        , target "_blank"
                        ]
                        [ Icons.externalLink ]

                _ ->
                    text ""
            , button
                [ class "btn btn-icon feed-control feed-control-btn"
                , onInternalClick <| EditFeed <| ShowEditFeed feed
                ]
                [ Icons.edit ]
            , feedState
            , refreshBtn
            ]


viewItem : Model -> Maybe Feed -> ( Int, Item ) -> Html Msg
viewItem model feed ( index, item ) =
    let
        listened =
            if item.markPlayCount == -1 then
                item.playCount > 0
            else
                item.markPlayCount > 0
    in
        li
            [ classList
                [ ( "item", True )
                , ( "is-current", isCurrent item model )
                , ( "is-unplayed", item.progress == -1 && not listened )
                , ( "is-played", listened )
                , ( "is-selected", index == model.view.itemSelected )
                , ( "is-enqueued", inPlayList item model )
                ]

            --, onMouseEnter (ItemAction <| SelectItem index)
            -- , onMouseLeave (UnselectItem item)
            , id ("item-" ++ toString index)
            ]
            [ div
                [ class "item-info-state"
                , toggleItem model item
                ]
                [ viewItemInfo model feed item ]
            , viewItemQueued model item
            , renderQueueControl item model.view.listView
            , viewItemControl listened model item
            , div [ class "item-date-time" ]
                [ div
                    [ class "item-date"
                    , title <| format item.pubDate model.view.currentTime True
                    ]
                    [ text <| format item.pubDate model.view.currentTime False ]
                , div [ class "item-progress" ]
                    [ text <| formatDurationShort item.duration ]
                ]
            ]


toggleItem : Model -> Item -> Html.Attribute Msg
toggleItem model item =
    if isCurrent item model then
        if model.view.playerState == Playing then
            onClick (Player <| Pause item)
        else
            onClick (Player <| Play item)
    else
        onClick (Player <| Play item)


viewItemInfo : Model -> Maybe Feed -> Item -> Html Msg
viewItemInfo model feed item =
    div
        [ class "item-info" ]
        [ div [ class "item-desp" ]
            [ case feed of
                Just feed_ ->
                    div
                        [ class "item-feed-title"
                        , onInternalClick (ItemList <| SetListView <| ViewFeed feed_.url)
                        ]
                        [ text feed_.title ]

                Nothing ->
                    text ""
            , div
                [ class "item-title" ]
                [ renderItemState item model.view.currentItem model.view.playerState
                , text item.title
                ]
            ]
        , let
            description_ =
                item.description
                    |> Maybe.map stripHtml
                    >> Maybe.withDefault ""
                    >> String.slice 0 300
          in
            div
                [ class "item-description-text"

                --, title <| Escape.unEsc description_
                ]
                [ description_ |> Escape.text_ ]
        ]


renderItemState : Item -> Maybe ItemId -> PlayerState -> Html Msg
renderItemState item currentItem playerState =
    if Just ( item.url, item.feedUrl ) == currentItem then
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


renderQueueControl : Item -> ListView -> Html Msg
renderQueueControl item listView =
    if listView == Queued then
        div
            [ class "item-reorder" ]
            [ button
                [ class "btn btn-icon"
                , onInternalClick (ItemAction <| MoveQueuedItemUp item)
                , title "move up"
                ]
                [ Icons.arrowUp ]
            , button
                [ class "btn btn-icon"
                , onInternalClick (ItemAction <| MoveQueuedItemDown item)
                , title "move down"
                ]
                [ Icons.arrowDown ]
            , button
                [ class "btn btn-icon"
                , onInternalClick (ItemAction <| Dequeue item)
                , title "remove from queue"
                ]
                [ Icons.close ]
            ]
    else
        text ""


viewItemQueued : Model -> Item -> Html Msg
viewItemQueued model item =
    if inPlayList item model && model.view.listView /= Queued then
        div
            [ class "item-queued" ]
            [ text "queued" ]
    else
        text ""


viewItemControl : Bool -> Model -> Item -> Html Msg
viewItemControl listened model item =
    let
        newLinkItem =
            case item.link of
                Just link ->
                    [ div
                        [ class "dropdown-item"
                        , onInternalClick (ItemAction <| OpenNewLink link)
                        ]
                        [ text "Open link" ]
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
            , if inPlayList item model then
                div
                    [ class "dropdown-item"
                    , onInternalClick (ItemAction <| Dequeue item)
                    ]
                    [ text "Dequeue" ]
              else
                div
                    [ class "dropdown-item"
                    , onInternalClick (ItemAction <| Enqueue item)
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
                , onInternalClick (ItemAction <| MarkItemsBelowListened item.url)
                ]
                [ text "Mark item and items below as listened" ]
            ]

        menusItems =
            newLinkItem ++ markItem
    in
        if List.length menusItems > 0 then
            div
                [ class "item-control" ]
                [ div
                    [ class "feed-more-btn" ]
                    [ button
                        [ class "btn btn-icon btn-more"
                        , onInternalClick
                            (FloatPanelAction <| ShowItemDropdown item.url)
                        ]
                        [ Icons.ellipsisV ]
                    , case model.view.floatPanel of
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

        markPlayCount =
            if listened then
                0
            else
                1
    in
        ItemAction <| MarkPlayCount item markPlayCount


markItemsListened : Dict String Bool -> List Item -> List Item
markItemsListened toUpdate list =
    List.map
        (\item ->
            if Dict.member item.url toUpdate then
                { item
                    | markPlayCount =
                        if item.markPlayCount == -1 then
                            item.playCount + 1
                        else
                            item.markPlayCount
                }
            else
                item
        )
        list
