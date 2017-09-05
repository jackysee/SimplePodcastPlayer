module Feed
    exposing
        ( loadFeed
        , updateFeed
        , updateModelFeed
        , updateFeedItems
        , updateUpdateFeed
        , updateDeleteFeed
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
import Storage exposing (saveItems, saveView, deleteFeed, noOpTask, saveFeeds)
import Dom
import Player exposing (stop)


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

        SetEditingFeedTitle feedTitle ->
            model
                |> updateView (\v -> { v | editingFeedTitle = feedTitle })
                |> Return.singleton
                |> Return.command
                    (case feedTitle of
                        Just feedTitle_ ->
                            if model.view.editingFeedTitle == Nothing then
                                noOpTask (Dom.focus "input-feed-title")
                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none
                    )

        SetFeedTitle feed title ->
            let
                feed_ =
                    { feed | title = title }
            in
                updateModelFeed feed_ model
                    |> Return.singleton
                    |> Return.command (saveFeeds [ feed_ ])


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


updateDeleteFeed : DeleteFeedMsg -> Model -> Return Msg Model
updateDeleteFeed msg model =
    case msg of
        ShowConfirmDeleteFeed feed ->
            updateModelFeed { feed | showConfirmDelete = True } model
                |> Return.singleton

        HideConfirmDeleteFeed feed ->
            updateModelFeed { feed | showConfirmDelete = False } model
                |> Return.singleton

        ConfirmDeleteFeed feed ->
            let
                feeds =
                    List.filter (\f -> f.url /= feed.url) model.feeds

                items =
                    List.filter (\i -> i.feedUrl /= feed.url) model.items

                currentItemDeleted =
                    not (List.any (\item -> isCurrent item model) items)

                currentItem =
                    if currentItemDeleted then
                        Nothing
                    else
                        model.view.currentItem

                itemUrls =
                    List.map (\item -> ( item.url, item.feedUrl )) items

                playList =
                    List.filter
                        (\playListItem -> not (List.member playListItem itemUrls))
                        model.view.playList
            in
                { model
                    | feeds = feeds
                    , items = items
                }
                    |> updateView
                        (\v ->
                            { v
                                | listView = AllFeed
                                , playList = playList
                                , currentItem = currentItem
                            }
                        )
                    |> Return.singleton
                    |> Return.map updateViewItems
                    |> Return.effect_ saveView
                    |> Return.command (deleteFeed <| toStoreFeed feed)
                    |> Return.command
                        (if currentItemDeleted then
                            stop ""
                         else
                            Cmd.none
                        )


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

        feedTitle =
            case model.view.editingFeedTitle of
                Just title_ ->
                    title_

                Nothing ->
                    feed.title
    in
        div [ class "feed-header" ]
            [ div
                [ classList
                    [ ( "feed-header-title", True )
                    , ( "is-editing", model.view.editingFeedTitle /= Nothing )
                    ]
                ]
                [ input
                    [ id "input-feed-title"
                    , class "input-text input-feed-title"
                    , value feedTitle
                    , onInput (\value -> UpdateFeed <| SetEditingFeedTitle (Just value))
                    , onBlur <|
                        MsgBatch
                            [ UpdateFeed <|
                                SetFeedTitle feed <|
                                    if model.view.editingFeedTitle /= (Just "") then
                                        Maybe.withDefault feed.title model.view.editingFeedTitle
                                    else
                                        feed.title
                            , UpdateFeed <| SetEditingFeedTitle Nothing
                            ]
                    ]
                    []
                , span
                    [ class "feed-title"
                    , title feedTitle
                    , onClick (UpdateFeed <| SetEditingFeedTitle <| Just feed.title)
                    ]
                    [ text feedTitle ]
                ]
            , feedState
            , refreshBtn
            , if feed.state /= Refreshing then
                viewConfirmDelete feed
              else
                text ""
            ]


viewConfirmDelete : Feed -> Html Msg
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
                        , onClick <| DeleteFeed (HideConfirmDeleteFeed feed)
                        ]
                        [ text "Cancel" ]
                    , span [] [ text "/" ]
                    , button
                        [ class "btn btn-text confirm-delete-btn "
                        , onClick <| DeleteFeed (ConfirmDeleteFeed feed)
                        ]
                        [ text "Delete" ]
                    ]
                ]
          else
            button
                [ classList
                    [ ( "btn btn-icon feed-control feed-trash", True ) ]
                , onClick <| DeleteFeed (ShowConfirmDeleteFeed feed)
                ]
                [ Icons.trash ]
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
            , toggleItem model item
              --, onMouseEnter (ItemAction <| SelectItem index)
              -- , onMouseLeave (UnselectItem item)
            , id ("item-" ++ toString index)
            ]
            [ renderItemState item model.view.currentItem model.view.playerState
            , div [ class "item-info" ]
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
                        [ class "item-title", title item.title ]
                        [ text item.title ]
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
                        , title <| Escape.unEsc description_
                        ]
                        [ description_
                            |> String.slice 0 300
                            >> Escape.text_
                        ]
                ]
            , viewItemQueued model item
            , renderQueueControl item model.view.listView
            , viewItemControl listened model item
            , div []
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
                ]
                [ Icons.arrowUp ]
            , button
                [ class "btn btn-icon"
                , onInternalClick (ItemAction <| MoveQueuedItemDown item)
                ]
                [ Icons.arrowDown ]
            , button
                [ class "btn btn-icon"
                , onInternalClick (ItemAction <| Dequeue item)
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
