module View exposing (view)

import Html exposing (div, text, input, Html, span, ul, li, button, img, node)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, src, style, classList, type', checked, title, property)
import Json.Encode
import String

import Models exposing (..)
import Msgs exposing (..)
import Feed exposing (viewFeedTitle, viewItem)
import AddFeed exposing (viewAddFeed, addFeedButton)
import Player exposing (viewPlayer)
import About exposing (viewAbout, viewAboutButton)
import Events exposing (onScroll)
import Icons

view : Model -> Html Msg
view model =
    let
        feed' = model.list
            |> List.filter
                (\f ->
                    case model.listView of
                        ViewFeed url ->
                            f.url == url
                        _ ->
                            False
                )
            |> List.head
        filterBar =
            if List.length model.list > 0 && model.listView /= Queued then
                div
                    [ class "feed-filter" ]
                    [ filterButton "Unlistened" Unlistened model.itemFilter
                    , filterButton "All" All model.itemFilter
                    ]
            else
                text ""
    in
        div [ classList
                [ ("app-wrap", True)
                , ("theme-" ++ (themeToStr model.theme |> String.toLower) , True)
                ]
            ]
            [ viewFontSizeStyle model.fontSize
            , viewAddFeed model
            , viewAbout model
            , div
                [ class "wrap"
                , onClick (SetFloatPanel Hidden)
                ]
                <|
                    [ div
                        [ class "top-bar-wrap" ]
                        [ div
                            [ class "top-bar"]
                            [ viewLeftBtn model.listView
                            , viewTitle model feed'
                            , viewTopLeftBar model
                            ]
                        , filterBar
                        ]
                    ]
                    ++ (viewItemList model feed')
                    ++ [ viewPlayer model ]
            ]


viewFontSizeStyle : FontSize -> Html Msg
viewFontSizeStyle fontSize =
    node "style"
        [ property "innerHTML" <|
            Json.Encode.string
                ("html{font-size:" ++ getFontSizePx fontSize ++ "}")
        ]
        []


viewLeftBtn : ListView -> Html Msg
viewLeftBtn listView =
    if listView == AllFeed then
        addFeedButton
      else
        button
            [ class "btn add-btn btn-icon top-bar-outset-btn"
            , onClick (SetListView AllFeed)
            ]
            [ Icons.arrowLeft ]


viewTitle : Model -> Maybe Feed -> Html Msg
viewTitle model feed' =
    case feed' of
        Just feed ->
            viewFeedTitle model feed

        Nothing ->
            let
                isRefreshing = model.list
                    |> List.filter (\feed -> feed.state == Refreshing )
                    |> List.length
                    |> (<) 0
            in
                div
                    [ class "feed-header" ]
                    [ div
                        [ class "feed-title" ]
                        [ if List.length model.list == 0 then
                            span
                                [ class "feed-empty" ]
                                [ text "← Click to add feed" ]
                          else
                              case model.listView of
                                  Queued ->
                                      text "Queued"
                                  _ ->
                                      text "All Podcasts"
                        ]
                    , if isRefreshing then
                        div
                            [ class "feed-state" ]
                            [ Icons.loadingSpin
                            , viewStatus model
                            ]
                      else
                        text ""

                    , if not isRefreshing && List.length model.list > 0 && model.listView /= Queued then
                        button
                            [ class "btn btn-icon feed-control feed-refresh"
                            , onClick UpdateAllFeed
                            , title "Refresh all feeds"
                            ]
                            [ Icons.refresh ]
                      else
                          text ""
                    ]


viewTopLeftBar : Model -> Html Msg
viewTopLeftBar model =
        div
            [ class "top-left-bar" ]
            [ if List.length model.list > 0 then
                button
                    [ classList
                        [ ("btn btn-icon queued-btn", True)
                        , ("is-selected", model.listView == Queued)
                        ]
                    , onClick (SetListView Queued)
                    ]
                    [ Icons.list
                    , if List.length model.playList > 0 then
                        span
                            [ class "queued-count" ]
                            [ List.length model.playList |> toString |> text ]
                      else
                        text ""
                    ]
            else
                text ""
            , viewAboutButton
            ]


filterButton : String -> ItemFilter -> ItemFilter -> Html Msg
filterButton label filter modelItemFilter =
    button
        [ classList
            [ ("btn btn-text" , True)
            , ("is-active", modelItemFilter == filter)
            ]
        , onClick (SetItemFilter filter)
        ]
        [ text label ]


viewStatus : Model -> Html Msg
viewStatus model =
    let
        txt = model.list
            |> List.filter (\f -> f.state == Refreshing)
            |> List.map (\f -> "Refreshing feed " ++ f.title ++ "...")
            |> List.head
            |> Maybe.withDefault ""
    in
        div
            [ class "feed-status" ]
            [ text txt ]


viewItemList : Model -> Maybe Feed -> List (Html Msg)
viewItemList model feed_ =
    let
        (list, hasMoreItem) = itemList model
        itemList_ =
            case feed_ of
                Just feed ->
                    let
                        list_ = viewItem model Nothing
                    in
                        if List.length list == 0 then
                            div
                                [ class "item-empty" ]
                                [ text "This list is empty." ]
                        else
                            ul
                                [ class "item-list" ]
                                (list
                                    |> List.map snd
                                    |> List.indexedMap (,)
                                    |> List.map list_
                                )
                Nothing ->
                    if List.length list == 0 && List.length model.list > 0 then
                        div
                            [ class "item-empty" ]
                            [ text "This list is empty." ]
                    else
                        ul
                            [ class "item-list"]
                            ( list
                                |> List.indexedMap (,)
                                |> List.map (\(index, (feed, item)) ->
                                     viewItem model (Just feed) (index, item)
                                  )
                            )

        sortBar =
            if List.length list > 0 then
                viewItemSortLatest model
            else
                Nothing

        showMore =
            if hasMoreItem then
                div
                    [ class "feed-show-more" ]
                    [ button
                        [ class "btn btn-text"
                        , onClick ShowMoreItem
                        ]
                        [ text "show more"]
                    ]
              else
                  text ""
    in
        [ sortBar |> Maybe.withDefault (text "")
        , div
            [ classList
                [ ("item-list-wrap", True)
                , ("has-sort", sortBar /= Nothing)
                ]
            , onScroll HideItemDropdown
            ]
            [ itemList_
            , showMore
            ]
        ]



viewItemSortLatest : Model -> Maybe (Html Msg)
viewItemSortLatest model =
    if model.listView /= Queued then
        Just <|
            div
                [ class "item-sort" ]
                [ span
                    [ onClick (SetItemSortLatest (not model.itemSortLatest)) ]
                    [ if model.itemSortLatest then
                        text "latest first"
                      else
                        text "oldest first"
                    ]
                ]
    else
        Nothing
