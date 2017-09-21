port module EditFeed exposing (viewEditFeed, updateEditFeed)

import Models exposing (..)
import Msgs exposing (..)
import Html exposing (Html, div, text, button, input)
import Html.Attributes exposing (classList, class, readonly, value, id, tabindex, disabled)
import Icons
import Html.Events exposing (onClick, onInput)
import Events exposing (onKeyup, onInternalClick)
import Return exposing (Return)
import Storage exposing (saveView, saveFeeds, noOpTask, deleteFeed)
import Feed exposing (updateModelFeed)
import Dom
import Player exposing (stop)
import String


updateEditFeed : EditFeedMsg -> Model -> Return Msg Model
updateEditFeed msg model =
    case msg of
        ShowEditFeed feed ->
            model
                |> updateView
                    (\v ->
                        { v
                            | floatPanel = EditFeedPanel feed
                            , editingFeedTitle = feed.title
                        }
                    )
                |> Return.singleton
                |> Return.command (noOpTask (Dom.focus "edit-input"))

        HideEditFeed ->
            model
                |> updateView (\v -> { v | floatPanel = Hidden })
                |> Return.singleton
                |> Return.command (noOpTask (Dom.focus "app-main"))

        SetEditingFeedTitle feedTitle ->
            model
                |> updateView (\v -> { v | editingFeedTitle = feedTitle })
                |> Return.singleton

        SetFeedTitle feed ->
            let
                title =
                    if model.view.editingFeedTitle == "" then
                        feed.title
                    else
                        model.view.editingFeedTitle

                feed_ =
                    { feed | title = title }
            in
                model
                    |> updateModelFeed feed_
                    |> updateView (\v -> { v | floatPanel = Hidden })
                    |> Return.singleton
                    |> Return.command (saveFeeds [ feed_ ])

        ShowConfirmDelete ->
            updateEditingFeed model (\f -> { f | showConfirmDelete = True })
                |> Return.singleton

        HideConfirmDelete ->
            updateEditingFeed model (\f -> { f | showConfirmDelete = False })
                |> Return.singleton

        ConfirmDelete feed ->
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
                                , floatPanel = Hidden
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

        FocusUrl ->
            Return.singleton model
                |> Return.command (selectText "edit-feed-url")


updateEditingFeed : Model -> (Feed -> Feed) -> Model
updateEditingFeed model updater =
    case model.view.floatPanel of
        EditFeedPanel feed ->
            model
                |> updateView (\v -> { v | floatPanel = EditFeedPanel (updater feed) })

        _ ->
            model


viewEditFeed : Model -> Html Msg
viewEditFeed model =
    div
        [ classList
            [ ( "panel edit-panel", True )
            , ( "is-show"
              , case model.view.floatPanel of
                    EditFeedPanel _ ->
                        True

                    _ ->
                        False
              )
            ]
        , id "edit-panel"
        , tabindex -1
        , onKeyup [ ( 27, \_ -> EditFeed HideEditFeed ) ]
        ]
        (case model.view.floatPanel of
            EditFeedPanel feed ->
                [ button
                    [ class "btn btn-icon panel-close"
                    , onClick <| EditFeed HideEditFeed
                    ]
                    [ Icons.close ]
                , div
                    [ class "feed-title edit-feed-title" ]
                    [ text feed.title ]
                , input
                    [ class "edit-input feed-url"
                    , id "edit-feed-url"
                    , readonly True
                    , value feed.url
                    , onInternalClick <| EditFeed FocusUrl
                    ]
                    []
                , div
                    [ class "edit-item" ]
                    [ div [ class "edit-label" ] [ text "Title (Required)" ]
                    , input
                        [ class "edit-input input-text"
                        , value model.view.editingFeedTitle
                        , onInput (\v -> EditFeed <| SetEditingFeedTitle v)
                        ]
                        []
                    , button
                        [ class "btn btn-block btn-set-feed-title"
                        , onClick <| EditFeed <| SetFeedTitle feed
                        , disabled <|
                            (||)
                                (String.trim model.view.editingFeedTitle == feed.title)
                                (String.trim model.view.editingFeedTitle == "")
                        ]
                        [ text "Change title" ]
                    ]
                , viewConfirmDelete feed
                ]

            _ ->
                [ text "" ]
        )


viewConfirmDelete : Feed -> Html Msg
viewConfirmDelete feed =
    div
        [ class "feed-confirm-delete" ]
    <|
        if feed.showConfirmDelete then
            [ div []
                [ text "Sure to unsubscribe?" ]
            , button
                [ class "btn btn-block"
                , onClick <| EditFeed HideConfirmDelete
                ]
                [ text "Cancel" ]
            , button
                [ class "btn btn-block btn-danger"
                , onClick <| EditFeed <| ConfirmDelete feed
                ]
                [ text "Confirm Unsubscribe" ]
            ]
        else
            [ button
                [ class "btn btn-block btn-danger"
                , onClick <| EditFeed ShowConfirmDelete
                ]
                [ text "Unsubscribe" ]
            ]


port selectText : String -> Cmd msg
