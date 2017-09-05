module Goto exposing (viewGoto, updateGoto)

import Html exposing (Html, div, button, span, input, text, ul, li)
import Html.Attributes exposing (classList, class, src, id, class, value, placeholder, disabled)
import Html.Events exposing (onClick, onInput, on, keyCode, onWithOptions)
import Events exposing (onInternalClick, onKeyup)
import Models exposing (..)
import Msgs exposing (..)
import Dom
import Return exposing (Return)
import Storage exposing (noOpTask)
import ListUtil exposing (findIndex)


viewGoto : Model -> Html Msg
viewGoto model =
    let
        feeds =
            model.feeds
                |> List.filter
                    (\feed ->
                        String.contains
                            (String.toLower model.view.gotoSearch)
                            (String.toLower feed.title)
                    )
                |> List.take 8

        selected =
            findIndex model.view.gotoSelected feeds
    in
        div
            [ classList
                [ ( "goto-panel", True )
                , ( "is-show", model.view.floatPanel == GotoPanel )
                ]
            ]
            [ div
                [ class "goto-input" ]
                [ input
                    [ id "goto-feed"
                    , class "goto-feed input-text"
                    , onKeyup
                        [ ( 38, \_ -> GotoAction GotoUp )
                        , ( 40, \_ -> GotoAction GotoDown )
                        , ( 27, \_ -> GotoAction HideGoto )
                        , ( 13
                          , \_ ->
                                selected
                                    |> Maybe.map
                                        (\f ->
                                            MsgBatch
                                                [ ItemList <| SetListView <| ViewFeed f.url
                                                , GotoAction HideGoto
                                                ]
                                        )
                                    |> Maybe.withDefault NoOp
                          )
                        ]
                    , onInput (\s -> GotoAction <| SetSearch s)
                    , value model.view.gotoSearch
                    , placeholder "Go to"
                      --, disabled <| model.view.loadFeedState == Loading
                    ]
                    []
                , if List.length model.feeds > 0 then
                    ul
                        [ class "goto-items" ]
                        (feeds
                            |> List.indexedMap
                                (\index feed ->
                                    li
                                        [ classList
                                            [ ( "goto-item", True )
                                            , ( "selected", model.view.gotoSelected == index )
                                            ]
                                        ]
                                        [ text feed.title ]
                                )
                        )
                  else
                    text ""
                ]
            ]


updateGoto : GotoMsg -> Model -> Return Msg Model
updateGoto msg model =
    case msg of
        ShowGoto ->
            Return.singleton model
                |> Return.map
                    (updateView
                        (\v ->
                            { v
                                | floatPanel = GotoPanel
                                , gotoSelected = 0
                                , gotoSearch = ""
                            }
                        )
                    )
                |> Return.command (noOpTask <| Dom.focus "goto-feed")

        HideGoto ->
            ( model
                |> updateView
                    (\v ->
                        { v
                            | floatPanel = Hidden
                            , gotoSearch = ""
                        }
                    )
            , noOpTask (Dom.blur "goto-feed")
            )

        SetSearch str ->
            model
                |> updateView
                    (\view ->
                        { view | gotoSearch = str }
                    )
                |> Return.singleton

        GotoUp ->
            model
                |> updateView
                    (\v ->
                        { v | gotoSelected = max 0 (model.view.gotoSelected - 1) }
                    )
                |> Return.singleton

        GotoDown ->
            let
                len =
                    List.length model.feeds
            in
                model
                    |> updateView
                        (\v ->
                            { v | gotoSelected = min (len - 1) (model.view.gotoSelected + 1) }
                        )
                    |> Return.singleton
