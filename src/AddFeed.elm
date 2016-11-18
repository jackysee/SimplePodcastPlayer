module AddFeed exposing (viewAddFeed, addFeedButton)

import Html exposing (Html, div, button, img, span, input, text, ul, li)
import Html.Attributes exposing (classList, class, src, id, class, value, placeholder, disabled)
import Html.Events exposing (onClick, onInput, on, keyCode, onWithOptions)
import Models exposing (..)
import Msgs exposing (..)
import Events exposing (onInternalClick, onKeyup)
import Icons


viewAddFeed : Model -> Html Msg
viewAddFeed model =
    div
        [ classList
            [ ( "add-panel", True )
            , ( "is-show", model.view.floatPanel == AddPanel )
            ]
        ]
        [ div
            [ class "add-input" ]
            [ button
                [ class "btn btn-icon add-close"
                , onClick HideAddPanel
                ]
                [ Icons.close
                ]
            , input
                [ id "add-feed"
                , class "add-feed input-text"
                , onKeyup
                    [ ( 13
                      , \code ->
                            if model.view.urlToAdd == "" then
                                NoOp
                            else
                                AddFeed
                      )
                    , ( 27, \_ -> HideAddPanel )
                    ]
                , onInput SetUrl
                , value model.view.urlToAdd
                , placeholder "Add Feed"
                , disabled <| model.view.loadFeedState == Loading
                ]
                []
            , viewLoadFeedState model.view.loadFeedState
            ]
        , if model.view.floatPanel == AddPanel && List.length model.feeds > 0 then
            div
                [ class "subscriptions-wrap" ]
                [ div
                    [ class "subscription-title" ]
                    [ text "Subscribed: " ]
                , div
                    [ class "subscriptions" ]
                  <|
                    List.map
                        (\feed ->
                            span
                                [ class "subscription-item" ]
                                [ span
                                    [ class "add-feed-title"
                                    , onInternalClick (SetListView (ViewFeed feed.url))
                                    ]
                                    [ text feed.title ]
                                ]
                        )
                        model.feeds
                ]
          else
            text ""
        ]


viewLoadFeedState : LoadFeedState -> Html Msg
viewLoadFeedState state =
    div [ class "add-feed-state" ]
        [ if state == Loading then
            span [] [ text "Loading feed..." ]
          else if state == Error then
            span [] [ text "Problem loading feed" ]
          else if state == AlreadyExist then
            span [] [ text "The feed is added already." ]
          else
            text ""
        ]


addFeedButton : Html Msg
addFeedButton =
    button
        [ class "btn add-btn btn-icon top-bar-outset-btn"
        , onInternalClick ShowAddPanel
        ]
        [ Icons.subscription
        ]
