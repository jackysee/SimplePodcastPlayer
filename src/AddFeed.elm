module AddFeed exposing (viewAddFeed, addFeedButton)

import Html exposing (Html, div, button, img, span, input, text, ul, li)
import Html.Attributes exposing (classList, class, src, id, class, value, placeholder)
import Html.Events exposing (onClick, onInput, on, keyCode, onWithOptions)

import Models exposing (..)
import Msgs exposing (..)
import Events exposing (onInternalClick, onKeydown)
import Icons


viewAddFeed : Model -> Html Msg
viewAddFeed model =
    div
        [ classList
            [ ("add-panel", True)
            , ("is-show", model.floatPanel == AddPanel)
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
                , class "add-feed"
                , onKeydown
                    [ (13, AddFeed)
                    , (27, HideAddPanel)
                    ]
                , onInput SetUrl
                , value model.urlToAdd
                , placeholder "Add Feed"
                ]
                []
            , viewLoadFeedState model.loadFeedState
            ]
        , if model.floatPanel == AddPanel && List.length model.list > 0 then
            div
                [ class "subscriptions-wrap" ]
                [ div
                    [ class "subscription-title" ]
                    [ text "Subscribed: " ]
                , div
                    [ class "subscriptions" ] <|
                    List.map (\feed ->
                        span
                            [ class "subscription-item" ]
                            [ span
                                [ class "add-feed-title"
                                , onInternalClick (ShowFeed feed.url)
                                ]
                                [ text feed.title ]
                            ]

                    ) model.list
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
