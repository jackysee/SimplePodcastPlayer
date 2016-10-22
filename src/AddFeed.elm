module AddFeed exposing (viewAddFeed, addFeedButton)

import Html exposing (Html, div, button, img, span, input, text)
import Html.Attributes exposing (classList, class, src, id, class, value, placeholder)
import Html.Events exposing (onClick, onInput, on, keyCode, onWithOptions)

import Models exposing (..)
import Msgs exposing (..)
import Events exposing (onEnter, onInternalClick)


viewAddFeed : Model -> Html Msg
viewAddFeed model =
    div
        [ classList
            [ ("add-panel", True)
            , ("is-show", model.showAddPanel)
            ]
        ]
        [ button
            [ class "btn btn-icon add-close"
            , onClick HideAddPanel
            ]
            [ img [ src "assets/close.svg"] [] ]
        , input
            [ id "add-feed"
            , class "add-feed"
            , onInput SetUrl
            , onEnter AddFeed 
            , value model.urlToAdd
            , placeholder "Add Feed"
            ]
            []
        , viewLoadFeedState model.loadFeedState
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
        [ img [ src "assets/plus-circle.svg" ] []
            ]
