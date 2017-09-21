module AddFeed exposing (addFeedButton, updateAddFeed, viewAddInput)

import Html exposing (Html, div, button, img, span, input, text, ul, li)
import Html.Attributes exposing (classList, class, src, id, class, value, placeholder, disabled)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Models exposing (..)
import Msgs exposing (..)
import Events exposing (onInternalClick, onKeyup)
import Icons
import Return exposing (Return)
import Dom
import Storage exposing (..)
import Feed exposing (loadFeed)


updateAddFeed : AddFeedMsg -> Model -> Return Msg Model
updateAddFeed msg model =
    case msg of
        ShowAddPanel ->
            Return.singleton model
                |> Return.map (updateView (\v -> { v | floatPanel = About Subscriptions }))
                |> Return.command (noOpTask (Dom.focus "add-feed"))

        HideAddPanel ->
            ( model
                |> updateView
                    (\v ->
                        { v
                            | floatPanel = Hidden
                            , urlToAdd = ""
                        }
                    )
            , noOpTask (Dom.blur "add-feed")
            )

        SetUrl value ->
            model
                |> updateView
                    (\view ->
                        { view
                            | urlToAdd = value
                            , loadFeedState = Empty
                        }
                    )
                |> Return.singleton

        ToAddFeed ->
            if List.any (\feed -> feed.url == model.view.urlToAdd) model.feeds then
                model
                    |> updateView (\view -> { view | loadFeedState = AlreadyExist })
                    |> Return.singleton
            else
                model
                    |> updateView (\view -> { view | loadFeedState = Loading })
                    |> Return.singleton
                    |> Return.command
                        (loadFeed
                            model.setting.fallbackRssServiceUrl
                            model.view.urlToAdd
                        )

        FetchFeedSucceed ( feed, items ) ->
            let
                view =
                    model.view
            in
                { model
                    | feeds = model.feeds ++ [ feed ]
                    , items = model.items ++ items
                    , view =
                        { view
                            | loadFeedState = Empty
                            , urlToAdd = ""

                            --, floatPanel = Hidden
                            --, listView = ViewFeed feed.url
                            --, itemFilter = Unlistened
                        }
                }
                    |> Return.singleton
                    |> Return.command (noOpTask (Dom.blur "add-feed"))
                    |> Return.map updateViewItems
                    |> Return.effect_ saveView
                    |> Return.command (saveFeeds [ feed ])
                    |> Return.command (saveItems items)

        FetchFeedFail error ->
            let
                e =
                    Debug.log "error" error
            in
                model
                    |> updateView (\v -> { v | loadFeedState = Error })
                    |> Return.singleton
                    |> Return.command (noOpTask (Dom.focus "add-feed"))



--viewAddFeed : Model -> Html Msg
--viewAddFeed model =
--    div
--        [ classList
--            [ ( "panel add-panel", True )
--            , ( "is-show", model.view.floatPanel == AddPanel )
--            ]
--        ]
--        [ button
--            [ class "btn btn-icon panel-close"
--            , onClick <| AddFeed HideAddPanel
--            ]
--            [ Icons.close ]
--        , viewAddInput model
--        ]


viewAddInput : Model -> Html Msg
viewAddInput model =
    div
        [ class "add-input" ]
        [ input
            [ id "add-feed"
            , class "add-feed input-text"
            , onKeyup
                [ ( 13
                  , \code ->
                        if model.view.urlToAdd == "" then
                            NoOp
                        else
                            AddFeed ToAddFeed
                  )
                , ( 27, \_ -> AddFeed HideAddPanel )
                ]
            , onInput (\s -> AddFeed <| SetUrl s)
            , value model.view.urlToAdd
            , placeholder "Input Podcast Feed and ENTER to Add"
            , disabled <| model.view.loadFeedState == Loading
            ]
            []
        , viewLoadFeedState model.view.loadFeedState
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
        [ class "btn add-btn btn-icon"
        , onInternalClick (AddFeed ShowAddPanel)
        ]
        [ Icons.plus
        ]
