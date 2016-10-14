module Player exposing (viewPlayer)

import Html exposing (Html, text, div, img, button)
import Html.Attributes exposing (class, style, src, classList)
import Html.Events exposing (on, onClick)
import Json.Decode as Json

import DecodePosition exposing (decodeLeftPercentage)
import Models exposing (..)
import Msgs exposing (..)
import DateFormat exposing (formatDuration)

progressBar : Progress -> Html Msg
progressBar p =
    if p.duration == -1 then
        text ""
    else
        div
            [ class "progress-bar"
            , onSetProgress SetProgress
            ]
            [ div
                [ class "progress-bar-value"
                , style [("width", toString (p.current * 100 / p.duration) ++ "%" )]
                ]
                []
            ]


onSetProgress : (Float -> Msg) -> Html.Attribute Msg
onSetProgress tagger =
    on "mouseup" <|
        Json.map tagger decodeLeftPercentage


getCurrentItem : Model -> Maybe Item
getCurrentItem model =
    model.list
        |> List.concatMap (\feed -> feed.items)
        |> List.filter (\item -> isCurrent item.url model)
        |> List.head


viewPlayer : Model -> Html Msg
viewPlayer model =
    let
        item = getCurrentItem model
        playerClass = if item /= Nothing then "is-show" else ""
    in
        div
            [ class <| "player-wrap "  ++ playerClass ]
            [ div
                [ class "player" ] <|
                case item of
                    Just item' ->
                        [ div
                            [ class "player-control "]
                            [ div
                                [ class "player-buttons" ]
                                [
                                    if model.playerState == SoundLoading then
                                        div [ class "btn player-btn" ]
                                            [ img [ src "assets/loading-spin.svg" ] [] ]
                                    else if (model.playerState == Stopped || model.playerState == Paused) then
                                        button
                                            [ class "btn player-btn"
                                            , onClick (Play item')
                                            ]
                                            [ img [ src "assets/play.svg" ] [] ]
                                    else
                                        button
                                            [ class "btn player-btn"
                                            , onClick (Pause item')
                                            ]
                                            [ img [ src "assets/pause.svg" ] [] ]
                                ]
                            , div [ class "progress" ]
                                [ div
                                    [ class "player-title" ]
                                    [ marquee item'.title (model.playerState == Playing)
                                    ]
                                , progressBar item'.progress
                                ]
                            , div [ class "player-rate" ]
                                [ button
                                    [ class "btn"
                                    , onClick ToggleRate
                                    ]
                                    [ text <| (toString model.playerRate) ++ "X" ]
                                ]
                            , div [ class "player-vol" ]
                                [ div
                                    [ class "player-buttons" ]
                                    [ button
                                        [ class "btn player-btn"
                                        , onClick TogglePlayerMute
                                        ]
                                        [ if model.playerMute then
                                            img [ src "assets/volume-off.svg" ] []
                                          else
                                            img [ src "assets/volume-up.svg" ] []
                                        ]
                                    ]
                                , div
                                    [ class "player-vol-bar" ]
                                    [ div
                                        [ class "progress-bar"
                                        , onSetProgress SetVol
                                        ]
                                        [ div
                                            [ class "progress-bar-value"
                                            , style [("width", toString (model.playerVol * 100) ++ "%" )]
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            , div
                                [ class "player-progress" ]
                                [ text <|
                                    formatDuration item'.progress.current
                                    ++ "/"
                                    ++ formatDuration item'.progress.duration
                                ]
                            , div
                                [ class "player-close" ]
                                [ button
                                    [ class "btn"
                                    , onClick ClosePlayer
                                    ]
                                    [ img [ src "assets/close.svg"] [] ]
                                ]
                            -- , div
                            --     [ class "player-title "]
                            --     [ text item'.title ]
                            ]
                        ]
                    Nothing ->
                        [ text "" ]

            ]


marquee : String -> Bool -> Html Msg
marquee txt isPlaying =
    div
        [ class "player-title-text" ]
        [ div
            [ classList
                [ ("marquee-text", True)
                , ("is-playing", isPlaying) ] ]
            [ text txt ]
        ]
