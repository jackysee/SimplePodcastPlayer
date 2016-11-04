module Player exposing (viewPlayer)

import Html exposing (Html, text, div, img, button, input)
import Html.Attributes exposing (class, style, src, classList, type', min, max, value)
import Html.Events exposing (on, onClick, onInput)

import Models exposing (..)
import Msgs exposing (..)
import DateFormat exposing (formatDuration)
import String
import Time exposing (Time)

range: Float -> Float -> Float -> Float -> (Float -> msg) -> Html msg
range min max step value' msg =
    let
        fraction = value' / max
    in
        div
            [ class "range-wrap" ]
            [ input
                [ type' "range"
                , Html.Attributes.min (toString min)
                , Html.Attributes.max (toString max)
                , Html.Attributes.step (toString step)
                , value (toString value')
                , onInput (setFloat msg)
                ]
                []
            , div
                [ class "range-progress" ]
                [ div
                    [ class "range-progress-left"
                    , style [("flex" , (toString fraction) ++ " 1 0%")]
                    ]
                    []
                , div
                    [ class "range-progress-right"
                    , style [("flex" , (toString (1 - fraction)) ++ " 1 0%")]
                    ]
                    []
                ]
            ]


progressBar : Time -> Time -> Html Msg
progressBar progress duration =
    if duration == -1 then
        text ""
    else
        range 0 duration 1 progress SetProgress


setFloat: (Float -> msg) -> String -> msg
setFloat msg' input =
    input
        |> String.toFloat
        |> Result.withDefault 0
        |> msg'


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
                                , progressBar item'.progress item'.duration
                                , div
                                    [ class "player-item-queued-info" ]
                                    [ let
                                        currentInQueue = List.member
                                            (Maybe.withDefault "" model.currentItemUrl)
                                            model.playList
                                      in
                                        if model.playerState == Playing then
                                            if currentInQueue  then
                                                text "Playing queued items"
                                            else if List.length model.playList > 0 then
                                                text "Queued items will be played next."
                                            else
                                                text ""
                                        else
                                            text ""
                                    ]
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
                                        [ class "btn player-btn" ]
                                        [ if model.playerVol == 0 then
                                            img [ src "assets/volume-off.svg" ] []
                                          else
                                            img [ src "assets/volume-up.svg" ] []
                                        ]
                                    ]
                                , div
                                    [ class "player-vol-bar" ]
                                    [ range 0 1 0.01 model.playerVol SetVol ]
                                ]
                            , div
                                [ class "player-progress" ]
                                [ text <| "-"
                                    ++ formatDuration (item'.duration - item'.progress)
                                ]
                            , div
                                [ class "player-close" ]
                                [ button
                                    [ class "btn btn-icon"
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
