module Player exposing (viewPlayer)

import Html exposing (Html, text, div, img, button, input)
import Html.Attributes exposing (class, style, src, classList, type', value)
import Html.Events exposing (on, onClick, onInput)
import String
import Time exposing (Time)

import Models exposing (..)
import Msgs exposing (..)
import DateFormat exposing (formatDuration)
import Icons

range: Float -> Float -> Float -> Float -> Bool -> (Float -> msg) -> Html msg
range vmin vmax step val disabled msg =
    let
        fraction = val / vmax
    in
        div
            [ class "range-wrap" ]
            [ if not disabled then 
                input
                    [ type' "range"
                    , Html.Attributes.min (toString vmin)
                    , Html.Attributes.max (toString vmax)
                    , Html.Attributes.step (toString step)
                    , value (toString val)
                    , onInput (setFloat msg)
                    ]
                    []
              else 
                text ""
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
        range 0 100 1 0 True (\_ -> NoOp)
    else
        range 0 duration 1 progress False SetProgress


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
                    Just item_ ->
                        [ div
                            [ class "player-control "]
                            [ div
                                [ class "player-buttons" ]
                                [
                                    if model.playerState == SoundLoading then
                                        div [ class "btn player-btn" ]
                                            [ Icons.loadingSpin ]
                                    else if (model.playerState == Stopped || model.playerState == Paused) then
                                        button
                                            [ class "btn player-btn"
                                            , onClick (Play item_)
                                            ]
                                            [ Icons.play ]
                                    else
                                        button
                                            [ class "btn player-btn"
                                            , onClick (Pause item_)
                                            ]
                                            [ Icons.pause ]
                                ]
                            , div [ class "progress" ]
                                [ div
                                    [ class "player-title" ]
                                    [ marquee item_.title (model.playerState == Playing)
                                    ]
                                , progressBar item_.progress item_.duration
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
                                            Icons.volumeOff
                                          else
                                            Icons.volumeUp
                                        ]
                                    ]
                                , div
                                    [ class "player-vol-bar" ]
                                    [ range 0 1 0.01 model.playerVol False SetVol ]
                                ]
                            , div
                                [ class "player-progress"
                                , onClick (SetPlayerShowTimeLeft <| not model.playerShowTimeLeft)
                                ]
                                [ text <|
                                    let
                                        progress_ =
                                            if item_.progress == -1 then
                                                0
                                            else
                                                item_.progress
                                    in
                                        if model.playerShowTimeLeft then
                                            "-" ++ formatDuration (item_.duration - progress_)
                                        else
                                            formatDuration progress_
                                ]
                            , div
                                [ class "player-close" ]
                                [ button
                                    [ class "btn btn-icon"
                                    , onClick ClosePlayer
                                    ]
                                    [ Icons.close ]
                                ]
                            -- , div
                            --     [ class "player-title "]
                            --     [ text item_.title ]
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
