port module Player exposing (viewPlayer, updatePlayer, playError, paused, playEnd, soundLoaded, updateProgress, stop)

import Html exposing (Html, text, div, img, button, input)
import Html.Attributes exposing (class, style, src, classList, type_, value)
import Html.Events exposing (on, onClick, onInput)
import String
import Time exposing (Time)
import Models exposing (..)
import Msgs exposing (..)
import DateFormat exposing (formatDuration)
import Icons
import ListUtil exposing (findFirst, dropWhile)
import Return exposing (Return)
import Storage exposing (..)


updatePlayer : PlayerMsg -> Model -> Return Msg Model
updatePlayer msg model =
    case msg of
        Play item ->
            model
                |> updateView
                    (\view ->
                        { view
                            | currentItem = Just ( item.url, item.feedUrl )
                            , playerState = SoundLoading
                        }
                    )
                |> Return.singleton
                |> Return.command
                    (play
                        { url = item.url
                        , seek = item.progress
                        , rate = model.view.playerRate
                        , vol = model.view.playerVol
                        }
                    )
                |> Return.effect_ saveView

        SoundLoaded loaded ->
            model
                |> updateView (\v -> { v | playerState = Playing })
                |> Return.singleton

        Pause item ->
            model
                |> Return.singleton
                |> Return.command (pause "")

        Stop item ->
            model
                |> updateView (\v -> { v | playerState = Stopped })
                |> Return.singleton
                |> Return.command (stop "")

        UpdateProgress progress ->
            Return.singleton model
                |> Return.map
                    (updateCurrentItem
                        (\item ->
                            { item
                                | duration = progress.duration
                                , progress = progress.progress
                            }
                        )
                    )
                |> Return.command
                    (getCurrentItem model
                        |> Maybe.map (\item -> saveItems [ item ])
                        |> Maybe.withDefault Cmd.none
                    )

        SetProgress current ->
            case getCurrentItem model of
                Nothing ->
                    Return.singleton model

                Just item_ ->
                    let
                        item__ =
                            { item_ | progress = current }
                    in
                        Return.singleton model
                            |> Return.map (updateCurrentItem (\item -> item__))
                            |> Return.command (seek current)
                            |> Return.command (saveItems [ item__ ])

        ClosePlayer ->
            model
                |> updateView
                    (\v ->
                        { v
                            | playerState = Paused
                            , currentItem = Nothing
                        }
                    )
                |> Return.singleton
                |> Return.command (pause "")
                |> Return.effect_ saveView

        PlayError url ->
            model
                |> updateView (\v -> { v | playerState = SoundError })
                |> Return.singleton

        ToggleRate ->
            let
                rate =
                    [ 1, 1.12, 1.2, 1.5, 2.0 ]
                        |> dropWhile (\r -> r <= model.view.playerRate)
                        |> List.head
                        |> Maybe.withDefault 1
            in
                model
                    |> updateView (\v -> { v | playerRate = rate })
                    |> Return.singleton
                    |> Return.command (setRate rate)
                    |> Return.effect_ saveView

        SetVol percentage ->
            model
                |> updateView (\v -> { v | playerVol = percentage })
                |> Return.singleton
                |> Return.command (setVol percentage)
                |> Return.effect_ saveView

        SetPlayerShowTimeLeft show ->
            updateView (\v -> { v | playerShowTimeLeft = show }) model
                |> Return.singleton
                |> Return.effect_ saveView

        PlayerPaused stopped ->
            Return.singleton <|
                case getCurrentItem model of
                    Just item ->
                        updateView (\v -> { v | playerState = Paused }) model

                    Nothing ->
                        model


range : Float -> Float -> Float -> Float -> Bool -> (Float -> msg) -> Html msg
range vmin vmax step val disabled msg =
    let
        fraction =
            val / vmax
    in
        div
            [ class "range-wrap" ]
            [ if not disabled then
                input
                    [ type_ "range"
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
                    , style [ ( "flex", (toString fraction) ++ " 1 0%" ) ]
                    ]
                    []
                , div
                    [ class "range-progress-right"
                    , style [ ( "flex", (toString (1 - fraction)) ++ " 1 0%" ) ]
                    ]
                    []
                ]
            ]


progressBar : Time -> Time -> Html Msg
progressBar progress duration =
    if duration == -1 then
        range 0 100 1 0 True (\_ -> NoOp)
    else
        range 0 duration 1 progress False (Player << SetProgress)


setFloat : (Float -> msg) -> String -> msg
setFloat msg_ input =
    input
        |> String.toFloat
        |> Result.withDefault 0
        |> msg_


getCurrentItem : Model -> Maybe Item
getCurrentItem model =
    model.items
        |> findFirst (\item -> isCurrent item model)


viewPlayer : Model -> Html Msg
viewPlayer model =
    let
        item =
            getCurrentItem model

        playerClass =
            if item /= Nothing then
                "is-show"
            else
                ""
    in
        div
            [ class <| "player-wrap " ++ playerClass ]
            [ div
                [ class "player" ]
              <|
                case item of
                    Just item_ ->
                        [ div
                            [ class "player-control " ]
                            [ div
                                [ class "player-buttons" ]
                                [ if model.view.playerState == SoundLoading then
                                    div [ class "btn player-btn" ]
                                        [ Icons.loadingSpin ]
                                  else if (model.view.playerState == Stopped || model.view.playerState == Paused || model.view.playerState == SoundError) then
                                    button
                                        [ class "btn player-btn"
                                        , onClick (Player <| Play item_)
                                        ]
                                        [ Icons.play ]
                                  else
                                    button
                                        [ class "btn player-btn"
                                        , onClick (Player <| Pause item_)
                                        ]
                                        [ Icons.pause ]
                                , div
                                    [ class "btn player-btn player-btn-restart "
                                    , onClick (Player <| SetProgress 0)
                                    ]
                                    [ Icons.angleDoubleLeft ]
                                ]
                            , div [ class "progress player-time-progress" ]
                                [ div
                                    [ class "player-title" ]
                                    [ if model.view.playerState == SoundError then
                                        div
                                            [ class "player-title-text player-error" ]
                                            [ text "Cannot load the file! " ]
                                      else
                                        text ""
                                    , div
                                        [ class "player-title-text" ]
                                        [ marquee item_.title (model.view.playerState == Playing)
                                        ]
                                    ]
                                , progressBar item_.progress item_.duration

                                --, div
                                --    [ class "player-item-queued-info" ]
                                --    [ let
                                --        currentInQueue =
                                --            case model.view.currentItem of
                                --                Just item_ ->
                                --                    List.member item_ model.view.playList
                                --                Nothing ->
                                --                    False
                                --      in
                                --        if model.view.playerState == Playing then
                                --            if currentInQueue then
                                --                text "Playing queued items"
                                --            else if List.length model.view.playList > 0 then
                                --                text "Queued items will be played next."
                                --            else
                                --                text ""
                                --        else
                                --            text ""
                                --    ]
                                ]
                            , div [ class "player-rate" ]
                                [ button
                                    [ class "btn"
                                    , onClick <| Player ToggleRate
                                    ]
                                    [ text <| (toString model.view.playerRate) ++ "X" ]
                                ]
                            , div [ class "player-vol" ]
                                [ div
                                    [ class "player-buttons" ]
                                    [ button
                                        [ class "btn player-btn" ]
                                        [ if model.view.playerVol == 0 then
                                            Icons.volumeOff
                                          else if model.view.playerVol <= 0.5 then
                                            Icons.volume1
                                          else
                                            Icons.volume2
                                        ]
                                    ]
                                , div
                                    [ class "player-vol-bar" ]
                                    [ range 0 1 0.01 model.view.playerVol False (Player << SetVol) ]
                                ]
                            , div
                                [ class "player-progress" ]
                                [ button
                                    [ class "btn"
                                    , onClick (Player (SetPlayerShowTimeLeft <| not model.view.playerShowTimeLeft))
                                    ]
                                    [ text <|
                                        let
                                            progress_ =
                                                if item_.progress == -1 then
                                                    0
                                                else
                                                    item_.progress
                                        in
                                            if model.view.playerShowTimeLeft then
                                                "-" ++ formatDuration (item_.duration - progress_)
                                            else
                                                formatDuration progress_
                                    ]
                                ]
                            , div
                                [ class "player-close" ]
                                [ button
                                    [ class "btn btn-icon"
                                    , onClick <| Player ClosePlayer
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
        [ classList
            [ ( "marquee-text", True )
            , ( "is-playing", isPlaying )
            ]
        ]
        [ text txt ]


port play : PlayLoad -> Cmd msg


port stop : String -> Cmd msg


port pause : String -> Cmd msg


port updateProgress : (Progress -> msg) -> Sub msg


port soundLoaded : (Bool -> msg) -> Sub msg


port seek : Float -> Cmd msg


port playEnd : (String -> msg) -> Sub msg


port setRate : Float -> Cmd msg


port setVol : Float -> Cmd msg


port playError : (String -> msg) -> Sub msg


port paused : (Bool -> msg) -> Sub msg
