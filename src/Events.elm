module Events exposing (..)

import Html
import Html.Events exposing (on, onWithOptions, keyCode, targetValue)
import DecodePosition exposing (decodeBottomRight)
import Json.Decode as Json
import Dict

import Msgs exposing (Msg(..))

onKeydown : List (Int, (Int -> Msg)) -> Html.Attribute Msg
onKeydown msgs =
    let
        codeMap = Dict.fromList msgs
    in
        on "keydown" <|
            Json.map
                (\code ->
                    case Dict.get code codeMap of
                        Just msg ->
                            msg code
                        Nothing ->
                            NoOp
                )
                keyCode


onInternalClick : Msg -> Html.Attribute Msg
onInternalClick msg =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.succeed msg)


onClickPosBottomRight : ((Float, Float) -> Msg) -> Html.Attribute Msg
onClickPosBottomRight msg =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.map
            (\(x, y) -> msg (x, y))
            decodeBottomRight
        )


onScroll: Msg -> Html.Attribute Msg
onScroll msg =
    on "scroll" (Json.succeed msg)
