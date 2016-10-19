module Events exposing (..)

import Html
import Html.Events exposing (on, onWithOptions, keyCode)
import DecodePosition exposing (decodeBottomRight)
import Json.Decode as Json

import Msgs exposing (Msg(..))


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    on "keydown" <|
        Json.map
            (\code ->
                if code == 13 then
                    msg
                else
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

{-- TODO mouseleave fix
onMouseLeave : Msg -> Html.Attribute Msg
onMouseLeave msg =
    on "mouseleave" <|
        Json.map 
            (\within ->
                if within then
                    msg
                else
                    NoOp
            )
            Json.oneOf
                [ Json.at ["relatedTarget"] Json.bool
                , Json.succeed False
                ]
--}
