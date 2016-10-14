module Events exposing (..)

import Html
import Html.Events exposing (on, onWithOptions, keyCode)
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
