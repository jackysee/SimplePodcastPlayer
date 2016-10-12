module DecodePosition exposing (decodeLeft, decodeLeftPercentage)

import Json.Decode as Decode exposing ((:=), Decoder)

target : Decoder a -> Decoder a
target decoder =
    "target" := decoder


currentTarget : Decoder a -> Decoder a
currentTarget decoder =
    "currentTarget" := decoder


decodeLeftPercentage : Decoder Float
decodeLeftPercentage =
    Decode.object2
        (\left width -> left / width )
        decodeLeft
        ( currentTarget ( "offsetWidth" := Decode.float ) )


decodeLeft : Decoder Float
decodeLeft =
    Decode.object2
        (\clientX posX -> clientX + posX )
        ( "clientX" := Decode.float )
        ( currentTarget (positionLeft 0) )


offsetParent : a -> Decoder a -> Decoder a
offsetParent x decoder =
    Decode.oneOf
    [ "offsetParent" := Decode.null x
    , "offsetParent" := decoder
    ]


positionLeft : Float -> Decoder Float
positionLeft x =
    Decode.object2
        (\scrollLeft offsetLeft ->
            x + offsetLeft - scrollLeft
        )
        ("scrollLeft" := Decode.float)
        ("offsetLeft" := Decode.float)
    `Decode.andThen`
        (\x' ->
            offsetParent x' (positionLeft x')
        )
