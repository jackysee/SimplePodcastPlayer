module DecodePosition exposing (decodeLeft, decodeLeftPercentage, decodeBottomRight)

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
        (\left width -> left / width)
        decodeLeft
        ( currentTarget ( "offsetWidth" := Decode.float ) )


decodeLeft : Decoder Float
decodeLeft =
    Decode.object2
        (\pageX posLeft -> pageX - posLeft)
        ( "pageX" := Decode.float )
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

positionTop: Float -> Decoder Float
positionTop y =
    Decode.object2
        (\scrollTop offsetTop ->
            y + offsetTop - scrollTop
        )
        ("scrollTop" := Decode.float)
        ("offsetTop" := Decode.float)
    `Decode.andThen`
        (\y' ->
            offsetParent y' (positionTop y')
        )


decodeBottomRight: Decoder (Float, Float)
decodeBottomRight =
    Decode.object4
        (\posLeft posTop w h ->
            (posLeft + w, posTop + h)
        )
        ( currentTarget (positionLeft 0) )
        ( currentTarget (positionTop 0) )
        ( currentTarget ("offsetWidth" := Decode.float) )
        ( currentTarget ("offsetHeight" := Decode.float) )
