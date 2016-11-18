module DecodePosition exposing (decodeLeft, decodeLeftPercentage, decodeBottomRight)

import Json.Decode as Decode exposing (Decoder)


target : Decoder a -> Decoder a
target decoder =
    Decode.field "target" decoder


currentTarget : Decoder a -> Decoder a
currentTarget decoder =
    Decode.field "currentTarget" decoder


decodeLeftPercentage : Decoder Float
decodeLeftPercentage =
    Decode.map2
        (\left width -> left / width)
        decodeLeft
        (currentTarget (Decode.field "offsetWidth" Decode.float))


decodeLeft : Decoder Float
decodeLeft =
    Decode.map2
        (\pageX posLeft -> pageX - posLeft)
        (Decode.field "pageX" Decode.float)
        (currentTarget (positionLeft 0))


offsetParent : a -> Decoder a -> Decoder a
offsetParent x decoder =
    Decode.oneOf
        [ Decode.field "offsetParent" (Decode.null x)
        , Decode.field "offsetParent" decoder
        ]


positionLeft : Float -> Decoder Float
positionLeft x =
    Decode.map2
        (\scrollLeft offsetLeft ->
            x + offsetLeft - scrollLeft
        )
        (Decode.field "scrollLeft" Decode.float)
        (Decode.field "offsetLeft" Decode.float)
        |> Decode.andThen
            (\x_ ->
                offsetParent x_ (positionLeft x_)
            )


positionTop : Float -> Decoder Float
positionTop y =
    Decode.map2
        (\scrollTop offsetTop ->
            y + offsetTop - scrollTop
        )
        (Decode.field "scrollTop" Decode.float)
        (Decode.field "offsetTop" Decode.float)
        |> Decode.andThen
            (\y_ ->
                offsetParent y_ (positionTop y_)
            )


decodeBottomRight : Decoder ( Float, Float )
decodeBottomRight =
    Decode.map4
        (\posLeft posTop w h ->
            ( posLeft + w, posTop + h )
        )
        (currentTarget (positionLeft 0))
        (currentTarget (positionTop 0))
        (currentTarget (Decode.field "offsetWidth" Decode.float))
        (currentTarget (Decode.field "offsetHeight" Decode.float))
