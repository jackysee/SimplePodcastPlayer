module DateFormat exposing (format)

import Date exposing (..)
import Time exposing (Time)
import String


format : Time -> Time -> String
format time currentTime =
    let
        currentYear =
            Date.fromTime currentTime |> Date.year

        date =
            Date.fromTime time

        dateYear =
            Date.year date
    in
        String.join "" <|
            [ Date.day date
                |> toString
                |> String.padLeft 2 '0'
            , Date.month date
                |> printMonth
            ]
                ++ if dateYear /= currentYear then
                    [ toString currentYear |> String.dropLeft 2 ]
                   else
                    []


printMonth : Month -> String
printMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
