module DateFormat exposing (format, formatDuration, parseDuration)

import Date exposing (..)
import Time exposing (Time)
import String
import Result

formatDuration: Time -> String
formatDuration time =
    if time < 0 then
        "--"
    else
        let
            hour = floor (time / (60*60))
            min = floor ((time - (toFloat hour*60*60)) / 60)
            sec = floor (time - (toFloat hour*60*60) - (toFloat min*60))
        in
            (if hour > 0 then
                (String.padLeft 2 '0' (toString hour)) ++ ":"
            else
                ""
            )
            ++ (String.padLeft 2 '0' (toString min)) ++ ":"
            ++ (String.padLeft 2 '0' (toString sec))


parseDuration : String -> Result String Time
parseDuration str =
    let
        list = String.split ":" str
            |> List.map String.toFloat
            |> List.reverse
            |> List.map2
                (\seconds part -> Result.map2 (*) seconds part)
                [Ok 1, Ok 60, Ok 3600]
        sum = (\list' ->
            case list' of
                [] -> Ok 0
                x::xs -> Result.map2 (+) x (sum xs)
        )
    in
        sum list


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
