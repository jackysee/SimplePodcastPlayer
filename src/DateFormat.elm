module DateFormat exposing (format, formatDuration, formatDurationShort, parseDuration)

import Date exposing (..)
import Time exposing (Time)
import String
import Result


formatDuration : Time -> String
formatDuration time =
    if time < 0 then
        "--"
    else
        let
            hour =
                floor (time / (60 * 60))

            min =
                floor ((time - (toFloat hour * 60 * 60)) / 60)

            sec =
                floor (time - (toFloat hour * 60 * 60) - (toFloat min * 60))
        in
            (if hour > 0 then
                -- (String.padLeft 2 '0' (toString hour)) ++ ":"
                toString hour ++ ":"
             else
                ""
            )
                ++ (String.padLeft 2 '0' (toString min))
                ++ ":"
                ++ (String.padLeft 2 '0' (toString sec))


formatDurationShort : Time -> String
formatDurationShort time =
    if time < 0 then
        "--"
    else if time < 60 then
        toString time ++ "s"
    else if time < 60 * 60 then
        roundDp 0 (time / 60) ++ "m"
    else
        roundDp 1 (time / (60 * 60)) ++ "h"


roundDp : Int -> Float -> String
roundDp dp num =
    let
        factor =
            10 ^ dp |> toFloat
    in
        toFloat (ceiling (num * factor))
            / factor
            |> toString


parseDuration : String -> Result String Time
parseDuration str =
    let
        list =
            String.split ":" str
                |> List.map String.toFloat
                |> List.reverse
                |> List.map2
                    (\seconds part -> Result.map2 (*) seconds part)
                    [ Ok 1, Ok 60, Ok 3600 ]

        sum =
            (\list_ ->
                case list_ of
                    [] ->
                        Ok 0

                    x :: xs ->
                        Result.map2 (+) x (sum xs)
            )
    in
        sum list


format : Time -> Time -> Bool -> String
format time currentTime showYear =
    let
        currentYear =
            Date.fromTime currentTime |> Date.year

        date =
            Date.fromTime time

        dateYear =
            Date.year date
    in
        String.join "" <|
            [ Date.day date |> toString
            , "/"
            , Date.month date |> printMonthInNum
            , if showYear then
                "/" ++ (toString dateYear)
              else
                ""
            ]


printMonthInNum : Month -> String
printMonthInNum month =
    case month of
        Jan ->
            "1"

        Feb ->
            "2"

        Mar ->
            "3"

        Apr ->
            "4"

        May ->
            "5"

        Jun ->
            "6"

        Jul ->
            "7"

        Aug ->
            "8"

        Sep ->
            "9"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


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
