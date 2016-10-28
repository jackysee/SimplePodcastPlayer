module DecodeFeed exposing (decodeFeed)

import Json.Decode as Json exposing ((:=))
import Json.Decode.Pipeline exposing
    ( decode, required, hardcoded, requiredAt, custom, optional )
import Models exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import DateFormat exposing (parseDuration)
import Regex


decodeFeed : String -> Json.Decoder Feed
decodeFeed url =
    decode Feed
        |> hardcoded url
        |> requiredAt ["query", "results", "rss", "channel", "title"] Json.string
        |> requiredAt ["query", "results", "rss", "channel", "item"]
            (Json.list decodeItem
                `Json.andThen`
                    (\list -> list
                        |> List.filterMap identity
                        |> Json.succeed
                    )
            )
        |> hardcoded Normal
        |> hardcoded False


decodeItem : Json.Decoder (Maybe Item)
decodeItem =
    Json.maybe
        ( decode Item
            |> required "title" Json.string
            |> required "pubDate" jsonDate
            |> custom (Json.maybe ("link" := Json.string))
            |> custom decodeEnclosure
            |> optional "duration" decodeDuration -1
            |> hardcoded -1
            |> hardcoded 0
            |> hardcoded -1
        )


decodeEnclosure : Json.Decoder String
decodeEnclosure =
    Json.oneOf
        [ "enclosure" := decodeSingleEnclosureUrl
        , "enclosure" := decodeEnclosureListUrl
        ]


decodeSingleEnclosureUrl : Json.Decoder String
decodeSingleEnclosureUrl =
    Json.object2 (\url _ -> url)
        ("url" := Json.string)
        ("type" := Json.string
            `Json.andThen`
             (\type_ ->
                if Regex.contains (Regex.regex "^audio/") type_ then
                    Json.succeed type_
                else
                    Json.fail "not audio type"
            )
        )

decodeEnclosureListUrl : Json.Decoder String
decodeEnclosureListUrl =
    (Json.list (Json.maybe decodeSingleEnclosureUrl))
    `Json.andThen`
        (\list ->
            list
                |> List.filterMap identity
                |> List.head
                |> Maybe.map Json.succeed
                |> Maybe.withDefault (Json.fail "no audio found")
        )


jsonDate : Json.Decoder Time
jsonDate =
    Json.string
        `Json.andThen`
            \val ->
                case Date.fromString val of
                    Ok date ->
                        Json.succeed (Date.toTime date)

                    Err error ->
                        Json.fail "not a correct date string"


decodeDuration: Json.Decoder Time
decodeDuration =
    Json.string `Json.andThen`
        \val ->
            case parseDuration val of
                Ok value ->
                    Json.succeed value
                Err error ->
                    Json.fail "not a correct duration"
