module DecodeFeed exposing (decodeFeed)

import Json.Decode as Json exposing ((:=))
import Json.Decode.Pipeline exposing
    ( decode, required, hardcoded, requiredAt, custom, nullable
    , optional )
import Models exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import DateFormat exposing (parseDuration)


decodeFeed : String -> Json.Decoder Feed
decodeFeed url =
    decode Feed
        |> hardcoded url
        |> requiredAt ["query", "results", "rss", "channel", "title"] Json.string
        |> requiredAt ["query", "results", "rss", "channel", "item"] (Json.list decodeItem)
        |> hardcoded Normal
        |> hardcoded False


decodeItem : Json.Decoder Item
decodeItem =
    decode Item
        |> required "title" Json.string
        |> required "pubDate" jsonDate
        |> custom (Json.maybe ("link" := Json.string))
        |> custom decodeEnclosure
        |> optional "duration" decodeDuration -1
        |> hardcoded -1
        |> hardcoded 0
        |> hardcoded -1

decodeEnclosure : Json.Decoder (Maybe String)
decodeEnclosure =
    Json.maybe
        ( Json.oneOf
            [ (Json.at ["enclosure", "url"] Json.string)
            , ("enclosure" := Json.list ("url" := Json.string))
                `Json.andThen`
                (\list ->
                    case List.head list of
                        Just first ->
                            Json.succeed first
                        Nothing ->
                            Json.fail  "cannot get enclosure"
                 )
            ]
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
