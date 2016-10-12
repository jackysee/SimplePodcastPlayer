module DecodeFeed exposing (decodeFeed)

import Models exposing (..)
import Json.Decode as Json exposing ((:=))
import Date exposing (Date)
import Time exposing (Time)


decodeFeed : String -> Json.Decoder Feed
decodeFeed url =
    Json.object4 Feed
        (Json.succeed url)
        (Json.at [ "query", "results", "rss", "channel", "title" ] Json.string)
        (Json.at [ "query", "results", "rss", "channel", "item" ] (Json.list decodeItem))
        (Json.succeed Normal)


decodeItem : Json.Decoder Item
decodeItem =
    Json.object6 Item
        ("title" := Json.string)
        ("pubDate" := jsonDate)
        (Json.maybe ("link" := Json.string))
        (Json.maybe (Json.at [ "enclosure", "url" ] Json.string))
        (Json.succeed False)
        (Json.succeed { current = -1 , duration = -1 })


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


-- decodeGuid : Json.Decoder String
-- decodeGuid =
--     Json.oneOf
--         [ (Json.at [ "guid", "content" ] Json.string)
--         , (Json.at [ "enclosure", "url" ] Json.string)
--         ]


-- decodeEnclosure : Json.Decoder Enclosure
-- decodeEnclosure =
--     Json.object2 Enclosure
--         ("type" := Json.string)
--         ("url" := Json.string)
