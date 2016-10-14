module DecodeFeed exposing (decodeFeed)

import Models exposing (..)
import Json.Decode as Json exposing ((:=))
import Date exposing (Date)
import Time exposing (Time)
import DateFormat exposing (parseDuration)


decodeFeed : String -> Json.Decoder Feed
decodeFeed url =
    Json.object5 Feed
        (Json.succeed url)
        (Json.at [ "query", "results", "rss", "channel", "title" ] Json.string)
        (Json.at [ "query", "results", "rss", "channel", "item" ] (Json.list decodeItem))
        (Json.succeed Normal)
        (Json.succeed False)


decodeItem : Json.Decoder Item
decodeItem =
    Json.object6 Item
        ("title" := Json.string)
        ("pubDate" := jsonDate)
        (Json.maybe ("link" := Json.string))
        (Json.oneOf
            [ (Json.at ["enclosure", "url"] Json.string)
                `Json.andThen`
                (\url -> Json.succeed (Just url))
            , ("enclosure" := Json.list
                ( Json.maybe ("url" := Json.string))
              )
              `Json.andThen`
                (\list ->
                    case List.head (Debug.log "list" list)  of
                        Just first ->
                            Json.succeed first
                        Nothing ->
                            Json.fail "fail to get enclosures"
                )
             , Json.succeed Nothing
            ]
        )
        (Json.succeed False)
        (Json.object2 Progress
            (Json.oneOf
                [ "duration" := decodeDuration
                , Json.succeed -1
                ]
            )
            (Json.succeed -1)
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
