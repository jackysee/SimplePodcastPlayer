module DecodeFeed exposing (decodeYqlFeed, decodeCustomFeed)

import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, required, hardcoded, requiredAt, custom, optional)
import Models exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import DateFormat exposing (parseDuration)
import Regex


decodeFeed : List String -> String -> Json.Decoder ( Feed, List Item )
decodeFeed paths url =
    decode
        (\title link items ->
            ( { url = url
              , title = title
              , state = Normal
              , showConfirmDelete = False
              , link = link
              }
            , items
            )
        )
        |> requiredAt (paths ++ [ "title" ]) Json.string
        |> custom
            (Json.oneOf
                [ Json.at (paths ++ [ "link" ]) decodeLink
                , Json.at (paths ++ [ "url" ]) decodeLink
                ]
            )
        |> requiredAt (paths ++ [ "item" ])
            (Json.list (Json.maybe (decodeItem url))
                |> Json.andThen
                    (\list ->
                        list
                            |> List.filterMap identity
                            |> Json.succeed
                    )
            )


decodeYqlFeed : String -> Json.Decoder ( Feed, List Item )
decodeYqlFeed =
    decodeFeed [ "query", "results", "rss", "channel" ]


decodeCustomFeed : String -> Json.Decoder ( Feed, List Item )
decodeCustomFeed =
    decodeFeed []


decodeLink : Json.Decoder (Maybe String)
decodeLink =
    Json.maybe <|
        Json.oneOf
            [ Json.string
            , decodeLinkList
            ]


decodeLinkList : Json.Decoder String
decodeLinkList =
    Json.list
        (Json.oneOf
            [ Json.maybe Json.string
            , Json.maybe (Json.field "href" Json.string)
            ]
        )
        |> Json.andThen
            (\list ->
                list
                    |> List.filterMap identity
                    |> List.head
                    |> Maybe.map Json.succeed
                    |> Maybe.withDefault (Json.fail "no href found")
            )


decodeItem : String -> Json.Decoder Item
decodeItem feedUrl =
    decode Item
        |> required "title" stringListHead
        |> required "pubDate" jsonDate
        |> custom (Json.maybe (Json.field "link" Json.string))
        |> custom decodeEnclosure
        |> custom (Json.maybe decodeDescription)
        |> optional "duration" decodeDuration -1
        |> hardcoded -1
        |> hardcoded 0
        |> hardcoded -1
        |> hardcoded feedUrl


stringListHead : Json.Decoder String
stringListHead =
    Json.oneOf
        [ Json.string
        , Json.list Json.string
            |> Json.andThen
                (\list ->
                    list
                        |> List.head
                        |> Maybe.map Json.succeed
                        |> Maybe.withDefault (Json.fail "no string")
                )
        ]


decodeDescription : Json.Decoder String
decodeDescription =
    Json.oneOf
        [ Json.field "summary" Json.string
        , Json.field "description" Json.string
        ]


decodeEnclosure : Json.Decoder String
decodeEnclosure =
    Json.oneOf
        [ Json.field "enclosure" decodeSingleEnclosureUrl
        , Json.field "enclosure" decodeEnclosureListUrl
        ]


decodeSingleEnclosureUrl : Json.Decoder String
decodeSingleEnclosureUrl =
    Json.map2 (\url _ -> url)
        (Json.field "url" Json.string)
        (Json.field "type" Json.string
            |> Json.andThen
                (\type_ ->
                    if Regex.contains (Regex.regex "^audio/") type_ then
                        Json.succeed type_
                    else
                        Json.fail "not audio type"
                )
        )


decodeEnclosureListUrl : Json.Decoder String
decodeEnclosureListUrl =
    Json.list (Json.maybe decodeSingleEnclosureUrl)
        |> Json.andThen
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
        |> Json.andThen
            (\val ->
                case Date.fromString val of
                    Ok date ->
                        Json.succeed (Date.toTime date)

                    Err error ->
                        Json.fail "not a correct date string"
            )


decodeDuration : Json.Decoder Time
decodeDuration =
    Json.string
        |> Json.andThen
            (\val ->
                case parseDuration val of
                    Ok value ->
                        Json.succeed value

                    Err error ->
                        Json.fail "not a correct duration"
            )
