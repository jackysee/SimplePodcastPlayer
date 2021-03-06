module DecodeStoreModel exposing (decodeStoreValue)

import Result
import Json.Decode as Json exposing (value, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, custom)
import Models exposing (..)


decodeStoreValue : Maybe Json.Value -> Maybe StoreModel
decodeStoreValue value =
    case value of
        Just value_ ->
            value_
                |> Json.decodeValue decodeStoreModel
                |> Result.toMaybe

        Nothing ->
            Nothing


decodeStoreModel : Json.Decoder StoreModel
decodeStoreModel =
    decode StoreModel
        |> required "view" decodeStoreView
        |> required "setting" decodeStoreSetting
        |> required "feeds" (Json.list decodeStoreFeed)
        |> required "items" (Json.list decodeStoreItem)


decodeStoreSetting : Json.Decoder StoreSetting
decodeStoreSetting =
    decode StoreSetting
        |> required "fallbackRssServiceUrl" (nullable Json.string)
        |> optional "fontSize" Json.string (fontSizeToStr defaultModel.setting.fontSize)
        |> optional "theme" Json.string (themeToStr defaultModel.setting.theme)


decodeStoreView : Json.Decoder StoreView
decodeStoreView =
    decode StoreView
        -- |> required "currentItem" (nullable Json.string)
        |> custom (Json.at [ "currentItem" ] (Json.maybe decodeItemId))
        |> optional "playerRate" Json.float defaultModel.view.playerRate
        |> optional "playerVol" Json.float defaultModel.view.playerVol
        |> optional "listView" Json.string (listViewToStr defaultModel.view.listView)
        |> optional "itemFilter" Json.string (itemFilterToStr defaultModel.view.itemFilter)
        |> optional "itemSortLatest" Json.bool (defaultModel.view.itemSortLatest)
        |> required "playList" (Json.list decodeItemId)
        |> optional "playerShowTimeLeft" Json.bool defaultModel.view.playerShowTimeLeft


decodeStoreFeed : Json.Decoder StoreFeed
decodeStoreFeed =
    decode StoreFeed
        |> required "url" Json.string
        |> required "title" Json.string
        |> required "link" (nullable Json.string)


decodeStoreItem : Json.Decoder Item
decodeStoreItem =
    decode Item
        |> required "title" Json.string
        |> required "pubDate" Json.float
        |> required "link" (nullable Json.string)
        |> required "url" Json.string
        |> required "description" (nullable Json.string)
        |> optional "duration" Json.float -1
        |> optional "progress" Json.float -1
        |> optional "playCount" Json.int 0
        |> optional "markPlayCount" Json.int -1
        |> required "feedUrl" Json.string


decodeItemId : Json.Decoder ItemId
decodeItemId =
    Json.list Json.string
        |> Json.andThen
            (\list ->
                case list of
                    x :: y :: [] ->
                        Json.succeed ( x, y )

                    _ ->
                        Json.fail "not tuple"
            )
