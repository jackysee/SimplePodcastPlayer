module Models exposing (..)

import Time exposing (Time)
import Dict
import ListUtil exposing (findFirst)


type alias Item =
    { title : String
    , pubDate : Time
    , link : Maybe String
    , url: String
    , description: Maybe String
    , duration : Time
    , progress : Time
    , playCount : Int
    , markPlayCount : Int
    }


type alias Feed =
    { url : String
    , title : String
    , items : List Item
    , state : FeedState
    , showConfirmDelete : Bool
    }


type alias Progress =
    { progress : Time
    , duration : Time
    }


type FeedState
    = Normal
    | Refreshing
    | RefreshError


type LoadFeedState
    = Empty
    | Loading
    | Error
    | AlreadyExist


type PlayerState
    = Stopped
    | Playing
    | Paused
    | SoundLoading


type ItemFilter
    = All
    | Queued
    | Unlistened


type FloatPanel
    = AddPanel
    | About AboutPanel
    | ItemDropdown String
    | Hidden


type AboutPanel
    = Credit
    | Shortcut
    | Settings


type FontSize
    = Small
    | Medium
    | Large


type Theme
    = Light
    | Dark


type alias Model =
    { urlToAdd : String
    , loadFeedState : LoadFeedState
    , playerState : PlayerState
    , list : List Feed
    , currentTime : Time
    , itemsToShow : Int
    , currentItemUrl : Maybe String
    , playerRate : Float
    , playerVol : Float
    , showFeedUrl : Maybe String
    , itemFilter: ItemFilter
    , itemSortLatest: Bool
    , itemSelected : Maybe String
    , playList: List String
    , shortcutGoTo: Bool
    , shortcutKeys : List String
    , floatPanel : FloatPanel
    , fallbackRssServiceUrl : Maybe String
    , fontSize : FontSize
    , playerShowTimeLeft : Bool
    , theme : Theme
    }


type alias StoreModel =
    { urlToAdd : String
    , list : List StoreFeed
    , itemsToShow : Int
    , currentItemUrl : Maybe String
    , playerRate : Float
    , playerVol : Float
    , itemFilter : String
    , itemSortLatest : Bool
    , playList: List String
    , fallbackRssServiceUrl : Maybe String
    , fontSize : String
    , playerShowTimeLeft : Bool
    , theme : String
    }

type alias StoreFeed =
    { url : String
    , title : String
    , items : List Item
    -- , state : UpdateFeedState
    }


type alias PlayLoad =
    { url : String
    , seek : Time
    , rate : Float
    , vol : Float
    }


isCurrent : String -> Model -> Bool
isCurrent itemUrl model =
    Just itemUrl == model.currentItemUrl


getCurrentItem : Model -> Maybe Item
getCurrentItem model =
    model.list
        |> List.concatMap (\feed -> feed.items)
        |> List.filter (\item -> isCurrent item.url model)
        |> List.head


getSelectedItem : Model -> Maybe Item
getSelectedItem model =
    model.list
        |> List.concatMap (\feed -> feed.items)
        |> List.filter (\item -> Just item.url == model.itemSelected)
        |> List.head


updateCurrentItem : (Item -> Item) -> Model -> Model
updateCurrentItem updater model =
    updateItem updater model.currentItemUrl model


updateItem : (Item -> Item) -> Maybe String -> Model -> Model
updateItem updater url model =
    { model | list =
        List.map (\feed ->
            { feed | items =
                List.map (\item ->
                    if Just item.url == url && url /= Nothing then
                        updater item
                    else
                        item
                )
                feed.items
            }
        )
        model.list
    }


toItemFilter : String -> ItemFilter
toItemFilter str =
    case str of
        "Unlistened" -> Unlistened
        "Queued" -> Queued
        _ -> All


itemFilterToStr : ItemFilter -> String
itemFilterToStr filter =
    case filter of
        Unlistened -> "Unlistened"
        Queued -> "Queued"
        All -> "All"


fontSizeToStr : FontSize -> String
fontSizeToStr fontSize =
    case fontSize of
        Small -> "Small"
        Medium -> "Medium"
        Large -> "Large"


toFontSize : String -> FontSize
toFontSize str =
    case str of
        "Small" -> Small
        "Large" -> Large
        _ -> Medium


themeToStr : Theme -> String
themeToStr theme =
    case theme of
        Light -> "Light"
        Dark -> "Dark"


toTheme : String -> Theme
toTheme str =
    case str of
        "Dark" -> Dark
        _ -> Light


toFeed : StoreFeed -> Feed
toFeed storeFeed =
    { url = storeFeed.url
    , title = storeFeed.title
    , items = storeFeed.items
    , state = Normal
    , showConfirmDelete = False
    }


toStoreModel : Model -> StoreModel
toStoreModel model =
    { urlToAdd = model.urlToAdd
    , list = List.map toStoreFeed model.list
    , itemsToShow = model.itemsToShow
    , currentItemUrl = model.currentItemUrl
    , playerRate = model.playerRate
    , playerVol = model.playerVol
    , itemFilter = itemFilterToStr model.itemFilter
    , itemSortLatest = model.itemSortLatest
    , playList = model.playList
    , fallbackRssServiceUrl = model.fallbackRssServiceUrl
    , fontSize = fontSizeToStr model.fontSize
    , playerShowTimeLeft = model.playerShowTimeLeft
    , theme = themeToStr model.theme
    }


toStoreFeed : Feed -> StoreFeed
toStoreFeed feed =
    { url = feed.url
    , title = feed.title
    , items = feed.items
    }


defaultModel : Model
defaultModel =
    { urlToAdd = ""
    , list = []
    , loadFeedState = Empty
    , currentTime = 0
    , itemsToShow = 30
    , currentItemUrl = Nothing
    , playerState = Stopped
    , playerRate = 1
    , playerVol = toFloat 1
    , showFeedUrl = Nothing
    , itemFilter = Unlistened
    , itemSelected = Nothing
    , itemSortLatest = True
    , playList = []
    , shortcutGoTo = False
    , shortcutKeys = []
    , floatPanel = Hidden
    , fallbackRssServiceUrl = Nothing
    , fontSize = Medium
    , playerShowTimeLeft = True
    , theme = Light
    }


fromStoreModel : StoreModel -> Model
fromStoreModel m =
    let
        feeds = List.map toFeed m.list
    in
        { defaultModel
            | floatPanel = initAddPanel feeds
            , urlToAdd = m.urlToAdd
            , list = feeds
            , currentItemUrl = m.currentItemUrl
            , playerRate = m.playerRate
            , playerVol = m.playerVol
            , itemFilter = toItemFilter m.itemFilter
            , playList = m.playList
            , fallbackRssServiceUrl = m.fallbackRssServiceUrl
            , fontSize = toFontSize m.fontSize
            , playerShowTimeLeft = m.playerShowTimeLeft
            , theme = toTheme m.theme
        }


initAddPanel : List feed -> FloatPanel
initAddPanel feeds =
    if List.length feeds  == 0 then
        AddPanel
    else
        Hidden


itemList: Model -> (List (Feed, Item), Bool)
itemList =
    itemListAll True


itemListAll : Bool -> Model -> (List (Feed, Item), Bool)
itemListAll limit model =
    case model.showFeedUrl of
        Just url' ->
            ( model.list
                |> List.filter (\f -> f.url == url' )
                |> itemsByDate model
            , False
            )

        Nothing ->
            if model.itemFilter == Queued then
                let
                    items = model.list
                        |> List.concatMap (\feed ->
                           List.map (\item -> (item.url, (feed, item))) feed.items
                        )
                        |> Dict.fromList
                    playListItem =  model.playList
                        |> List.filterMap (\url -> Dict.get url items)
                in
                    ( playListItem, False )
            else
                let
                    list = itemsByDate model model.list
                in
                    if limit then
                        ( List.take model.itemsToShow list
                        , List.length list > model.itemsToShow
                        )
                    else
                        ( list, False )


itemsByDate: Model -> List Feed -> List (Feed, Item)
itemsByDate model list =
    let
        list_ = list
            |> List.concatMap (\feed ->
                    List.map (\item -> (feed, item)) feed.items
                )
            |> List.filter (\(feed, item) ->
                    filterByItemFilter model item
                )
            |> List.sortBy (\(feed, item) -> item.pubDate)
    in
        if model.itemSortLatest then
            List.reverse list_
        else
            list_


filterByItemFilter : Model -> Item -> Bool
filterByItemFilter model item =
    case model.itemFilter of
        All -> True
        Unlistened ->
            item.playCount == 0
        Queued ->
            List.member item.url model.playList


getItemByUrl : Model -> String -> Maybe (Feed, Item)
getItemByUrl model url =
    model.list
        |> List.concatMap (\feed ->
            List.map (\item -> (feed, item)) feed.items
        )
        |> findFirst (\(feed, item) -> item.url == url)



getFontSizePx : FontSize -> String
getFontSizePx fontSize =
    case fontSize of
        Large -> "18px"
        Medium -> "16px"
        Small -> "12px"
