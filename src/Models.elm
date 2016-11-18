module Models exposing (..)

import Time exposing (Time)
import Dict
import ListUtil exposing (findFirst)
import Regex


type alias Item =
    { title : String
    , pubDate : Time
    , link : Maybe String
    , url : String
    , description : Maybe String
    , duration : Time
    , progress : Time
    , playCount : Int
    , markPlayCount : Int
    , feedUrl : String
    }


type alias Feed =
    { url : String
    , title : String
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
    | SoundError


type ListView
    = AllFeed
    | ViewFeed String
    | Queued


type ItemFilter
    = All
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


type alias ItemId =
    ( String, String )


type alias Setting =
    { fallbackRssServiceUrl : Maybe String
    , fontSize : FontSize
    , theme : Theme
    }


type alias View =
    { urlToAdd : String
    , loadFeedState : LoadFeedState
    , playerState : PlayerState
    , currentTime : Time
    , itemsToShow : Int
    , currentItem : Maybe ItemId
    , playerRate : Float
    , playerVol : Float
    , listView : ListView
    , itemFilter : ItemFilter
    , itemSortLatest : Bool
    , itemSelected : Maybe ItemId
    , playList : List ItemId
    , shortcutKeys : List String
    , floatPanel : FloatPanel
    , editingFeedTitle : Maybe String
    , playerShowTimeLeft : Bool
    }


type alias Model =
    { view : View
    , setting : Setting
    , feeds : List Feed
    , items : List Item
    }


type alias StoreView =
    -- { list : List StoreFeed
    { currentItem : Maybe ItemId
    , playerRate : Float
    , playerVol : Float
    , listView : String
    , itemFilter : String
    , itemSortLatest : Bool
    , playList : List ItemId
    , playerShowTimeLeft : Bool
    }


type alias StoreSetting =
    { fallbackRssServiceUrl : Maybe String
    , fontSize : String
    , theme : String
    }


type alias StoreFeed =
    { url : String
    , title : String
    }


type alias StoreModel =
    { view : StoreView
    , setting : StoreSetting
    , feeds : List StoreFeed
    , items : List Item
    }


type alias PlayLoad =
    { url : String
    , seek : Time
    , rate : Float
    , vol : Float
    }


isItemEqual : Maybe ItemId -> Item -> Bool
isItemEqual target item =
    Just ( item.url, item.feedUrl ) == target


isCurrent : Item -> Model -> Bool
isCurrent item model =
    isItemEqual model.view.currentItem item


getCurrentItem : Model -> Maybe Item
getCurrentItem model =
    model.items
        |> findFirst (\item -> isCurrent item model)


getSelectedItem : Model -> Maybe Item
getSelectedItem model =
    model.items
        |> findFirst (isItemEqual model.view.itemSelected)


updateCurrentItem : (Item -> Item) -> Model -> Model
updateCurrentItem updater model =
    updateItem updater model.view.currentItem model


updateItem : (Item -> Item) -> Maybe ItemId -> Model -> Model
updateItem updater currentItem model =
    { model
        | items =
            List.map
                (\item ->
                    if Just ( item.url, item.feedUrl ) == currentItem && currentItem /= Nothing then
                        updater item
                    else
                        item
                )
                model.items
    }


updateView : (View -> View) -> Model -> Model
updateView updater model =
    { model | view = updater model.view }


updateSetting : (Setting -> Setting) -> Model -> Model
updateSetting updater model =
    { model | setting = updater model.setting }


toItemFilter : String -> ItemFilter
toItemFilter str =
    case str of
        "Unlistened" ->
            Unlistened

        _ ->
            All


itemFilterToStr : ItemFilter -> String
itemFilterToStr filter =
    case filter of
        Unlistened ->
            "Unlistened"

        All ->
            "All"


fontSizeToStr : FontSize -> String
fontSizeToStr fontSize =
    case fontSize of
        Small ->
            "Small"

        Medium ->
            "Medium"

        Large ->
            "Large"


toFontSize : String -> FontSize
toFontSize str =
    case str of
        "Small" ->
            Small

        "Large" ->
            Large

        _ ->
            Medium


themeToStr : Theme -> String
themeToStr theme =
    case theme of
        Light ->
            "Light"

        Dark ->
            "Dark"


toTheme : String -> Theme
toTheme str =
    case str of
        "Dark" ->
            Dark

        _ ->
            Light


toFeed : StoreFeed -> Feed
toFeed storeFeed =
    { url = storeFeed.url
    , title =
        storeFeed.title
        -- , items = storeFeed.items
        -- , items = []
    , state = Normal
    , showConfirmDelete = False
    }


listViewToStr : ListView -> String
listViewToStr listView =
    case listView of
        AllFeed ->
            "AllFeed"

        Queued ->
            "Queued"

        ViewFeed url ->
            "ViewFeed::" ++ url


toListView : String -> ListView
toListView str =
    let
        regex =
            Regex.regex "^ViewFeed::"
    in
        if Regex.contains regex str then
            ViewFeed <| Regex.replace Regex.All regex (\_ -> "") str
        else if str == "Queued" then
            Queued
        else
            AllFeed


toStoreModel : Model -> StoreModel
toStoreModel model =
    { view = toStoreView model.view
    , setting = toStoreSetting model.setting
    , feeds = List.map toStoreFeed model.feeds
    , items = model.items
    }



{--list = List.map toStoreFeed model.list
    { currentItemUrl = model.currentItemUrl
    , playerRate = model.playerRate
    , playerVol = model.playerVol
    , listView = listViewToStr model.listView
    , itemFilter = itemFilterToStr model.itemFilter
    , itemSortLatest = model.itemSortLatest
    , playList = model.playList
    , fallbackRssServiceUrl = model.fallbackRssServiceUrl
    , fontSize = fontSizeToStr model.fontSize
    , playerShowTimeLeft = model.playerShowTimeLeft
    , theme = themeToStr model.theme
    }
    --}


toStoreView : View -> StoreView
toStoreView view =
    { currentItem = view.currentItem
    , playerRate = view.playerRate
    , playerVol = view.playerVol
    , listView = listViewToStr view.listView
    , itemFilter = itemFilterToStr view.itemFilter
    , itemSortLatest = view.itemSortLatest
    , playList = view.playList
    , playerShowTimeLeft = view.playerShowTimeLeft
    }


toStoreSetting : Setting -> StoreSetting
toStoreSetting setting =
    { fallbackRssServiceUrl = setting.fallbackRssServiceUrl
    , fontSize = fontSizeToStr setting.fontSize
    , theme = themeToStr setting.theme
    }


toStoreFeed : Feed -> StoreFeed
toStoreFeed feed =
    { url = feed.url
    , title =
        feed.title
        -- , items = feed.items
    }


defaultModel : Model
defaultModel =
    { view =
        { urlToAdd = ""
        , loadFeedState = Empty
        , currentTime = 0
        , itemsToShow = 30
        , currentItem = Nothing
        , playerState = Stopped
        , playerRate = 1
        , playerVol = toFloat 1
        , listView = AllFeed
        , itemFilter = Unlistened
        , itemSelected = Nothing
        , itemSortLatest = True
        , playList = []
        , shortcutKeys = []
        , floatPanel = Hidden
        , editingFeedTitle = Nothing
        , playerShowTimeLeft = True
        }
    , setting =
        { fallbackRssServiceUrl = Nothing
        , fontSize = Medium
        , theme = Light
        }
    , feeds = []
    , items = []
    }


fromStoreModel : StoreModel -> Model
fromStoreModel m =
    let
        defaultSetting =
            defaultModel.setting

        defaultView =
            defaultModel.view

        feeds =
            List.map toFeed m.feeds
    in
        { view =
            { defaultView
                | floatPanel = initAddPanel feeds
                , currentItem = m.view.currentItem
                , playerRate = m.view.playerRate
                , playerVol = m.view.playerVol
                , listView = toListView m.view.listView
                , itemFilter = toItemFilter m.view.itemFilter
                , itemSortLatest = m.view.itemSortLatest
                , playList = m.view.playList
                , playerShowTimeLeft = m.view.playerShowTimeLeft
            }
        , setting =
            { defaultSetting
                | fallbackRssServiceUrl = m.setting.fallbackRssServiceUrl
                , fontSize = toFontSize m.setting.fontSize
                , theme = toTheme m.setting.theme
            }
        , feeds = feeds
        , items = m.items
        }


initAddPanel : List feed -> FloatPanel
initAddPanel feeds =
    if List.length feeds == 0 then
        AddPanel
    else
        Hidden


itemList : Model -> ( List ( Feed, Item ), Bool )
itemList =
    itemListAll True


itemListAll : Bool -> Model -> ( List ( Feed, Item ), Bool )
itemListAll limit model =
    case model.view.listView of
        ViewFeed url_ ->
            let
                list =
                    itemsByDate model (getFeedByUrl model url_)
            in
                if limit then
                    ( List.take model.view.itemsToShow list
                    , List.length list > model.view.itemsToShow
                    )
                else
                    ( list, False )

        Queued ->
            let
                items =
                    model.items
                        |> List.filterMap
                            (\item ->
                                case getFeedByUrl model item.feedUrl of
                                    Just feed_ ->
                                        Just ( ( item.url, item.feedUrl ), ( feed_, item ) )

                                    Nothing ->
                                        Nothing
                            )
                        |> Dict.fromList

                playListItem =
                    model.view.playList
                        |> List.filterMap (\( url, feedUrl ) -> Dict.get ( url, feedUrl ) items)
            in
                ( playListItem, False )

        AllFeed ->
            let
                list =
                    itemsByDate model Nothing
            in
                if limit then
                    ( List.take model.view.itemsToShow list
                    , List.length list > model.view.itemsToShow
                    )
                else
                    ( list, False )


itemsByDate : Model -> Maybe Feed -> List ( Feed, Item )
itemsByDate model feed =
    let
        list_ =
            case feed of
                Just feed_ ->
                    model.items
                        |> List.filterMap
                            (\item ->
                                if item.feedUrl == feed_.url then
                                    Just ( feed_, item )
                                else
                                    Nothing
                            )

                Nothing ->
                    model.items
                        |> List.filterMap
                            (\item ->
                                case getFeedByUrl model item.feedUrl of
                                    Just feed__ ->
                                        Just ( feed__, item )

                                    Nothing ->
                                        Nothing
                            )

        list__ =
            list_
                |> List.filter
                    (\( feed, item ) ->
                        filterByItemFilter model item
                    )
                |> List.sortBy (\( feed, item ) -> item.pubDate)
    in
        if model.view.itemSortLatest then
            List.reverse list__
        else
            list__


filterByItemFilter : Model -> Item -> Bool
filterByItemFilter model item =
    case model.view.itemFilter of
        All ->
            True

        Unlistened ->
            item.playCount == 0


getItemByUrl : Model -> ItemId -> Maybe ( Feed, Item )
getItemByUrl model ( url, feedUrl ) =
    model.items
        |> findFirst (\item -> ( item.url, item.feedUrl ) == ( url, feedUrl ))
        |> Maybe.andThen
            (\item ->
                case getFeedByUrl model item.feedUrl of
                    Just feed ->
                        Just ( feed, item )

                    Nothing ->
                        Nothing
            )


getFeedByUrl : Model -> String -> Maybe Feed
getFeedByUrl model url =
    findFirst (\feed -> feed.url == url) model.feeds


getFontSizePx : FontSize -> String
getFontSizePx fontSize =
    case fontSize of
        Large ->
            "18px"

        Medium ->
            "16px"

        Small ->
            "12px"


inPlayList : Item -> Model -> Bool
inPlayList item model =
    List.member ( item.url, item.feedUrl ) model.view.playList
