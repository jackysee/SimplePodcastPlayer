module Models exposing (..)

import Time exposing (Time)


type alias Item =
    { title : String
    , pubDate : Time
    , link : Maybe String
    , url: String
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


type alias Model =
    { showAddPanel : Bool
    , urlToAdd : String
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
    , itemDropdown : Maybe String
    , itemSelected : Maybe String
    , showAbout : Bool
    , playList: List String
    }


type alias StoreModel =
    -- { showAddPanel : Bool
    { urlToAdd : String
    -- , loadFeedState : LoadFeedState
    , list : List StoreFeed
    -- , currentTime : Time
    , itemsToShow : Int
    , currentItemUrl : Maybe String
    , playerRate : Float
    -- , playerState : PlayerState
    , playerVol : Float
    , itemFilter : String
    , playList: List String
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
    , playList = model.playList
    }



toStoreFeed : Feed -> StoreFeed
toStoreFeed feed =
    { url = feed.url
    , title = feed.title
    , items = feed.items
    }


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
                           List.map (\item -> (feed, item)) feed.items
                        )
                    playListItem =  model.playList
                        |> List.filterMap (\url -> findItemByUrl url items)
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


findItemByUrl: String -> List (Feed, Item) -> Maybe (Feed, Item)
findItemByUrl url list =
    case list of
        [] -> 
            Nothing

        (feed, item)::xs ->
            if item.url == url then
                Just (feed, item)
            else
                findItemByUrl url xs


itemsByDate: Model -> List Feed -> List (Feed, Item)
itemsByDate model list =
    list
        |> List.concatMap (\feed ->
                List.map (\item -> (feed, item)) feed.items
            )
        |> List.filter (\(feed, item) ->
                filterByItemFilter model item
            )
        |> List.sortBy (\(feed, item) -> item.pubDate)
        |> List.reverse


filterByItemFilter : Model -> Item -> Bool
filterByItemFilter model item =
    case model.itemFilter of
        All -> True
        Unlistened ->
            item.playCount == 0
        Queued ->
            List.member item.url model.playList
            -- item.progress > -1 && item.playCount == 0
            
