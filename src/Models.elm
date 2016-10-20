module Models exposing (..)

import Time exposing (Time)


type alias Item =
    { title : String
    , pubDate : Time
    , link : Maybe String
    , url: Maybe String
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
    | Listening
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


isCurrent : Maybe String -> Model -> Bool
isCurrent itemUrl model =
    (Maybe.map2 (==) itemUrl model.currentItemUrl) == Just True


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
        |> List.filter (\item -> item.url == model.itemSelected)
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
                    if item.url == url && url /= Nothing then
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
        "Listening" -> Listening
        _ -> All


itemFilterToStr : ItemFilter -> String
itemFilterToStr filter =
    case filter of
        Unlistened -> "Unlistened"
        Listening -> "Listening"
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
    }



toStoreFeed : Feed -> StoreFeed
toStoreFeed feed =
    { url = feed.url
    , title = feed.title
    , items = feed.items
    }


itemList: Model -> (List (Feed, Item), Bool)
itemList model =
    case model.showFeedUrl of
        Just url' ->
            ( model.list
                |> List.filter (\f -> f.url == url' )
                |> itemsByDate model.itemFilter
            , False
            )

        Nothing ->
            let
                list = itemsByDate model.itemFilter model.list
            in
                ( List.take model.itemsToShow list
                , List.length list > model.itemsToShow
                )


itemsByDate: ItemFilter -> List Feed -> List (Feed, Item)
itemsByDate filter list =
    list
        |> List.concatMap (\feed ->
                List.map (\item -> (feed, item)) feed.items
            )
        |> List.filter (\(feed, item) ->
                filterByItemFilter filter item
            )
        |> List.sortBy (\(feed, item) -> item.pubDate)
        |> List.reverse


filterByItemFilter : ItemFilter -> Item -> Bool
filterByItemFilter filter item =
    case filter of
        All -> True
        Unlistened ->
            item.playCount == 0
        Listening ->
            item.progress > -1 && item.playCount == 0
