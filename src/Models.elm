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
    , playerMute : Bool
    , showFeedUrl : Maybe String
    , itemFilter: ItemFilter
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
    , playerMute : Bool
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


updateCurrentItem : (Item -> Item) -> Model -> Model
updateCurrentItem updater model =
    { model | list =
        List.map (\feed ->
            { feed | items =
                List.map (\item ->
                    if isCurrent item.url model then
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
