module Models exposing (..)

import Time exposing (Time)


type alias Enclosure =
    { contentType : String
    , url : String
    }



-- , length : Time


type alias Item =
    { title : String
    , pubDate : Time
    , enclosure : Maybe Enclosure
    , link : Maybe String
    , show : Bool
    }


type alias Feed =
    { url : String
    , title : String
    , items : List Item
    }


type LoadFeedState
    = Empty
    | Loading
    | Error
    | AlreadyExist


type alias Model =
    { urlToAdd : String
    , loadFeedState : LoadFeedState
    , list : List Feed
    , currentTime : Time
    , itemsToShow : Int
    , currentItem : Maybe Item
    }
