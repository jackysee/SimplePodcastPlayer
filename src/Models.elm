module Models exposing (..)

import Time exposing (Time)


{--
type alias Enclosure =
    { contentType : String
    , url : String
    }
--}


-- , length : Time


type alias Item =
    { title : String
    , pubDate : Time
    , link : Maybe String
    , url: Maybe String
    , show : Bool
    , progress : Progress
    }


type alias Feed =
    { url : String
    , title : String
    , items : List Item
    }

type alias Progress =
    { duration : Time
    , current : Time
    }

type LoadFeedState
    = Empty
    | Loading
    | Error
    | AlreadyExist

type PlayerState
    = Stopped
    | Playing
    | Paused

type alias Model =
    { urlToAdd : String
    , loadFeedState : LoadFeedState
    , playerState : PlayerState
    , list : List Feed
    , currentTime : Time
    , itemsToShow : Int
    , currentItemUrl : Maybe String
    }


type alias StoreModel =
    { urlToAdd : String
    -- , loadFeedState : LoadFeedState
    , list : List Feed
    -- , currentTime : Time
    , itemsToShow : Int
    , currentItemUrl : Maybe String
    -- , playerState : PlayerState
    }
