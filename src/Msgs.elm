module Msgs exposing (..)

import Models exposing (..)
import Http
import Time exposing (Time)

type Msg
    = NoOp
    | SetUrl String
    | AddFeed
    | FetchFeedSucceed Feed
    | FetchFeedFail Http.Error
    | UpdateCurrentTime Time
    | ShowMoreItem Feed
    | Play Item
    | SoundLoaded Bool
    | Pause Item
    | Stop Item
    | UpdateProgress Progress
    | UpdateFeeds (List Feed) Feed
    | UpdateFeedFail (List Feed) Feed Http.Error
    | UpdateFeedSucceed (List Feed) Feed
    | SetProgress Float
    | ShowAddPanel
    | HideAddPanel
    | ShowConfirmDeleteFeed Feed
    | HideConfirmDeleteFeed Feed
    | ConfirmDeleteFeed Feed
    | ClosePlayer
    | PlayEnd String
    | ToggleRate
    | OpenNewLink String
    | HideAllUnder Item
    | SetVol Float
    | TogglePlayerMute
    | SetGroupByFeed Bool
