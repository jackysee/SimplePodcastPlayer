module Msgs exposing (..)

import Models exposing (..)
import Http
import Time exposing (Time)

type Msg
    = NoOp
    | ShowAddPanel
    | HideAddPanel
    | SetUrl String
    | AddFeed
    | FetchFeedSucceed Feed
    | FetchFeedFail Http.Error
    | UpdateCurrentTime Time
    | ShowMoreItem
    | Play Item
    | SoundLoaded Bool
    | Pause Item
    | Stop Item
    | UpdateProgress Progress
    | UpdateFeeds (List Feed) Feed
    | UpdateFeedFail (List Feed) Feed Http.Error
    | UpdateFeedSucceed (List Feed) Feed
    | SetProgress Float
    | ShowFeed String
    | HideFeed
    | ShowConfirmDeleteFeed Feed
    | HideConfirmDeleteFeed Feed
    | ConfirmDeleteFeed Feed
    | ClosePlayer
    | PlayEnd String
    | ToggleRate
    | OpenNewLink String
    | SetVol Float
    | TogglePlayerMute
    | SetItemFilter ItemFilter
