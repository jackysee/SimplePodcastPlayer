module Msgs exposing (..)

import Models exposing (..)
import Http
import Time exposing (Time)
-- import Keyboard exposing (KeyCode)

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
    | UpdateAllFeed
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
    | PlayError String
    | PlayEnd String
    | ToggleRate
    | OpenNewLink String
    | SetVol Float
    | SetItemFilter ItemFilter
    | MarkPlayCount String Int
    | MarkItemsBelowListened String
    | MarkAllItemsAsListened 
    | ShowItemDropdown String
    | HideItemDropdown
    | SelectItem Item
    | UnselectItem Item
    | SelectNext
    | SelectPrev
    | Enqueue String
    | Dequeue String
    | MoveQueuedItemUp String
    | MoveQueuedItemDown String
    | ToggleShortcutGoto Bool
    | SetShortcutKeys (List String)
    | SetFloatPanel FloatPanel
    | MsgBatch (List Msg)
    | SetItemSortLatest Bool
    | SetFallbackRssServiceUrl String
    | SetFontSize FontSize
    | SetPlayerShowTimeLeft Bool
    | SetTheme Theme
