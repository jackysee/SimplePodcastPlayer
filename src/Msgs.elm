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
    | FetchFeedSucceed (Feed, List Item)
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
    | UpdateFeedSucceed (List Feed) (Feed, List Item)
    | SetProgress Float
    | SetListView ListView
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
    | MarkPlayCount Item Int
    | MarkItemsBelowListened String
    | MarkAllItemsAsListened
    | ShowItemDropdown String
    | HideItemDropdown
    | SelectItem Item
    | SelectNext
    | SelectPrev
    | Enqueue Item
    | Dequeue Item
    | MoveQueuedItemUp Item
    | MoveQueuedItemDown Item
    | SetShortcutKeys (List String)
    | SetFloatPanel FloatPanel
    | MsgBatch (List Msg)
    | SetItemSortLatest Bool
    | SetFallbackRssServiceUrl String
    | SetFontSize FontSize
    | SetPlayerShowTimeLeft Bool
    | SetTheme Theme
    | SetEditingFeedTitle (Maybe String)
    | SetFeedTitle Feed String
