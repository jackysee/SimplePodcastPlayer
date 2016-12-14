module Msgs exposing (..)

import Models exposing (..)
import Http
import Time exposing (Time)


type Msg
    = NoOp
    | UpdateCurrentTime Time
    | AddFeed AddFeedMsg
    | Player PlayerMsg
    | PlayEnd String
    | ShowMoreItem
    | UpdateAllFeed
    | UpdateFeeds (List Feed) Feed
    | UpdateFeedFail (List Feed) Feed Http.Error
    | UpdateFeedSucceed (List Feed) ( Feed, List Item )
    | SetListView ListView
    | HideFeed
    | ShowConfirmDeleteFeed Feed
    | HideConfirmDeleteFeed Feed
    | ConfirmDeleteFeed Feed
    | OpenNewLink String
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
    | SetTheme Theme
    | SetEditingFeedTitle (Maybe String)
    | SetFeedTitle Feed String


type AddFeedMsg
    = ShowAddPanel
    | HideAddPanel
    | SetUrl String
    | ToAddFeed
    | FetchFeedSucceed ( Feed, List Item )
    | FetchFeedFail Http.Error


type PlayerMsg
    = Play Item
    | SoundLoaded Bool
    | Pause Item
    | Stop Item
    | UpdateProgress Progress
    | SetProgress Float
    | ClosePlayer
    | PlayError String
    | ToggleRate
    | SetVol Float
    | SetPlayerShowTimeLeft Bool
    | PlayerPaused Bool
