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
    | UpdateFeed UpdateFeedMsg
    | DeleteFeed DeleteFeedMsg
    | ItemList ItemListMsg
    | UpdateItem UpdateItemMsg
    | OpenNewLink String
    | ShowItemDropdown String
    | HideItemDropdown
    | SetShortcutKeys (List String)
    | SetFloatPanel FloatPanel
    | MsgBatch (List Msg)
    | SetFallbackRssServiceUrl String
    | SetFontSize FontSize
    | SetTheme Theme


type AddFeedMsg
    = ShowAddPanel
    | HideAddPanel
    | SetUrl String
    | ToAddFeed
    | FetchFeedSucceed ( Feed, List Item )
    | FetchFeedFail Http.Error


type DeleteFeedMsg
    = ShowConfirmDeleteFeed Feed
    | HideConfirmDeleteFeed Feed
    | ConfirmDeleteFeed Feed


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


type UpdateFeedMsg
    = UpdateAllFeed
    | UpdateFeeds (List Feed) Feed
    | UpdateFeedFail (List Feed) Feed Http.Error
    | UpdateFeedSucceed (List Feed) ( Feed, List Item )
    | SetEditingFeedTitle (Maybe String)
    | SetFeedTitle Feed String


type ItemListMsg
    = SetListView ListView
    | ShowMoreItem
    | SetItemFilter ItemFilter
    | HideFeed
    | SetItemSortLatest Bool


type UpdateItemMsg
    = MarkPlayCount Item Int
    | MarkItemsBelowListened String
    | MarkAllItemsAsListened
    | SelectItem Int
    | SelectNext
    | SelectPrev
    | Enqueue Item
    | Dequeue Item
    | MoveQueuedItemUp Item
    | MoveQueuedItemDown Item
