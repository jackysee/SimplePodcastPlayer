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
    | ItemAction ItemMsg
    | UpdateSetting UpdateSettingMsg
    | FloatPanelAction FloatPanelMsg
    | GotoAction GotoMsg
    | SetShortcutKeys (List String)
    | MsgBatch (List Msg)


type AddFeedMsg
    = ShowAddPanel
    | HideAddPanel
    | SetUrl String
    | ToAddFeed
    | FetchFeedSucceed ( Feed, List Item )
    | FetchFeedFail Http.Error


type GotoMsg
    = ShowGoto
    | HideGoto
    | SetSearch String
    | GotoUp
    | GotoDown


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


type ItemMsg
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
    | OpenNewLink String


type UpdateSettingMsg
    = SetFallbackRssServiceUrl String
    | SetFontSize FontSize
    | SetTheme Theme


type FloatPanelMsg
    = SetFloatPanel FloatPanel
    | ShowItemDropdown String
    | HideItemDropdown
