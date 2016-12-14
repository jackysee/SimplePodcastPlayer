port module Storage exposing (..)

import Models exposing (..)
import Msgs exposing (..)
import Task exposing (Task)


noOpTask : Task x a -> Cmd Msg
noOpTask task =
    Task.attempt (\_ -> NoOp) task


saveSetting : Model -> Cmd Msg
saveSetting model =
    model.setting |> toStoreSetting |> storeSetting


saveView : Model -> Cmd Msg
saveView model =
    model.view |> toStoreView |> storeView


saveFeeds : List Feed -> Cmd Msg
saveFeeds feeds =
    feeds
        |> List.map toStoreFeed
        |> storeFeeds


saveItems : List Item -> Cmd Msg
saveItems items =
    storeItems items


port storeSetting : StoreSetting -> Cmd msg


port storeView : StoreView -> Cmd msg


port storeFeeds : List StoreFeed -> Cmd msg


port storeItems : List Item -> Cmd msg


port deleteFeed : StoreFeed -> Cmd msg
