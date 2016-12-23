port module Main exposing (..)

import Html as App
import Task exposing (Task)
import Time
import ListUtil exposing (swapDown, swapUp, getNext, getPrev)
import Json.Decode
import Models exposing (..)
import Msgs exposing (..)
import View exposing (view)
import Feed
    exposing
        ( loadFeed
        , updateFeed
        , updateModelFeed
        , updateFeedItems
        , updateUpdateFeed
        , updateFeeds
        , updateDeleteFeed
        )
import Shortcut exposing (keyMap, scrollToIndex)
import FloatPlanel exposing (hideItemDropdown)
import DecodeStoreModel exposing (decodeStoreValue)
import Return exposing (Return)
import AddFeed exposing (updateAddFeed)
import Storage exposing (..)
import Player exposing (updatePlayer, playError, paused, playEnd, soundLoaded, updateProgress)
import ItemList exposing (updateItemList, updateUpdateItem)
import About exposing (updateSettings)


main : Program (Maybe Json.Decode.Value) Model Msg
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe Json.Decode.Value -> ( Model, Cmd Msg )
init storeValue =
    case decodeStoreValue storeValue of
        Just storeModel ->
            let
                model =
                    fromStoreModel storeModel
            in
                model
                    ! [ updateCurrentTime
                      , updateFeeds model.feeds
                      ]

        Nothing ->
            defaultModel
                ! [ saveSetting defaultModel
                  , saveView defaultModel
                  , updateCurrentTime
                  ]


updateCurrentTime : Cmd Msg
updateCurrentTime =
    Task.perform UpdateCurrentTime Time.now


update : Msg -> Model -> Return Msg Model
update msg model =
    (case msg of
        NoOp ->
            Return.singleton model

        UpdateCurrentTime time ->
            let
                view =
                    model.view
            in
                { model | view = { view | currentTime = time } }
                    |> Return.singleton

        AddFeed addFeedMsg ->
            updateAddFeed addFeedMsg model

        Player playerMsg ->
            updatePlayer playerMsg model

        PlayEnd url ->
            let
                model_ =
                    model
                        |> updateCurrentItem
                            (\item ->
                                { item
                                    | progress = 0
                                    , markPlayCount = item.playCount + 1
                                }
                            )

                nextInQueue =
                    oneOfMaybe
                        [ getNext (\( url_, feedUrl ) -> url == url_) model.view.playList
                        , List.head model.view.playList
                        ]
                        |> Maybe.map (getItemByUrl model)
                        |> Maybe.withDefault Nothing

                nextItem =
                    oneOfMaybe
                        [ nextInQueue
                        , itemList model
                            |> Tuple.first
                            |> getNext (\( feed, item ) -> item.url == url)
                        ]
            in
                case nextItem of
                    Just ( feed, item_ ) ->
                        model_
                            |> Return.singleton
                            |> Return.andThen (update <| Player (Play item_))
                            |> Return.andThen
                                (getCurrentItem model
                                    |> Maybe.map (\item -> update <| UpdateItem <| Dequeue item)
                                    |> Maybe.withDefault (Return.singleton)
                                )

                    Nothing ->
                        model_
                            |> updateView (\v -> { v | currentItem = Nothing })
                            |> Return.singleton
                            |> Return.effect_ saveView

        UpdateFeed msg ->
            updateUpdateFeed msg model

        DeleteFeed msg ->
            updateDeleteFeed msg model

        ItemList msg ->
            updateItemList msg model

        UpdateItem msg ->
            updateUpdateItem msg model

        UpdateSetting msg ->
            updateSettings msg model

        OpenNewLink url ->
            ( model, openNewLink url )

        ShowItemDropdown url ->
            model
                |> updateView (\v -> { v | floatPanel = ItemDropdown url })
                |> Return.singleton

        HideItemDropdown ->
            model
                |> updateView (\v -> { v | floatPanel = hideItemDropdown model.view.floatPanel })
                |> Return.singleton

        SetShortcutKeys keys ->
            updateView (\v -> { v | shortcutKeys = keys }) model
                |> Return.singleton
                |> Return.effect_ saveView

        SetFloatPanel panel ->
            updateView (\v -> { v | floatPanel = panel }) model
                |> Return.singleton
                |> Return.effect_ saveView

        MsgBatch list ->
            List.foldl
                Return.andThen
                (Return.singleton model)
                (List.map update list)
    )
        |> Return.command (updateCurrentTimeCmd msg)


updateCurrentTimeCmd : Msg -> Cmd Msg
updateCurrentTimeCmd msg =
    case msg of
        NoOp ->
            Cmd.none

        UpdateCurrentTime t ->
            Cmd.none

        _ ->
            updateCurrentTime


oneOfMaybe : List (Maybe a) -> Maybe a
oneOfMaybe list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            case x of
                Just x_ ->
                    Just x_

                Nothing ->
                    oneOfMaybe xs


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ updateProgress (Player << UpdateProgress)
        , soundLoaded (Player << SoundLoaded)
        , playEnd PlayEnd
        , keyUp <| keyMap model
        , playError (Player << PlayError)
        , paused (Player << PlayerPaused)
        ]


port openNewLink : String -> Cmd msg


port keyUp : (String -> msg) -> Sub msg
