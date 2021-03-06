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
        )
import Shortcut exposing (keyMap, scrollToIndex)
import FloatPlanel exposing (updateFloatPanel)
import DecodeStoreModel exposing (decodeStoreValue)
import Return exposing (Return)
import AddFeed exposing (updateAddFeed)
import Storage exposing (..)
import Player exposing (updatePlayer, playError, paused, stopped, playEnd, onPlay, updateProgress)
import ItemList exposing (updateItemList, updateUpdateItem)
import About exposing (updateSettings)
import Goto exposing (updateGoto)
import EditFeed exposing (updateEditFeed)


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

                currentInQueue =
                    case model.view.currentItem of
                        Just item_ ->
                            List.member item_ model.view.playList

                        Nothing ->
                            False

                nextItem =
                    if currentInQueue then
                        model.view.playList
                            |> getNext (\( url_, _ ) -> url == url_)
                            |> Maybe.map (getItemByUrl model)
                            |> Maybe.withDefault Nothing
                    else
                        Nothing
            in
                case nextItem of
                    Just ( feed, item_ ) ->
                        model_
                            |> Return.singleton
                            |> Return.andThen (update <| Player (Play item_))
                            |> Return.andThen
                                (getCurrentItem model
                                    |> Maybe.map (\item -> update <| ItemAction <| Dequeue item)
                                    |> Maybe.withDefault (Return.singleton)
                                )

                    Nothing ->
                        model_
                            |> updateView (\v -> { v | currentItem = Nothing })
                            |> Return.singleton
                            |> Return.map updateViewItems
                            |> Return.effect_ saveView

        UpdateFeed msg ->
            updateUpdateFeed msg model

        ItemList msg ->
            updateItemList msg model

        ItemAction msg ->
            updateUpdateItem msg model

        UpdateSetting msg ->
            updateSettings msg model

        FloatPanelAction msg ->
            updateFloatPanel msg model

        GotoAction msg ->
            updateGoto msg model

        SetShortcutKeys keys ->
            updateView (\v -> { v | shortcutKeys = keys }) model
                |> Return.singleton
                |> Return.effect_ saveView

        EditFeed msg ->
            updateEditFeed msg model

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
        , onPlay (Player << OnPlay)
        , playEnd PlayEnd
        , keyUp <| keyMap model
        , playError (Player << PlayError)
        , paused (Player << PlayerPaused)
        , stopped (Player << PlayerStopped)
        ]


port keyUp : (String -> msg) -> Sub msg
