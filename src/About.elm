module About exposing (viewAbout, viewAboutButton, updateSettings)

import Html exposing (Html, div, text, img, button, h2, span, input, a, ul, li)
import Html.Attributes exposing (class, src, classList, value, href, target, style)
import Html.Events exposing (onClick, onInput)
import Models exposing (..)
import Msgs exposing (..)
import Markdown
import Events exposing (onInternalClick)
import Icons
import String
import Regex
import Return exposing (Return)
import Storage exposing (saveSetting)
import AddFeed exposing (viewAddInput)


updateSettings : UpdateSettingMsg -> Model -> Return Msg Model
updateSettings msg model =
    case msg of
        SetFallbackRssServiceUrl url ->
            updateSetting
                (\s ->
                    { s
                        | fallbackRssServiceUrl =
                            if url /= "" then
                                Just url
                            else
                                Nothing
                    }
                )
                model
                |> Return.singleton
                |> Return.effect_ saveSetting

        SetFontSize fontSize ->
            updateSetting (\s -> { s | fontSize = fontSize }) model
                |> Return.singleton
                |> Return.effect_ saveSetting

        SetTheme theme ->
            updateSetting (\s -> { s | theme = theme }) model
                |> Return.singleton
                |> Return.effect_ saveSetting


creditContent : String
creditContent =
    """
Created by [@jackysee](http://github.com/jackysee/SimplePodcastPlayer) using
[Elm](http://elm-lang.org). The RSS service is provided by [YQL](http://developer.yahoo.com/yql/console/).
Icons provided by [Feather](https://feather.netlify.com/).
"""


shortcutContent : String
shortcutContent =
    """
- Navigation
    - Go to Unlistened : **g u**
    - Go to Queued : **g q**
    - Go to Feed by search: **g g**
    - Go to Feed of the selected item : **g f**
    - Go to All Podcasts : **g a**
    - show settings : **shift+,**
    - show shortcuts : **shift+/**
    - show Add feed panel : **n**
- Selection
    - next item : **j|down**
    - previous item : **k|up**
    - Open selected item link (if any) : **o**
    - play selected item : **enter**
    - Enqueue / Dequeue selected item : **q**
    - Mark selected item as listened : **m**
- Actions
    - Refresh selected feed / all feeds : **r r**
    - Play / Pause player item : **space**
    - Toggle player rate : **s**
- Queue
    - move selected item Up : **u**
    - move sleected item Down : **d**
"""


viewAbout : Model -> Html Msg
viewAbout model =
    div
        [ classList
            [ ( "panel app-about", True )
            , ( "is-show"
              , case model.view.floatPanel of
                    About _ ->
                        True

                    _ ->
                        False
              )
            ]
        ]
        [ div [] <|
            case model.view.floatPanel of
                About content ->
                    [ button
                        [ class "btn btn-icon panel-close"
                        , onClick (FloatPanelAction <| SetFloatPanel Hidden)
                        ]
                        [ Icons.close ]
                    , div
                        [ class "about-tabs" ]
                      <|
                        List.map
                            (\( content_, label ) ->
                                div
                                    [ classList
                                        [ ( "about-tab", True )
                                        , ( "is-selected", content == content_ )
                                        ]
                                    , onClick (FloatPanelAction << SetFloatPanel << About <| content_)
                                    ]
                                    [ text label ]
                            )
                            [ ( Subscriptions, "Subscriptions" )
                            , ( Settings, "Settings" )
                            , ( Shortcut, "Shortcuts" )
                            , ( Credit, "About" )
                            ]
                    , case content of
                        Credit ->
                            div []
                                [ h2 [] [ text "Simple Podcast Player" ]
                                , Markdown.toHtml
                                    [ class "about-tab-content app-about-content" ]
                                    creditContent
                                ]

                        Shortcut ->
                            Markdown.toHtml
                                [ class "about-tab-content about-shortcut" ]
                            <|
                                decorateKeys shortcutContent

                        Settings ->
                            div
                                [ class "about-tab-content about-settings" ]
                                [ viewSettingFallbackUrl model.setting.fallbackRssServiceUrl
                                , viewSettingFontSize model.setting.fontSize
                                , viewSettingTheme model.setting.theme
                                ]

                        Subscriptions ->
                            div
                                [ class "about-tab-content about-subscriptions" ]
                                [ div
                                    [ class "about-add-input" ]
                                    [ viewAddInput model ]

                                --button
                                --    [ class "btn btn-block"
                                --    , onInternalClick (AddFeed ShowAddPanel)
                                --    ]
                                --    [ text "Add Podcast feed" ]
                                , viewSubscriptions model.feeds
                                ]
                    ]

                _ ->
                    [ text "" ]
        ]


decorateKeys : String -> String
decorateKeys content =
    Regex.replace
        Regex.All
        (Regex.regex (": \\*\\*(.*)\\*\\*"))
        (\{ match, submatches } ->
            let
                keys =
                    submatches
                        |> List.filterMap identity
                        |> List.map
                            (splitAndJoin
                                [ ( "|", " or " )
                                , ( " ", " " )
                                , ( "+", " + " )
                                ]
                                (\key ->
                                    "<div class=\"about-key\">" ++ key ++ "</div>"
                                )
                            )
                        |> String.concat
            in
                "- ** " ++ keys ++ "**"
        )
        content


splitAndJoin : List ( String, String ) -> (String -> String) -> String -> String
splitAndJoin list transform str =
    case list of
        [] ->
            str

        ( spliter, joiner ) :: xs ->
            str
                |> String.split spliter
                |> List.map
                    (\part ->
                        if List.length xs == 0 then
                            transform part
                        else
                            splitAndJoin xs transform part
                    )
                |> String.join joiner


viewSettingFallbackUrl : Maybe String -> Html Msg
viewSettingFallbackUrl fallbackRssServiceUrl =
    div
        [ class "about-setting-item" ]
        [ div
            [ class "field-label" ]
            [ text "Fallback rss service" ]
        , div
            [ class "about-setting-input-wrap" ]
            [ div
                []
                [ input
                    [ class "input-text"
                    , onInput (\s -> UpdateSetting <| SetFallbackRssServiceUrl s)
                    , value (Maybe.withDefault "" fallbackRssServiceUrl)
                    ]
                    []
                ]
            , div
                [ class "about-setting-note" ]
                [ text "Fallback to this service if YQL failed. You may use your own rss service like "
                , a
                    [ href "https://github.com/jackysee/RssJson"
                    , target "_blank"
                    ]
                    [ text "this one" ]
                ]
            ]
        ]


viewSettingFontSize : FontSize -> Html Msg
viewSettingFontSize fontSize =
    div
        [ class "about-setting-item" ]
        [ div
            [ class "field-label" ]
            [ text "Font Size" ]
        , div
            [ class "input-boxes about-setting-input-wrap" ]
          <|
            List.map
                (\fontSize_ ->
                    div
                        [ classList
                            [ ( "font-size-box", True )
                            , ( "is-selected", fontSize == fontSize_ )
                            ]
                        , style
                            [ ( "font-size", getFontSizePx fontSize_ ) ]
                        , onClick (UpdateSetting <| SetFontSize fontSize_)
                        ]
                        [ text "aA" ]
                )
                [ Small
                , Medium
                , Large
                ]
        ]


viewSettingTheme : Theme -> Html Msg
viewSettingTheme theme =
    div
        [ class "about-setting-item" ]
        [ div
            [ class "field-label" ]
            [ text "Theme" ]
        , div
            [ class "input-boxes about-setting-input-wrap" ]
          <|
            List.map
                (\theme_ ->
                    div
                        [ classList [ ( "is-selected", theme == theme_ ) ]
                        , onClick (UpdateSetting <| SetTheme theme_)
                        ]
                        [ div
                            [ class <| "theme-box theme-" ++ (themeToStr theme_ |> String.toLower)
                            , style
                                [ ( "color", "var(--body-color)" )
                                , ( "background-color", "var(--body-bg)" )
                                ]
                            ]
                            [ text <| themeToStr theme_ ]
                        ]
                )
                [ Light
                , Dark
                ]
        ]


viewAboutButton : Html Msg
viewAboutButton =
    button
        [ class "btn btn-icon"
        , onInternalClick (FloatPanelAction <| SetFloatPanel <| About Credit)
        ]
        [ Icons.infoCircle
        ]


viewSubscriptions : List Feed -> Html Msg
viewSubscriptions feeds =
    ul [ class "subscription-list" ] <|
        List.map
            (\feed ->
                li
                    []
                    [ a
                        [ onInternalClick <| ItemList <| SetListView (ViewFeed feed.url) ]
                        [ text feed.title ]
                    ]
            )
            feeds



--(List.append feeds (List.repeat 20 { url = "", title = "abc", state = Normal, showConfirmDelete = False, link = Nothing }))
