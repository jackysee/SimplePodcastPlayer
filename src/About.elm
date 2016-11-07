module About exposing (viewAbout, viewAboutButton)

import Html exposing (Html, div, text, img, button, h2, span, input, a)
import Html.Attributes exposing (class, src, classList, value, href, target, style)
import Html.Events exposing (onClick, onInput)
import Models exposing (..)
import Msgs exposing (..)
import Markdown
import Events exposing (onInternalClick)
import Icons
import String

creditContent : String
creditContent = """
Created by [@jackysee](http://github.com/jackysee/SimplePodcastPlayer) using
[Elm](http://elm-lang.org). The RSS service is provided by [YQL](http://developer.yahoo.com/yql/console/).

Icons provided by [font-awesome](http://fontawesome.io) and:
* "Subscription" Created by Michal Beno from [the Noun Project](http://thenounproject.com)
* "Information" Created by Gonzalo Bravo from [the Noun Project](http://thenounproject.com)
"""

shortcutContent : String
shortcutContent = """
- Navigation
    - **gu** : Go to Unlistened
    - **gq** : Go to Queued
    - **gf** : Go to Feed of the selected item
    - **ga** : Go to All Podcasts
    - **s** : show settings
    - **?** : show shortcuts
- Selection
    - **j / down** : next item
    - **k / up** : previous item
- Actions on selected item
    - **o** : Open link (if any)
    - **right** : play selected item
    - **q** : Enqueue / Dequeue selected item
    - **m** : mark as listened
- Actions
    - **rr** : refresh selected feed / all feeds
- Player
    - **p** : Play / Pause
- Add Feed Panel
    - **n** : Show Add feed panel
    - **Esc** : Hide Add Panel
- Queue
    - **u** : move selected item up
    - **d** : move sleected item down
"""

viewAbout : Model -> Html Msg
viewAbout model =
    div
        [ classList
            [ ("app-about", True)
            , ("is-show",
                case model.floatPanel of
                    About _ -> True
                    _ -> False
              )
            ]
        ] <|
        case model.floatPanel of
            About content ->
                [ button
                    [ class "btn btn-icon app-about-close"
                    , onClick (SetFloatPanel Hidden)
                    ]
                    [ Icons.close
                    ]
                , div
                    [ class "about-tabs" ] <|
                        List.map
                            (\(content_, label) ->
                                div
                                    [ classList
                                        [ ("about-tab", True)
                                        , ("is-selected", content == content_)
                                        ]
                                    , onClick (SetFloatPanel (About content_))
                                    ]
                                    [ text label ]
                            )
                            [ (Settings, "Settings")
                            , (Shortcut ,"Shortcuts")
                            , (Credit, "Credits")
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
                            [ class "about-tab-content about-shortcut"]
                            shortcutContent
                    Settings ->
                        div
                            [ class "about-tab-content about-settings" ]
                            [ viewSettingFallbackUrl model.fallbackRssServiceUrl
                            , viewSettingFontSize model.fontSize
                            , viewSettingTheme model.theme
                            ]
                ]

            _ ->
                [ text "" ]


viewSettingFallbackUrl : Maybe String -> Html Msg
viewSettingFallbackUrl fallbackRssServiceUrl =
    div
        [ class "about-setting-item" ]
        [ div
            [ class "about-setting-label" ]
            [ text "Fallback rss service" ]
        , div
            [ class "about-setting-input-wrap" ]
            [ div
                []
                [ input
                    [ class "about-setting-input"
                    , onInput SetFallbackRssServiceUrl
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
            [ class "about-setting-label" ]
            [ text "Font Size" ]
        , div
            [ class "input-boxes about-setting-input-wrap" ]
            <| List.map
                (\(fontSize_) ->
                    div
                        [ classList
                            [ ("font-size-box", True)
                            , ("is-selected", fontSize == fontSize_)
                            ]
                        , style
                            [ ("font-size", getFontSizePx fontSize_) ]
                        , onClick (SetFontSize fontSize_)
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
            [ class "about-setting-label" ]
            [ text "Theme" ]
        , div
            [ class "input-boxes about-setting-input-wrap" ]
            <| List.map
                (\theme_ ->
                    div
                        [ classList [ ("is-selected", theme == theme_) ]
                        , onClick (SetTheme theme_)
                        ]
                        [ div
                            [ class <| "theme-box theme-" ++ (themeToStr theme_ |> String.toLower)
                            , style
                                [ ("color",  "var(--body-color)")
                                , ("background-color", "var(--body-bg)")
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
        , onInternalClick (SetFloatPanel (About Credit))
        ]
        [ Icons.infoCircle
        ]
