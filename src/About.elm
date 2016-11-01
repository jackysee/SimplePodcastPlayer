module About exposing (viewAbout, viewAboutButton)

import Html exposing (Html, div, text, img, button, h2, span, input, a)
import Html.Attributes exposing (class, src, classList, value, href, target)
import Html.Events exposing (onClick, onInput)
import Models exposing (..)
import Msgs exposing (..)
import Markdown
import Events exposing (onInternalClick)

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
- Selection
    - **j / down** : next item
    - **k / up** : previous item
- Actions on selected item
    - **o** : Open link (if any)
    - **right** : play selected item
    - **q** : Enqueue / Dequeue selected item
    - **m** : mark as listened
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
    case model.floatPanel of
        About content ->
            div
                [ class "app-about" ]
                [ button
                    [ class "btn btn-icon app-about-close"
                    , onClick (SetFloatPanel Hidden)
                    ]
                    [ img [ src "assets/close.svg" ] []
                    ]
                , h2 [] [ text "Simple Podcast Player" ]
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
                        Markdown.toHtml
                            [ class "about-tab-content app-about-content" ]
                            creditContent
                    Shortcut ->
                        Markdown.toHtml
                            [ class "about-tab-content about-shortcut"]
                            shortcutContent
                    Settings ->
                        div
                            [ class "about-tab-content about-settings" ]
                            [ div 
                                [ class "about-setting-item" ]
                                [ div 
                                    [] 
                                    [ text "Fallback rss to json service for YQL" ]
                                , div
                                    [ class "about-setting-input-wrap" ]
                                    [ div
                                        [] 
                                        [ input
                                            [ class "about-setting-input" 
                                            , onInput SetFallbackRssServiceUrl
                                            , value (Maybe.withDefault "" model.fallbackRssServiceUrl)
                                            ]
                                            []
                                        ]
                                    , div
                                    -- , Markdown.toHtml
                                        [ class "about-setting-note" ]
                                        [ text "You may use your own rss service like "
                                        , a 
                                            [ href "https://github.com/jackysee/RssJson" 
                                            , target "_blank"
                                            ]
                                            [ text "this one" ]
                                        ]
                                    ]
                                ]
                            ]
                ]

        _ ->
            text ""


viewAboutButton : Html Msg
viewAboutButton =
    button
        [ class "btn btn-icon"
        , onInternalClick (SetFloatPanel (About Credit))
        ]
        [ img [ src "assets/info-circle.svg" ] []
        ]
