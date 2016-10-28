module About exposing (viewAbout, viewAboutButton)

import Html exposing (Html, div, text, h1, img, button)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Models exposing (..)
import Msgs exposing (..)
import Markdown
import Events exposing (onInternalClick)

content : String
content = """
# Simple Podcast Player

Created by [@jackysee](http://github.com/jackysee/SimplePodcastPlayer) using
[Elm](http://elm-lang.org). The RSS service is provided by [YQL](http://developer.yahoo.com/yql/console/).

Icons provided by [font-awesome](http://fontawesome.io) and:
* "Subscription" Created by Michal Beno from [the Noun Project](http://thenounproject.com)
* "Information" Created by Gonzalo Bravo from [the Noun Project](http://thenounproject.com)
"""

viewAbout : Model -> Html Msg
viewAbout model =
    if model.floatPanel == About then
        div
            [ class "app-about" ]
            [ button
                [ class "btn btn-icon app-about-close"
                , onClick (SetFloatPanel Hidden)
                ]
                [ img [ src "assets/close.svg" ] []
                ]
            , Markdown.toHtml
                [ class "app-about-content" ]
                content
            ]
    else
        text ""


viewAboutButton : Html Msg
viewAboutButton =
    button
        [ class "btn btn-icon"
        , onInternalClick (SetFloatPanel About)
        ]
        [ img [ src "assets/info-circle.svg" ] []
        ]
