module Icons exposing (..)

import Html exposing (Html)
import Svg
    exposing
        ( svg
        , path
        , rect
        , animate
        , g
        , animateTransform
        , line
        , polyline
        , polygon
        , circle
        )
import Svg.Attributes
    exposing
        ( class
        , viewBox
        , d
        , transform
        , attributeName
        , width
        , height
        , x
        , x1
        , x2
        , cx
        , y
        , y1
        , y2
        , cy
        , r
        , from
        , to
        , dur
        , begin
        , values
        , keyTimes
        , repeatCount
        , opacity
        , type_
        , fill
        , stroke
        , strokeWidth
        , strokeLinecap
        , strokeLinejoin
        , points
        )


plus : Html msg
plus =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "3"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather"
        ]
        [ line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


subscription : Html msg
subscription =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path
            [ d "M1696 384q40 0 68 28t28 68v1216q0 40-28 68t-68 28h-960q-40 0-68-28t-28-68v-288h-544q-40 0-68-28t-28-68v-672q0-40 20-88t48-76l408-408q28-28 76-48t88-20h416q40 0 68 28t28 68v328q68-40 128-40h416zm-544 213l-299 299h299v-299zm-640-384l-299 299h299v-299zm196 647l316-316v-416h-384v416q0 40-28 68t-68 28h-416v640h512v-256q0-40 20-88t48-76zm956 804v-1152h-384v416q0 40-28 68t-68 28h-416v640h896z" ]
            []
        ]


arrowDown : Html msg
arrowDown =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "3"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather"
        ]
        [ line [ x1 "12", y1 "4", x2 "12", y2 "20" ] []
        , polyline [ points "18 14 12 20 6 14" ] []
        ]


arrowLeft : Html msg
arrowLeft =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "3"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather"
        ]
        [ line [ x1 "20", y1 "12", x2 "4", y2 "12" ] []
        , polyline [ points "10 18 4 12 10 6" ] []
        ]


arrowUp : Html msg
arrowUp =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "3"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather"
        ]
        [ line [ x1 "12", y1 "20", x2 "12", y2 "4" ] []
        , polyline [ points "6 10 12 4 18 10" ] []
        ]


close : Html msg
close =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "3"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather"
        ]
        [ line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]


ellipsisV : Html msg
ellipsisV =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ circle [ cx "12", cy "12", r "2" ] []
        , circle [ cx "12", cy "4", r "2" ] []
        , circle [ cx "12", cy "20", r "2" ] []
        ]


equalizerPlaying : Html msg
equalizerPlaying =
    svg
        [ viewBox "0 0 24 24" ]
        [ g
            [ transform "rotate(180 12 12)" ]
            [ rect
                [ x "0", y "0", width "6", height "15" ]
                [ animate
                    [ attributeName "height"
                    , from "2"
                    , to "2"
                    , dur "0.8s"
                    , begin "0s"
                    , values "2; 24; 2"
                    , keyTimes "0; 0.5; 1"
                    , repeatCount "indefinite"
                    ]
                    []
                ]
            , rect
                [ x "9", y "0", width "6", height "20" ]
                [ animate
                    [ attributeName "height"
                    , from "2"
                    , to "2"
                    , dur "0.8s"
                    , begin "0.2s"
                    , values "2; 24; 2"
                    , keyTimes "0; 0.5; 1"
                    , repeatCount "indefinite"
                    ]
                    []
                ]
            , rect
                [ x "18", y "0", width "6", height "15" ]
                [ animate
                    [ attributeName "height"
                    , from "2"
                    , to "2"
                    , dur "0.8s"
                    , begin "0.3s"
                    , values "2; 24; 2"
                    , keyTimes "0; 0.5; 1"
                    , repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        ]


equalizerStop : Html msg
equalizerStop =
    svg
        [ viewBox "0 0 24 24" ]
        [ g
            [ transform "rotate(180 12 12)" ]
            [ rect [ x "0", y "0", width "6", height "4" ] []
            , rect [ x "9", y "0", width "6", height "4" ] []
            , rect [ x "18", y "0", width "6", height "4" ] []
            ]
        ]


exclamation : Html msg
exclamation =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ circle [ cx "12", cy "12", r "10" ] []
        , line [ x1 "12", y1 "8", x2 "12", y2 "12" ] []
        , line [ x1 "12", y1 "16", x2 "12", y2 "16" ] []
        ]


infoCircle : Html msg
infoCircle =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ circle [ cx "12", cy "12", r "10" ] []
        , line [ x1 "12", y1 "16", x2 "12", y2 "12" ] []
        , line [ x1 "12", y1 "8", x2 "12", y2 "8" ] []
        ]


loadingSpin : Html msg
loadingSpin =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ g
            []
            [ line [ x1 "12", y1 "2", x2 "12", y2 "6" ] []
            , line [ x1 "12", y1 "18", x2 "12", y2 "22" ] []
            , line [ x1 "4.93", y1 "4.93", x2 "7.76", y2 "7.76" ] []
            , line [ x1 "16.24", y1 "16.24", x2 "19.07", y2 "19.07" ] []
            , line [ x1 "2", y1 "12", x2 "6", y2 "12" ] []
            , line [ x1 "18", y1 "12", x2 "22", y2 "12" ] []
            , line [ x1 "4.93", y1 "19.07", x2 "7.76", y2 "16.24" ] []
            , line [ x1 "16.24", y1 "7.76", x2 "19.07", y2 "4.93" ] []
            , animateTransform
                [ attributeName "transform"
                , type_ "rotate"
                , from "0 12 12"
                , to "360 12 12"
                , dur "1.5s"
                , repeatCount "indefinite"
                ]
                []
            ]
        ]



{-
   svg
       [ viewBox "0 0 32 32"
       , fill "#666666"
       ]
       [ path
           [ opacity ".25"
           , d "M16 0 A16 16 0 0 0 16 32 A16 16 0 0 0 16 0 M16 4 A12 12 0 0 1 16 28 A12 12 0 0 1 16 4"
           ]
           []
       , path
           [ d "M16 0 A16 16 0 0 1 32 16 L28 16 A12 12 0 0 0 16 4z" ]
           [ animateTransform
               [ attributeName "transform"
               , type_ "rotate"
               , from "0 16 16"
               , to "360 16 16"
               , dur "0.8s"
               , repeatCount "indefinite"
               ]
               []
           ]
       ]
-}


pause : Html msg
pause =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ rect [ x "6", y "4", width "4", height "16" ] []
        , rect [ x "14", y "4", width "4", height "16" ] []
        ]


play : Html msg
play =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ polygon [ points "5 3 19 12 5 21 5 3" ] []
        ]


refresh : Html msg
refresh =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ polyline [ points "23 4 23 10 17 10" ] []
        , path [ d "M20.49 15a9 9 0 1 1-2.12-9.36L23 10" ] []
        ]


trash : Html msg
trash =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ polyline [ points "3 6 5 6 21 6" ] []
        , path [ d "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2" ] []
        , line [ x1 "10", y1 "11", x2 "10", y2 "17" ] []
        , line [ x1 "14", y1 "11", x2 "14", y2 "17" ] []
        ]


volumeOff : Html msg
volumeOff =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ polygon [ points "11 5 6 9 2 9 2 15 6 15 11 19 11 5" ] []
        , line [ x1 "23", y1 "9", x2 "17", y2 "15" ] []
        , line [ x1 "17", y1 "9", x2 "23", y2 "15" ] []
        ]


volume1 : Html msg
volume1 =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ polygon [ points "11 5 6 9 2 9 2 15 6 15 11 19 11 5" ] []
        , path [ d "M15.54 8.46a5 5 0 0 1 0 7.07" ] []
        ]


volume2 : Html msg
volume2 =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ polygon [ points "11 5 6 9 2 9 2 15 6 15 11 19 11 5" ] []
        , path [ d "M19.07 4.93a10 10 0 0 1 0 14.14M15.54 8.46a5 5 0 0 1 0 7.07" ] []
        ]


list : Html msg
list =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather"
        ]
        [ line [ x1 "8", y1 "6", x2 "21", y2 "6" ] []
        , line [ x1 "8", y1 "12", x2 "21", y2 "12" ] []
        , line [ x1 "8", y1 "18", x2 "21", y2 "18" ] []
        , line [ x1 "3", y1 "6", x2 "3", y2 "6" ] []
        , line [ x1 "3", y1 "12", x2 "3", y2 "12" ] []
        , line [ x1 "3", y1 "18", x2 "3", y2 "18" ] []
        ]


angleDoubleLeft : Html msg
angleDoubleLeft =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ polyline [ points "11 17 6 12 11 7" ] []
        , polyline [ points "18 17 13 12 18 7" ] []
        ]


edit : Html msg
edit =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ path [ d "M20 14.66V20a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h5.34" ] []
        , polygon [ points "18 2 22 6 12 16 8 16 8 12 18 2" ] []
        ]


externalLink : Html msg
externalLink =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-thin"
        ]
        [ path [ d "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6" ] []
        , polyline [ points "15 3 21 3 21 9" ] []
        , line [ x1 "10", y1 "14", x2 "21", y2 "3" ] []
        ]
