module Icons exposing (..)

import Html exposing (Html)
import Svg exposing (svg, path, node, rect, animate, g, animateTransform)
import Svg.Attributes exposing 
    ( viewBox, d, transform, attributeName, width, height, x, y, from, to
    , dur, begin, values, keyTimes, repeatCount, opacity, type', fill
    )


subscription : Html msg
subscription =
    svg 
        [ viewBox "0 0 100 100" ]
        [ path
            [ d "M67.4,81.8c6.6,0,12.2-4.5,14-10.6c6.1-1.7,10.6-7.3,10.6-14V32.7c0-8-6.5-14.5-14.5-14.5H32.6c-6.6,0-12.2,4.5-14,10.6  c-6.1,1.7-10.6,7.3-10.6,14v24.7c0,8,6.5,14.5,14.5,14.5H67.4z M77.4,23.2c5.2,0,9.5,4.3,9.5,9.5v24.7c0,3.6-2,6.8-5,8.4v-23  c0-8-6.5-14.5-14.5-14.5H24.2c1.6-3,4.8-5,8.4-5H77.4z M34.6,66.8V43.2c0-1.5,1.7-2.5,3-1.7l20.4,11.8c1.3,0.8,1.3,2.7,0,3.5  L37.6,68.5C36.3,69.3,34.6,68.3,34.6,66.8z" ]
            []
        ] 


arrowDown : Html msg
arrowDown =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path
            [ d "M1675 832q0 53-37 90l-651 652q-39 37-91 37-53 0-90-37l-651-652q-38-36-38-90 0-53 38-91l74-75q39-37 91-37 53 0 90 37l294 294v-704q0-52 38-90t90-38h128q52 0 90 38t38 90v704l294-294q37-37 90-37 52 0 91 37l75 75q37 39 37 91z" ]
            []
        ]


arrowLeft : Html msg
arrowLeft = 
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path
            [ d "M1664 896v128q0 53-32.5 90.5t-84.5 37.5h-704l293 294q38 36 38 90t-38 90l-75 76q-37 37-90 37-52 0-91-37l-651-652q-37-37-37-90 0-52 37-91l651-650q38-38 91-38 52 0 90 38l75 74q38 38 38 91t-38 91l-293 293h704q52 0 84.5 37.5t32.5 90.5z" ]
            []
        ]


arrowUp : Html msg
arrowUp = 
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path
            [ d "M1675 971q0 51-37 90l-75 75q-38 38-91 38-54 0-90-38l-294-293v704q0 52-37.5 84.5t-90.5 32.5h-128q-53 0-90.5-32.5t-37.5-84.5v-704l-294 293q-36 38-90 38t-90-38l-75-75q-38-38-38-90 0-53 38-91l651-651q35-37 90-37 54 0 91 37l651 651q37 39 37 91z" ]
            []
        ]


close : Html msg
close =
    svg 
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1490 1322q0 40-28 68l-136 136q-28 28-68 28t-68-28l-294-294-294 294q-28 28-68 28t-68-28l-136-136q-28-28-28-68t28-68l294-294-294-294q-28-28-28-68t28-68l136-136q28-28 68-28t68 28l294 294 294-294q28-28 68-28t68 28l136 136q28 28 28 68t-28 68l-294 294 294 294q28 28 28 68z" ]
            []
        ]


ellipsisV : Html msg
ellipsisV =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1088 1248v192q0 40-28 68t-68 28h-192q-40 0-68-28t-28-68v-192q0-40 28-68t68-28h192q40 0 68 28t28 68zm0-512v192q0 40-28 68t-68 28h-192q-40 0-68-28t-28-68v-192q0-40 28-68t68-28h192q40 0 68 28t28 68zm0-512v192q0 40-28 68t-68 28h-192q-40 0-68-28t-28-68v-192q0-40 28-68t68-28h192q40 0 68 28t28 68z" ]
            []
        ]


equalizerPlaying : Html msg
equalizerPlaying = 
    svg
        [ viewBox "0 0 24 24" ]
        [ g 
            [ transform "rotate(180 12 12)" ]
            [ rect 
                [ x "0", y "0", width "6", height "15"]
                [ animate
                    [ attributeName "height"
                    , from "2", to "2", dur "0.8s"
                    , begin "0s", values "2; 24; 2"
                    , keyTimes "0; 0.5; 1" 
                    , repeatCount "indefinite"
                    ]
                    []
                ]
            , rect 
                [ x "9", y "0", width "6", height "20"]
                [ animate
                    [ attributeName "height"
                    , from "2", to "2", dur "0.8s"
                    , begin "0.2s", values "2; 24; 2"
                    , keyTimes "0; 0.5; 1" 
                    , repeatCount "indefinite"
                    ]
                    []
                ]
            , rect 
                [ x "18", y "0", width "6", height "15"]
                [ animate
                    [ attributeName "height"
                    , from "2", to "2", dur "0.8s"
                    , begin "0.3s", values "2; 24; 2"
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
            [ rect
                [ x "0", y "0", width "6", height"4" ]
                []
            , rect
                [ x "9", y "0", width "6", height"4" ]
                []
            , rect
                [ x "18", y "0", width "6", height"4" ]
                []
            ]
        ]


exclamation : Html msg
exclamation = 
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M896 128q209 0 385.5 103t279.5 279.5 103 385.5-103 385.5-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103zm128 1247v-190q0-14-9-23.5t-22-9.5h-192q-13 0-23 10t-10 23v190q0 13 10 23t23 10h192q13 0 22-9.5t9-23.5zm-2-344l18-621q0-12-10-18-10-8-24-8h-220q-14 0-24 8-10 6-10 18l17 621q0 10 10 17.5t24 7.5h185q14 0 23.5-7.5t10.5-17.5z" ]
            []
        ]


externalLink : Html msg
externalLink = 
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1408 928v320q0 119-84.5 203.5t-203.5 84.5h-832q-119 0-203.5-84.5t-84.5-203.5v-832q0-119 84.5-203.5t203.5-84.5h704q14 0 23 9t9 23v64q0 14-9 23t-23 9h-704q-66 0-113 47t-47 113v832q0 66 47 113t113 47h832q66 0 113-47t47-113v-320q0-14 9-23t23-9h64q14 0 23 9t9 23zm384-864v512q0 26-19 45t-45 19-45-19l-176-176-652 652q-10 10-23 10t-23-10l-114-114q-10-10-10-23t10-23l652-652-176-176q-19-19-19-45t19-45 45-19h512q26 0 45 19t19 45z" ]
            []
        ]


infoCircle : Html msg
infoCircle =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1152 1376v-160q0-14-9-23t-23-9h-96v-512q0-14-9-23t-23-9h-320q-14 0-23 9t-9 23v160q0 14 9 23t23 9h96v320h-96q-14 0-23 9t-9 23v160q0 14 9 23t23 9h448q14 0 23-9t9-23zm-128-896v-160q0-14-9-23t-23-9h-192q-14 0-23 9t-9 23v160q0 14 9 23t23 9h192q14 0 23-9t9-23zm640 416q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z" ]
            []
        ]

loadingSpin : Html msg
loadingSpin =
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
                [ attributeName "transform", type' "rotate"
                , from "0 16 16", to "360 16 16", dur "0.8s"
                , repeatCount "indefinite" 
                ]
                []

            ]
        ]


pause : Html msg
pause =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1664 192v1408q0 26-19 45t-45 19h-512q-26 0-45-19t-19-45v-1408q0-26 19-45t45-19h512q26 0 45 19t19 45zm-896 0v1408q0 26-19 45t-45 19h-512q-26 0-45-19t-19-45v-1408q0-26 19-45t45-19h512q26 0 45 19t19 45z" ]
            []
        ]


play : Html msg
play =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1576 927l-1328 738q-23 13-39.5 3t-16.5-36v-1472q0-26 16.5-36t39.5 3l1328 738q23 13 23 31t-23 31z" ]
            []
        ]


plusCiricle : Html msg
plusCiricle =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1344 960v-128q0-26-19-45t-45-19h-256v-256q0-26-19-45t-45-19h-128q-26 0-45 19t-19 45v256h-256q-26 0-45 19t-19 45v128q0 26 19 45t45 19h256v256q0 26 19 45t45 19h128q26 0 45-19t19-45v-256h256q26 0 45-19t19-45zm320-64q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z" ]
            []
        ]


refresh : Html msg
refresh =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1639 1056q0 5-1 7-64 268-268 434.5t-478 166.5q-146 0-282.5-55t-243.5-157l-129 129q-19 19-45 19t-45-19-19-45v-448q0-26 19-45t45-19h448q26 0 45 19t19 45-19 45l-137 137q71 66 161 102t187 36q134 0 250-65t186-179q11-17 53-117 8-23 30-23h192q13 0 22.5 9.5t9.5 22.5zm25-800v448q0 26-19 45t-45 19h-448q-26 0-45-19t-19-45 19-45l138-138q-148-137-349-137-134 0-250 65t-186 179q-11 17-53 117-8 23-30 23h-199q-13 0-22.5-9.5t-9.5-22.5v-7q65-268 270-434.5t480-166.5q146 0 284 55.5t245 156.5l130-129q19-19 45-19t45 19 19 45z" ]
            []
        ]


stop : Html msg
stop =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1664 192v1408q0 26-19 45t-45 19h-1408q-26 0-45-19t-19-45v-1408q0-26 19-45t45-19h1408q26 0 45 19t19 45z" ]
            []
        ]

trash : Html msg
trash =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M704 1376v-704q0-14-9-23t-23-9h-64q-14 0-23 9t-9 23v704q0 14 9 23t23 9h64q14 0 23-9t9-23zm256 0v-704q0-14-9-23t-23-9h-64q-14 0-23 9t-9 23v704q0 14 9 23t23 9h64q14 0 23-9t9-23zm256 0v-704q0-14-9-23t-23-9h-64q-14 0-23 9t-9 23v704q0 14 9 23t23 9h64q14 0 23-9t9-23zm-544-992h448l-48-117q-7-9-17-11h-317q-10 2-17 11zm928 32v64q0 14-9 23t-23 9h-96v948q0 83-47 143.5t-113 60.5h-832q-66 0-113-58.5t-47-141.5v-952h-96q-14 0-23-9t-9-23v-64q0-14 9-23t23-9h309l70-167q15-37 54-63t79-26h320q40 0 79 26t54 63l70 167h309q14 0 23 9t9 23z" ]
            []
        ]


volumeOff: Html msg
volumeOff =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M1280 352v1088q0 26-19 45t-45 19-45-19l-333-333h-262q-26 0-45-19t-19-45v-384q0-26 19-45t45-19h262l333-333q19-19 45-19t45 19 19 45z" ]
            []
        ]

volumeUp : Html msg
volumeUp =
    svg
        [ viewBox "0 0 1792 1792" ]
        [ path 
            [ d "M832 352v1088q0 26-19 45t-45 19-45-19l-333-333h-262q-26 0-45-19t-19-45v-384q0-26 19-45t45-19h262l333-333q19-19 45-19t45 19 19 45zm384 544q0 76-42.5 141.5t-112.5 93.5q-10 5-25 5-26 0-45-18.5t-19-45.5q0-21 12-35.5t29-25 34-23 29-35.5 12-57-12-57-29-35.5-34-23-29-25-12-35.5q0-27 19-45.5t45-18.5q15 0 25 5 70 27 112.5 93t42.5 142zm256 0q0 153-85 282.5t-225 188.5q-13 5-25 5-27 0-46-19t-19-45q0-39 39-59 56-29 76-44 74-54 115.5-135.5t41.5-173.5-41.5-173.5-115.5-135.5q-20-15-76-44-39-20-39-59 0-26 19-45t45-19q13 0 26 5 140 59 225 188.5t85 282.5zm256 0q0 230-127 422.5t-338 283.5q-13 5-26 5-26 0-45-19t-19-45q0-36 39-59 7-4 22.5-10.5t22.5-10.5q46-25 82-51 123-91 192-227t69-289-69-289-192-227q-36-26-82-51-7-4-22.5-10.5t-22.5-10.5q-39-23-39-59 0-26 19-45t45-19q13 0 26 5 211 91 338 283.5t127 422.5z" ]
            []
        ]
