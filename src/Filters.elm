module Filters exposing (goo, outline)

import Svg exposing (Svg)
import Svg.Attributes


outline : Svg msg
outline =
    Svg.filter [ Svg.Attributes.id "outline-filter" ]
        [ Svg.feGaussianBlur
            [ Svg.Attributes.in_ "SourceAlpha"
            , Svg.Attributes.stdDeviation "3"
            , Svg.Attributes.result "BLUR"
            ]
            []
        , Svg.feColorMatrix
            [ Svg.Attributes.in_ "BLUR"
            , Svg.Attributes.mode "matrix"
            , Svg.Attributes.values "1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 6 -3"
            , Svg.Attributes.result "GOO"
            ]
            []
        , Svg.feMorphology
            [ Svg.Attributes.in_ "GOO"
            , Svg.Attributes.operator "dilate"
            , Svg.Attributes.result "DILATED"
            , Svg.Attributes.radius "10"
            ]
            []
        , Svg.feFlood
            [ Svg.Attributes.floodColor "white"
            , Svg.Attributes.floodOpacity "100"
            , Svg.Attributes.result "BEIGE"
            ]
            []
        , Svg.feComposite
            [ Svg.Attributes.in_ "BEIGE"
            , Svg.Attributes.in2 "DILATED"
            , Svg.Attributes.operator "in"
            , Svg.Attributes.result "OUTLINE"
            ]
            []
        , Svg.feMerge []
            [ Svg.feMergeNode [ Svg.Attributes.in_ "OUTLINE" ] []
            , Svg.feMergeNode [ Svg.Attributes.in_ "SourceGraphic" ] []
            ]
        ]


goo : Svg msg
goo =
    Svg.filter [ Svg.Attributes.id "goo-filter" ]
        [ Svg.feGaussianBlur
            [ Svg.Attributes.in_ "SourceGraphic"
            , Svg.Attributes.stdDeviation "5"
            , Svg.Attributes.result "blur"
            ]
            []
        , Svg.feColorMatrix
            [ Svg.Attributes.in_ "blur"
            , Svg.Attributes.mode "matrix"
            , Svg.Attributes.values "1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 10 -5"
            , Svg.Attributes.result "goo"
            ]
            []
        ]
