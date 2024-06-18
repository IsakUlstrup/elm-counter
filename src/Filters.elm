module Filters exposing (goo, outline)

import Svg exposing (Svg)
import Svg.Attributes


outline : Svg msg
outline =
    Svg.filter [ Svg.Attributes.id "outline-filter" ]
        [ Svg.feMorphology
            [ Svg.Attributes.in_ "SourceAlpha"
            , Svg.Attributes.operator "dilate"
            , Svg.Attributes.result "DILATED"
            , Svg.Attributes.radius "3"
            ]
            []
        , Svg.feFlood
            [ Svg.Attributes.floodColor "beige"
            , Svg.Attributes.floodOpacity "1"
            , Svg.Attributes.result "PINK"
            ]
            []
        , Svg.feComposite
            [ Svg.Attributes.in_ "PINK"
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
