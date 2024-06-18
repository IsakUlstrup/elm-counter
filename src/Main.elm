module Main exposing (Inventory, Model, Msg, main)

import Array exposing (Array)
import Browser
import Browser.Events
import Engine.Counter as Counter exposing (Counter)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes


type alias Inventory =
    { current : Int
    , max : Int
    }



-- MODEL


type alias Model =
    { inventory : Inventory
    , counters : Array Counter
    }


init : Maybe String -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Inventory 0 100)
        (Array.fromList
            [ Counter.new "🥥"
            , Counter.new "🥭"
            , Counter.new "🍌"
            , Counter.new "🌴"
            ]
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = CounterPress Int
    | CounterRelease
    | Tick Float


setHolding : Int -> Int -> Counter -> Counter
setHolding targetIndex index counter =
    if targetIndex == index then
        Counter.setHolding counter

    else
        counter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterPress index ->
            ( { model | counters = Array.indexedMap (setHolding index) model.counters }
            , Cmd.none
            )

        CounterRelease ->
            ( { model | counters = Array.map Counter.setIdle model.counters }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | counters =
                    model.counters
                        -- |> Array.map (counterTransfer model.inventory)
                        |> Array.map (Counter.tick dt)
                        |> Array.map Counter.addCount

                -- , inventory = Array.foldl inventoryTransfer model.inventory model.counters
              }
            , Cmd.none
            )



-- VIEW


viewCounterTooltip : Counter -> Html msg
viewCounterTooltip counter =
    let
        isEmpty : Bool
        isEmpty =
            counter.count == 0

        isFull : Bool
        isFull =
            counter.count == counter.maxCount
    in
    Html.div
        [ Html.Attributes.class "custom-meter2"
        , Html.Attributes.classList [ ( "empty", isEmpty ), ( "full", isFull ) ]
        ]
        [ Html.progress
            [ Html.Attributes.value (String.fromInt counter.count)
            , Html.Attributes.max (String.fromInt counter.maxCount)
            ]
            []
        ]


viewCounter : Int -> Counter -> Html Msg
viewCounter index button =
    Html.button
        [ Html.Events.on "pointerdown" (Decode.succeed (CounterPress index))
        , Html.Events.on "pointerup" (Decode.succeed CounterRelease)
        , Html.Attributes.class (Counter.toString button)
        , Html.Attributes.class "button"
        , Html.Attributes.classList [ ( "done", Counter.isDone button ) ]
        ]
        [ viewCounterTooltip button
        , viewStrokeIcon button
        ]


strokeFilter : Svg msg
strokeFilter =
    Svg.filter [ Svg.Attributes.id "outline" ]
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


gooFilter : Svg msg
gooFilter =
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


viewStrokeIcon : Counter -> Svg msg
viewStrokeIcon counter =
    Svg.svg
        [ Svg.Attributes.viewBox "-100 -100 200 200"
        , Svg.Attributes.class "icon"
        ]
        [ Svg.defs []
            [ strokeFilter
            , gooFilter
            ]
        , Svg.text_
            [ Svg.Attributes.filter "url(#outline)"
            , Svg.Attributes.fontSize "6rem"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.dominantBaseline "middle"
            ]
            [ Svg.text counter.icon ]
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div
            [ Html.Attributes.class "counters"
            ]
            (model.counters |> Array.toList |> List.indexedMap viewCounter)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
