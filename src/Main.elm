module Main exposing (Inventory, Model, Msg, main)

import Array exposing (Array)
import Browser
import Browser.Events
import Engine.Counter as Counter exposing (Counter)
import Filters
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
-- viewCounterTooltip : Counter -> Html msg
-- viewCounterTooltip counter =
--     let
--         isEmpty : Bool
--         isEmpty =
--             counter.count == 0
--         isFull : Bool
--         isFull =
--             counter.count == counter.maxCount
--     in
--     Html.div
--         [ Html.Attributes.class "custom-meter2"
--         , Html.Attributes.classList [ ( "empty", isEmpty ), ( "full", isFull ) ]
--         ]
--         [ Html.progress
--             [ Html.Attributes.value (String.fromInt counter.count)
--             , Html.Attributes.max (String.fromInt counter.maxCount)
--             ]
--             []
--         ]


viewCounter : Int -> Counter -> Html Msg
viewCounter index button =
    Html.button
        [ Html.Events.on "pointerdown" (Decode.succeed (CounterPress index))
        , Html.Events.on "pointerup" (Decode.succeed CounterRelease)
        , Html.Attributes.class (Counter.toString button)
        , Html.Attributes.class "button"
        , Html.Attributes.classList [ ( "done", Counter.isDone button ) ]
        ]
        [ viewStrokeIcon button
        ]


viewRadialProgress : Int -> Int -> Svg msg
viewRadialProgress max value =
    let
        strokeOffset =
            100 - (toFloat value / toFloat max * 100)
    in
    Svg.circle
        [ Svg.Attributes.class "radial-progress"
        , Svg.Attributes.r "90"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.strokeWidth "10"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.pathLength "100"
        , Svg.Attributes.strokeDasharray "100"
        , Svg.Attributes.strokeDashoffset (String.fromFloat strokeOffset)
        ]
        []


viewStrokeIcon : Counter -> Svg msg
viewStrokeIcon counter =
    Svg.svg
        [ Svg.Attributes.viewBox "-100 -100 200 200"
        , Svg.Attributes.class "icon"
        ]
        [ Svg.defs []
            [ Filters.outline
            , Filters.goo
            ]
        , viewRadialProgress counter.maxCount counter.count
        , Svg.text_
            [ Svg.Attributes.filter "url(#outline-filter)"
            , Svg.Attributes.fontSize "8rem"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.dominantBaseline "central"
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
