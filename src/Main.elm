module Main exposing (Inventory, Model, Msg, main)

import Array exposing (Array)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine.Counter as Counter exposing (Counter)
import Filters
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes



-- INVENTORY


type alias Inventory =
    Dict String Int


emptyInventory : Inventory
emptyInventory =
    Dict.empty


addItem : String -> Inventory -> Inventory
addItem item inventory =
    case Dict.get item inventory of
        Just _ ->
            Dict.update item (Maybe.map ((+) 1)) inventory

        Nothing ->
            Dict.insert item 1 inventory


addItemIfDone : Counter -> Inventory -> Inventory
addItemIfDone counter inventory =
    if Counter.isDone counter then
        addItem counter.icon inventory

    else
        inventory



-- MODEL


type alias Model =
    { inventory : Inventory
    , counters : Array Counter
    }


init : Maybe String -> ( Model, Cmd Msg )
init _ =
    ( Model
        emptyInventory
        (Array.fromList
            [ Counter.new "🥥"
            , Counter.new "🥭"
            , Counter.new "🌺"
            , Counter.new "🐝"
            , Counter.new "🐞"
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
                        |> Array.map (Counter.tick dt)
                        |> Array.map Counter.addCount
                        |> Array.map (Counter.decayCount dt)
                , inventory = Array.foldl addItemIfDone model.inventory model.counters
              }
            , Cmd.none
            )



-- VIEW


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
        strokeOffset : Float
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
