module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Counter as Counter exposing (Counter)
import Engine.Zipper as Zipper exposing (Zipper)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    { inventory : Zipper Counter
    , counters : Zipper Counter
    }


init : Maybe String -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Zipper.new (Counter.new "🥭" 100) [ Counter.new "🥭" 100 |> Counter.setCount 0 ])
        (Zipper.new (Counter.new "🥭" 100 |> Counter.setCount 100) [ Counter.new "🥭" 100 |> Counter.setCount 50 ])
    , Cmd.none
    )



-- UPDATE


type Msg
    = CounterPress Int
    | CounterRelease
    | Tick Float


transferCount : Zipper Counter -> Zipper Counter -> ( Zipper Counter, Zipper Counter )
transferCount from to =
    ( Zipper.mapCurrent Counter.subtractCount from, Zipper.mapCurrent Counter.addCount to )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterPress index ->
            let
                ( newCounters, newInventory ) =
                    -- Counter.transferCount
                    transferCount
                        (model.counters |> Zipper.setCurrent index |> Zipper.mapCurrent Counter.setHolding)
                        model.inventory
            in
            ( { model
                | counters = newCounters
                , inventory = newInventory
              }
            , Cmd.none
            )

        CounterRelease ->
            ( { model | counters = Zipper.mapCurrent Counter.setIdle model.counters }
            , Cmd.none
            )

        Tick dt ->
            let
                ( newCounter, newInventory ) =
                    if Zipper.currentPred Counter.isDoneHolding model.counters then
                        transferCount model.counters model.inventory

                    else
                        ( model.counters, model.inventory )
            in
            ( { model
                | counters = Zipper.mapCurrent (Counter.tick dt) newCounter
                , inventory = newInventory
              }
            , Cmd.none
            )



-- VIEW


viewCounter : Int -> ( Bool, Counter ) -> Html Msg
viewCounter index ( selected, button ) =
    Html.button
        [ Html.Events.on "pointerdown" (Decode.succeed (CounterPress index))
        , Html.Events.on "pointerup" (Decode.succeed CounterRelease)
        , Html.Attributes.class (Counter.toString button)
        , Html.Attributes.class "button"
        , Html.Attributes.classList [ ( "selected", selected ) ]
        ]
        [ viewIconMeter button.icon button.maxCount button.count
        , Html.p [] [ Html.text (String.fromInt button.count) ]
        , Html.p [] [ Html.text ("#" ++ String.fromInt index) ]
        ]


viewIconMeter : String -> Int -> Int -> Html msg
viewIconMeter icon max value =
    let
        filledPercentage : Float
        filledPercentage =
            toFloat value / toFloat max * 100

        isEmpty : Bool
        isEmpty =
            value == 0

        isFull : Bool
        isFull =
            value == max
    in
    Html.div
        [ Html.Attributes.class "custom-meter"
        , Html.Attributes.classList [ ( "empty", isEmpty ), ( "full", isFull ) ]
        ]
        [ Html.div
            [ Html.Attributes.class "bar"
            , Html.Attributes.style "height" (String.fromFloat filledPercentage ++ "%")
            ]
            []
        , viewStrokeIcon icon
        ]


viewStrokeIcon : String -> Svg msg
viewStrokeIcon icon =
    Svg.svg
        [ Svg.Attributes.viewBox "-50 -50 100 100"
        , Svg.Attributes.class "icon"
        ]
        [ Svg.defs []
            [ Svg.filter [ Svg.Attributes.id "outline" ]
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
            ]
        , Svg.text_
            [ Svg.Attributes.filter "url('#outline')"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.dominantBaseline "central"
            , Svg.Attributes.fontSize "3rem"
            ]
            [ Svg.text icon ]
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div [ Html.Attributes.class "counters" ] (model.counters |> Zipper.toList |> List.indexedMap viewCounter)
        , Html.div [ Html.Attributes.class "counters" ] (model.inventory |> Zipper.toList |> List.indexedMap viewCounter)
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
