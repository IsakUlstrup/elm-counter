module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Html, button, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes



-- BUTTON


type ButtonState
    = Idle
    | Holding Float


setIdle : Counter -> Counter
setIdle button =
    { button | state = Idle }


setHolding : Counter -> Counter
setHolding button =
    { button | state = Holding 100 }


type alias Counter =
    { count : Int
    , maxCount : Int
    , state : ButtonState
    }


tick : Float -> Counter -> Counter
tick dt button =
    case button.state of
        Idle ->
            button

        Holding time ->
            if time == 0 then
                { button | state = Holding 100 }

            else
                { button | state = Holding ((time - dt) |> max 0) }


addCount : Counter -> Counter
addCount button =
    { button | count = button.count + 1 }


subtractCount : Counter -> Counter
subtractCount button =
    { button | count = button.count - 1 |> max 0 }


transferCount : Counter -> Counter -> ( Counter, Counter )
transferCount from to =
    if from.count > 0 then
        ( subtractCount from, addCount to )

    else
        ( from, to )


isDoneHolding : Counter -> Bool
isDoneHolding button =
    case button.state of
        Holding time ->
            time == 0

        _ ->
            False


toString : Counter -> String
toString button =
    case button.state of
        Idle ->
            "idle"

        Holding _ ->
            "holding"



-- MODEL


type alias Model =
    { inventory : Counter
    , counter : Counter
    }


init : Maybe String -> ( Model, Cmd Msg )
init _ =
    ( Model (Counter 0 100 Idle) (Counter 100 100 Idle)
    , Cmd.none
    )



-- UPDATE


type Msg
    = CounterPress
    | CounterRelease
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterPress ->
            let
                ( newCounter, newInventory ) =
                    transferCount model.counter model.inventory
            in
            ( { model
                | counter = setHolding newCounter
                , inventory = newInventory
              }
            , Cmd.none
            )

        CounterRelease ->
            ( { model | counter = model.counter |> setIdle }
            , Cmd.none
            )

        Tick dt ->
            let
                ( newCounter, newInventory ) =
                    if isDoneHolding model.counter then
                        transferCount model.counter model.inventory

                    else
                        ( model.counter, model.inventory )
            in
            ( { model
                | counter = tick dt newCounter
                , inventory = newInventory
              }
            , Cmd.none
            )



-- VIEW


viewCounter : Counter -> Html Msg
viewCounter button =
    Html.button
        [ Html.Events.on "pointerdown" (Decode.succeed CounterPress)
        , Html.Events.on "pointerup" (Decode.succeed CounterRelease)
        , Html.Attributes.class (toString button)
        , Html.Attributes.class "button"
        ]
        [ viewIconMeter "🥭" button.maxCount button.count
        , Html.p [] [ Html.text (String.fromInt button.count) ]
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
        [ Html.h3 [] [ Html.text "Counter" ]
        , viewCounter model.counter
        , Html.h3 [] [ Html.text "Inventory" ]
        , viewCounter model.inventory
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
