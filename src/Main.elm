module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, button, h3, main_, p, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Ports



-- MODEL


type alias Model =
    Int


init : Maybe Int -> ( Model, Cmd Msg )
init flags =
    case flags of
        Just count ->
            ( count, Cmd.none )

        Nothing ->
            ( 0, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1
            , Ports.storeCount (model + 1)
            )

        Decrement ->
            ( model - 1
            , Ports.storeCount (model - 1)
            )

        Reset ->
            ( 0
            , Ports.storeCount 0
            )



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ h3 [] [ text "Elm counter" ]
        , p [] [ text <| String.fromInt model ]
        , Html.div [ Html.Attributes.class "counter-controls" ]
            [ button [ onClick Increment ] [ text "+" ]
            , button [ onClick Decrement ] [ text "-" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program (Maybe Int) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
