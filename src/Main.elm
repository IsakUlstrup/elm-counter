module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, button, main_, p, text)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1
            , Ports.storeCount (model + 1)
            )



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ p [] [ text <| "Inventory: " ++ String.fromInt model ]
        , button [ onClick Increment ] [ text "Click" ]
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
