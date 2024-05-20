module Main exposing (Model, Msg, main)

import Browser
import Codec
import Engine.Inventory as Inventory exposing (Inventory)
import Html exposing (Html, button, main_, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Ports



-- MODEL


type alias Model =
    Inventory


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    ( Codec.decodeInventory flags, Cmd.none )



-- UPDATE


type Msg
    = Increment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newInventory =
                    Inventory.addItem "Test" 1 model
            in
            ( newInventory
            , Ports.storeInventory (Codec.encodeInventory newInventory)
            )



-- VIEW


viewItem : ( String, Int ) -> Html msg
viewItem ( itemName, amount ) =
    Html.div [ Html.Attributes.class "item" ] [ Html.p [] [ Html.text (itemName ++ " (" ++ String.fromInt amount ++ ")") ] ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div [ Html.Attributes.class "buttons" ] [ button [ onClick Increment ] [ text "Test" ] ]
        , Html.div [ Html.Attributes.class "inventory" ]
            (model |> Inventory.toList |> List.map viewItem)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
