module Main exposing (Model, Msg, main)

import Array exposing (Array)
import Browser
import Codec
import Engine.Inventory as Inventory exposing (Inventory)
import Html exposing (Html, main_, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Ports



--TILE


type alias Tile =
    { icon : String
    , history : Array ( String, Int )
    }


addHistory : ( String, Int ) -> Tile -> Tile
addHistory item tile =
    { tile | history = tile.history |> Array.push item }



-- MODEL


type alias Model =
    { inventory : Inventory
    , tiles : List Tile
    }


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    ( Model (Codec.decodeInventory flags) [ Tile "🌲" Array.empty ], Cmd.none )



-- UPDATE


type Msg
    = ClickedTile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTile ->
            let
                newInventory : Inventory
                newInventory =
                    Inventory.addItem "\u{1FAB5}" 1 model.inventory
            in
            ( { model
                | inventory = newInventory
                , tiles = List.map (addHistory ( "\u{1FAB5}", 1 )) model.tiles
              }
            , Ports.storeInventory (Codec.encodeInventory newInventory)
            )



-- VIEW


viewItem : ( String, Int ) -> Html msg
viewItem ( itemName, amount ) =
    Html.div [ Html.Attributes.class "item" ]
        [ Html.p [] [ Html.text itemName ]
        , Html.sup [] [ Html.text (String.fromInt amount) ]
        ]


viewInventory : Inventory -> Html msg
viewInventory inventory =
    Html.div
        [ Html.Attributes.id "player-inventory"
        , Html.Attributes.attribute "popover" ""
        ]
        (inventory
            |> Inventory.toList
            |> List.map viewItem
        )


viewHistoryItem : ( String, Int ) -> Html msg
viewHistoryItem ( itemName, amount ) =
    Html.p [] [ Html.text (String.fromInt amount ++ " " ++ itemName) ]


viewTile : Tile -> Html Msg
viewTile tile =
    Html.button [ onClick ClickedTile ]
        [ text tile.icon
        , Html.div [ Html.Attributes.class "inventory-history" ]
            (tile.history
                |> Array.toList
                |> List.map viewHistoryItem
            )
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div [ Html.Attributes.class "tiles" ] (List.map viewTile model.tiles)
        , Html.div [ Html.Attributes.class "player-stats" ]
            [ Html.button [ Html.Attributes.attribute "popovertarget" "player-inventory" ]
                [ Html.text "Inventory"
                ]
            , viewInventory model.inventory
            ]
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
