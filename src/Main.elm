module Main exposing (Model, Msg, main)

import Array exposing (Array)
import Browser
import Codec
import Engine.Inventory as Inventory exposing (Inventory)
import Engine.Tile exposing (Tile)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events exposing (onClick)
import Ports



--TILE
-- MODEL


type alias Model =
    { inventory : Inventory
    , tiles : Array Tile
    }


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    ( Model (Codec.decodeInventory flags)
        ([ Tile (Just "🌲") Array.empty
         , Tile (Just "🌳") Array.empty
         , Tile (Just "🌴") Array.empty
         ]
            |> Array.fromList
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedTile Int
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTile index ->
            let
                ( loot, newTiles ) =
                    Engine.Tile.harvest index model.tiles

                newInventory : Inventory
                newInventory =
                    case loot of
                        Just item ->
                            Inventory.addItem item 1 model.inventory

                        Nothing ->
                            model.inventory
            in
            ( { model
                | inventory = newInventory
                , tiles = newTiles
              }
            , Ports.storeInventory (Codec.encodeInventory newInventory)
            )

        Tick _ ->
            ( model, Cmd.none )



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
        [ Html.h3 [] [ Html.text "Inventory" ]
        , Html.div [ Html.Attributes.class "items" ]
            (inventory
                |> Inventory.toList
                |> List.map viewItem
            )
        ]


viewHistoryItem : ( String, Int ) -> Html msg
viewHistoryItem ( itemName, amount ) =
    Html.p [] [ Html.text (String.fromInt amount ++ " " ++ itemName) ]


viewTile : ( Int, Tile ) -> Html Msg
viewTile ( index, tile ) =
    Html.button [ onClick (ClickedTile index) ]
        [ Html.text (Maybe.withDefault " " tile.icon)
        , Html.div [ Html.Attributes.class "inventory-history" ]
            (tile.history
                |> Array.toList
                |> List.map viewHistoryItem
            )
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div [ Html.Attributes.class "tiles" ]
            (model.tiles
                |> Array.toIndexedList
                |> List.map viewTile
            )
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
    -- Browser.Events.onAnimationFrameDelta Tick
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
