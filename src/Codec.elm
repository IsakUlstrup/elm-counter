module Codec exposing (decodeInventory, encodeInventory)

import Engine.Inventory as Inventory exposing (Inventory)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


itemEncoder : ( String, Int ) -> Encode.Value
itemEncoder ( itemName, amount ) =
    Encode.object
        [ ( "itemName", Encode.string itemName )
        , ( "amount", Encode.int amount )
        ]


itemDecoder : Decoder ( String, Int )
itemDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "itemName" Decode.string)
        (Decode.field "amount" Decode.int)


inventoryDecoder : Decoder Inventory
inventoryDecoder =
    Decode.list itemDecoder
        |> Decode.andThen (Inventory.fromList >> Decode.succeed)


encodeInventory : Inventory -> String
encodeInventory inventory =
    inventory
        |> Inventory.toList
        |> Encode.list itemEncoder
        |> Encode.encode 0


decodeInventory : Maybe String -> Inventory
decodeInventory input =
    case input of
        Just inputString ->
            case Decode.decodeString inventoryDecoder inputString of
                Ok inv ->
                    inv

                Err _ ->
                    Inventory.empty

        Nothing ->
            Inventory.empty
