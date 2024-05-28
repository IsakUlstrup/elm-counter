module Codec exposing
    ( decodeInventory
    , decodeTiles
    , encodeInventory
    , encodeTiles
    )

import Array exposing (Array)
import Engine.Inventory as Inventory exposing (Inventory)
import Engine.Tile exposing (Tile)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


encodeInventory : Inventory -> String
encodeInventory inventory =
    inventory
        |> Encode.int
        |> Encode.encode 0


decodeInventory : Maybe String -> Inventory
decodeInventory maybeInput =
    case maybeInput of
        Just inputString ->
            Decode.decodeString Decode.int inputString
                |> Result.withDefault Inventory.empty

        Nothing ->
            Inventory.empty



-- ISLAND


tileEncoder : Tile -> Encode.Value
tileEncoder tile =
    Encode.int tile


tileDecoder : Decoder Tile
tileDecoder =
    Decode.int



-- islandEncoder : Island -> Encode.Value
-- islandEncoder island =
--     Encode.list tileEncoder (Island.toList island)
-- islandDecoder : Decoder Island
-- islandDecoder =
--     Decode.list tileDecoder
--         |> Decode.andThen
--             (\i ->
--                 case Island.fromList i of
--                     Just island ->
--                         Decode.succeed island
--                     Nothing ->
--                         Decode.fail "invalid island"
--             )
-- islandsDecoder : Decoder (List Island)
-- islandsDecoder =
--     Decode.list islandDecoder
-- encodeIslands : List Island -> String
-- encodeIslands islands =
--     islands
--         |> Encode.list islandEncoder
--         |> Encode.encode 0


encodeTiles : Array Tile -> String
encodeTiles tiles =
    tiles
        |> Array.toList
        |> Encode.list tileEncoder
        |> Encode.encode 0


decodeTiles : String -> Array Tile
decodeTiles inputString =
    Decode.decodeString (Decode.list tileDecoder) inputString
        |> Result.map Array.fromList
        |> Result.withDefault Array.empty
