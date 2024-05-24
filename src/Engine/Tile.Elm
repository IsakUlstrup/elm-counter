module Engine.Tile exposing (Tile, TileContent(..), contentToString, harvest)

import Array exposing (Array)


type TileContent
    = Evergreen
    | Palm
    | Empty


contentToString : Tile -> String
contentToString tile =
    case tile.content of
        Evergreen ->
            "evergreen"

        Palm ->
            "palm"

        Empty ->
            "empty"


type alias Tile =
    { content : TileContent
    , history : Array ( String, Int )
    }


harvest : Int -> Array Tile -> ( Maybe String, Array Tile )
harvest index tiles =
    let
        loot =
            Array.get index tiles |> Maybe.map .content
    in
    case loot of
        Just Evergreen ->
            ( Just "\u{1FAB5}"
            , arrayUpdate index (\t -> { t | content = Empty } |> addHistory ( "\u{1FAB5}", 1 )) tiles
            )

        Just Palm ->
            ( Just "🍃"
            , arrayUpdate index (\t -> { t | content = Empty } |> addHistory ( "🍃", 1 )) tiles
            )

        _ ->
            ( Nothing
            , tiles
            )


addHistory : ( String, Int ) -> Tile -> Tile
addHistory item tile =
    { tile | history = tile.history |> Array.push item }


arrayUpdate : Int -> (a -> a) -> Array a -> Array a
arrayUpdate index f array =
    let
        item =
            Array.get index array
    in
    case item of
        Just i ->
            Array.set index (f i) array

        Nothing ->
            array
