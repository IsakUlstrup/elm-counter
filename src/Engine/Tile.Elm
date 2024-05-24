module Engine.Tile exposing (Tile, harvest)

import Array exposing (Array)


type alias Tile =
    { icon : Maybe String
    , history : Array ( String, Int )
    }


harvest : Int -> Array Tile -> ( Maybe String, Array Tile )
harvest index tiles =
    let
        loot =
            Array.get index tiles |> Maybe.andThen .icon
    in
    case loot of
        Just _ ->
            ( Just "\u{1FAB5}"
            , arrayUpdate index (\t -> { t | icon = Nothing } |> addHistory ( "\u{1FAB5}", 1 )) tiles
            )

        Nothing ->
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


spawnResources : Array Tile -> Array Tile
spawnResources tiles =
    tiles
