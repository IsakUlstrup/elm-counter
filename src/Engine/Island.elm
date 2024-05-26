module Engine.Island exposing
    ( Island
    , Tile
    , empty
    , toIndexedList
    , updateTile
    )

import Array exposing (Array)


type alias Tile =
    Int


type Island
    = Island (Array Tile)



-- TODO: Ditch 3x3 requirement and opaque type


empty : Island
empty =
    List.range 1 9
        |> List.map (\_ -> 0)
        |> Array.fromList
        |> Island


toIndexedList : Island -> List ( Int, Tile )
toIndexedList (Island island) =
    Array.toIndexedList island


{-| Update tile at given index
-}
updateTile : Int -> (Tile -> Tile) -> Island -> Island
updateTile index f (Island island) =
    case Array.get index island of
        Just tile ->
            island
                |> Array.set index (f tile)
                |> Island

        Nothing ->
            Island island
