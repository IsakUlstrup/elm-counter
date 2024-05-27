module Engine.Island exposing
    ( Island
    , Tile
    , empty
    , fromList
    , randomUpdate
    , toIndexedList
    , toList
    , updateTile
    )

import Array exposing (Array)
import Random exposing (Generator)


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


fromList : List Tile -> Maybe Island
fromList tiles =
    if List.length tiles == 9 then
        Just <| Island <| Array.fromList tiles

    else
        Nothing


toIndexedList : Island -> List ( Int, Tile )
toIndexedList (Island island) =
    Array.toIndexedList island


toList : Island -> List Tile
toList (Island island) =
    Array.toList island


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


randomUpdate : Island -> Generator Island
randomUpdate (Island island) =
    let
        tileGenerator : Tile -> Generator Tile
        tileGenerator tile =
            if tile == 0 then
                Random.weighted ( 50000, 0 ) [ ( 10, 1 ), ( 5, 10 ), ( 0.1, 100 ) ]

            else
                Random.constant tile

        helper seed tiles acum =
            case tiles of
                [] ->
                    acum

                t :: ts ->
                    let
                        ( newTile, newSeed ) =
                            Random.step (tileGenerator t) seed
                    in
                    helper newSeed ts (acum ++ [ newTile ])
    in
    Random.independentSeed
        |> Random.andThen
            (\s ->
                Random.constant (helper s (Array.toList island) [] |> Array.fromList |> Island)
            )
