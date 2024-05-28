module Main exposing (Flags, Model, Msg, main)

import Array exposing (Array)
import Browser
import Browser.Events
import Codec
import Engine.Inventory exposing (Inventory)
import Engine.Tile exposing (Tile)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Ports
import Random exposing (Generator)


arrayUpdate : (a -> a) -> Int -> Array a -> Array a
arrayUpdate f index tiles =
    case Array.get index tiles of
        Just tile ->
            tiles
                |> Array.set index (f tile)

        Nothing ->
            tiles


generateTiles : Float -> Array Tile -> Generator (Array Tile)
generateTiles dt tiles2 =
    let
        tileGenerator : Tile -> Generator Tile
        tileGenerator tile =
            if tile == 0 then
                Random.weighted ( 9000000 / dt, 0 ) [ ( 10 * dt, 1 ), ( 5 * dt, 10 ), ( 0.1 * dt, 100 ) ]

            else
                Random.constant tile

        helper : Random.Seed -> List Tile -> List Tile -> List Tile
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
        |> Random.map
            (\s -> helper s (Array.toList tiles2) [] |> Array.fromList)



-- MODEL


type alias Model =
    { inventory : Inventory
    , tiles : Array Tile
    , seed : Random.Seed
    }


type alias Flags =
    { inventory : Maybe String
    , tiles : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initInventory : Inventory
        initInventory =
            Codec.decodeInventory flags.inventory

        initTiles : Array Tile
        initTiles =
            flags.tiles
                |> Maybe.map Codec.decodeTiles
                |> Maybe.withDefault Array.empty
    in
    ( Model
        initInventory
        initTiles
        (Random.initialSeed 42)
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedTile Int
    | ClickedAddIsland
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTile position ->
            let
                newTiles : Array Tile
                newTiles =
                    model.tiles
                        |> arrayUpdate (\_ -> 0) position
            in
            ( { model | tiles = newTiles }
            , Cmd.batch
                [ Ports.storeTiles (Codec.encodeTiles newTiles)
                , Ports.storeInventory (Codec.encodeInventory model.inventory)
                ]
            )

        ClickedAddIsland ->
            let
                newTiles : Array Tile
                newTiles =
                    Array.push 0 model.tiles
            in
            ( { model | tiles = newTiles }
            , Ports.storeTiles (Codec.encodeTiles newTiles)
            )

        Tick dt ->
            let
                ( newTiles, newSeed ) =
                    Random.step (generateTiles dt model.tiles) model.seed
            in
            ( { model
                | tiles = newTiles
                , seed = newSeed
              }
            , Cmd.none
            )



-- VIEW


viewTile : ( Int, Tile ) -> Html Msg
viewTile ( index, tile ) =
    Html.button
        [ Html.Attributes.class "tile"
        , Html.Attributes.classList [ ( "zero", tile == 0 ) ]
        , Html.Events.onMouseDown (ClickedTile index)
        ]
        [ Html.text (String.fromInt tile) ]


viewAddIsland : Html Msg
viewAddIsland =
    Html.button
        [ Html.Attributes.class "add-island"
        , Html.Events.onMouseDown ClickedAddIsland
        ]
        [ Html.text "Add Island" ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        ((model.tiles
            |> Array.toIndexedList
            |> List.map viewTile
         )
            ++ [ viewAddIsland ]
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
