module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Codec
import Engine.Inventory exposing (Inventory)
import Engine.Island as Island exposing (Island, Tile)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Ports
import Random exposing (Generator)



{- Update island at index -}


updateIsland : Int -> (Island -> Island) -> List Island -> List Island
updateIsland targetIndex f islands =
    let
        helper index i =
            if index == targetIndex then
                f i

            else
                i
    in
    List.indexedMap helper islands


{-| Update tile at index on island at index
-}
updateTile : ( Int, Int ) -> (Tile -> Tile) -> List Island -> List Island
updateTile ( islandIndex, tileIndex ) f islands =
    islands
        |> updateIsland islandIndex (Island.updateTile tileIndex f)


spawnTiles : Island -> Generator Island
spawnTiles island =
    Island.randomUpdate island


islandsSpawnTiles : List Island -> Generator (List Island)
islandsSpawnTiles islands =
    let
        helper seed tiles acum =
            case tiles of
                [] ->
                    acum

                t :: ts ->
                    let
                        ( newTile, newSeed ) =
                            Random.step (spawnTiles t) seed
                    in
                    helper newSeed ts (newTile :: acum)
    in
    Random.independentSeed
        |> Random.andThen
            (\s ->
                Random.constant (helper s islands [])
            )



-- MODEL


type alias Model =
    { inventory : Inventory
    , islands : List Island
    , seed : Random.Seed
    }


type alias Flags =
    { inventory : Maybe String
    , islands : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initInventory =
            Codec.decodeInventory flags.inventory

        initIslands =
            flags.islands
                |> Maybe.map Codec.decodeIslands
                |> Maybe.withDefault
                    [ Island.empty ]
    in
    ( Model
        initInventory
        initIslands
        (Random.initialSeed 42)
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedTile ( Int, Int )
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTile position ->
            let
                newIslands =
                    model.islands
                        |> updateTile position (\_ -> 0)
            in
            ( { model | islands = newIslands }
            , Ports.storeIslands (Codec.encodeIslands newIslands)
            )

        Tick _ ->
            let
                ( newIslands, newSeed ) =
                    Random.step (islandsSpawnTiles model.islands) model.seed
            in
            ( { model | islands = newIslands, seed = newSeed }, Cmd.none )



-- VIEW


viewTile : Int -> ( Int, Tile ) -> Html Msg
viewTile islandIndex ( index, tile ) =
    Html.button
        [ Html.Attributes.class "tile"
        , Html.Attributes.classList [ ( "zero", tile == 0 ) ]
        , Html.Events.onMouseDown (ClickedTile ( islandIndex, index ))
        ]
        [ Html.text (String.fromInt tile) ]


viewIsland : Int -> Island -> Html Msg
viewIsland index island =
    Html.div [ Html.Attributes.class "island" ]
        (List.map (viewTile index) (Island.toIndexedList island))


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        (model.islands
            |> List.indexedMap viewIsland
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
