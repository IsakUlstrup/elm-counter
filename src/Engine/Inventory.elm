module Engine.Inventory exposing (Inventory, addItem, empty, fromList, toList)

import Dict exposing (Dict)


type Inventory
    = Inv (Dict String Int)


empty : Inventory
empty =
    Inv Dict.empty


addItem : String -> Int -> Inventory -> Inventory
addItem itemName amount (Inv inventory) =
    if amount > 0 then
        case Dict.get itemName inventory of
            Just _ ->
                Inv <| Dict.update itemName (Maybe.map (\item -> item + amount)) inventory

            Nothing ->
                Inv <| Dict.insert itemName amount inventory

    else
        Inv inventory


fromList : List ( String, Int ) -> Inventory
fromList list =
    Inv <| Dict.fromList list


toList : Inventory -> List ( String, Int )
toList (Inv inventory) =
    Dict.toList inventory
