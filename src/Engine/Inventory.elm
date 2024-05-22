module Engine.Inventory exposing (Inventory, addItem, empty, fromList, historyToList, toList)

import Array exposing (Array)
import Dict exposing (Dict)


type Inventory
    = Inv { items : Dict String Int, history : Array ( String, Int ) }


empty : Inventory
empty =
    Inv
        { items = Dict.empty
        , history = Array.empty
        }


addHistory : ( String, Int ) -> Inventory -> Inventory
addHistory ( itemName, amount ) (Inv inventory) =
    Inv <| { inventory | history = Array.push ( itemName, amount ) inventory.history }


addItem : String -> Int -> Inventory -> Inventory
addItem itemName amount (Inv inventory) =
    if amount > 0 then
        (case Dict.get itemName inventory.items of
            Just _ ->
                { inventory
                    | items = Dict.update itemName (Maybe.map (\item -> item + amount)) inventory.items
                }

            Nothing ->
                { inventory
                    | items = Dict.insert itemName amount inventory.items
                }
        )
            |> Inv
            |> addHistory ( itemName, amount )

    else
        Inv inventory


fromList : List ( String, Int ) -> Inventory
fromList list =
    Inv <| { items = Dict.fromList list, history = Array.empty }


toList : Inventory -> List ( String, Int )
toList (Inv inventory) =
    Dict.toList inventory.items


historyToList : Inventory -> List ( String, Int )
historyToList (Inv inventory) =
    Array.toList inventory.history
