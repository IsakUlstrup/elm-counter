module Suite exposing (inventory)

import Engine.Inventory as Inventory
import Expect
import Fuzz exposing (int)
import Test exposing (Test, describe, fuzz, test)


inventory : Test
inventory =
    describe "Inventory"
        [ describe "Add"
            [ test "Add item to empty inventory" <|
                \_ ->
                    Inventory.empty
                        |> Inventory.addItem "Test" 1
                        |> Inventory.toList
                        |> Expect.equalLists [ ( "Test", 1 ) ]
            , fuzz int "Add random amount of an item, numbers below 1 should be ignored" <|
                \randomAmount ->
                    let
                        expectList =
                            if randomAmount > 0 then
                                [ ( "Test", randomAmount ) ]

                            else
                                []
                    in
                    Inventory.empty
                        |> Inventory.addItem "Test" randomAmount
                        |> Inventory.toList
                        |> Expect.equalLists expectList
            , test "Add item wich already exists, item count should increase" <|
                \_ ->
                    Inventory.empty
                        |> Inventory.addItem "Test" 1
                        |> Inventory.addItem "Test" 1
                        |> Inventory.toList
                        |> Expect.equalLists [ ( "Test", 2 ) ]
            ]
        ]
