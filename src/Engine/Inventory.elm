module Engine.Inventory exposing
    ( Inventory
    , addItem
    , empty
    )


type alias Inventory =
    Int


empty : Inventory
empty =
    0


addItem : Int -> Inventory -> Inventory
addItem item inventory =
    inventory + max 0 item
