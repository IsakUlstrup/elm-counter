port module Ports exposing (storeInventory, storeIslands)


port storeInventory : String -> Cmd msg


port storeIslands : String -> Cmd msg
