port module Ports exposing (storeInventory, storeTiles)


port storeInventory : String -> Cmd msg


port storeTiles : String -> Cmd msg
