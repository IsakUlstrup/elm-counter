import { Elm } from "./src/Main.elm";


const startingState = {
    inventory: localStorage.getItem("inventory"),
    tiles: localStorage.getItem("tiles")
};

const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: startingState
});

app.ports.storeInventory.subscribe(function (inventory) {
    // console.log("inv:", inventory)
    localStorage.setItem("inventory", inventory)
});
app.ports.storeTiles.subscribe(function (tiles) {
    console.log("tiles:", tiles)
    localStorage.setItem("tiles", tiles)
});
