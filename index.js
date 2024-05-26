import { Elm } from "./src/Main.elm";


const startingState = {
    inventory: localStorage.getItem("inventory"),
    islands: localStorage.getItem("islands")
};

const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: startingState
});

// app.ports.storeInventory.subscribe(function (inventory) {
//     // console.log("inv:", inventory)
//     localStorage.setItem("inventory", inventory)
// });
app.ports.storeIslands.subscribe(function (islands) {
    console.log("islands:", islands)
    localStorage.setItem("islands", islands)
});
