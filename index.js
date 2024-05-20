import { Elm } from "./src/Main.elm";

const storedState = localStorage.getItem("inventory");
const startingState = storedState ? JSON.parse(storedState) : null;

const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: storedState
});

app.ports.storeInventory.subscribe(function (inventory) {
    console.log("inv:", inventory)
    localStorage.setItem("inventory", inventory)
});
