import { Elm } from "./src/Main.elm";

const storedState = localStorage.getItem("count");
const startingState = storedState ? parseInt(storedState) : null;

const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: startingState
});

app.ports.storeCount.subscribe(function (count) {
    localStorage.setItem("count", count)
});

