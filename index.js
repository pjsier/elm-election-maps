import { Elm } from "./src/Main.elm";
import { registerCustomElement } from "elm-mapbox";

import "mapbox-gl/dist/mapbox-gl.css";

registerCustomElement({
  token: ""
});

Elm.Main.init({ flags: {}, node: document.getElementById("elm-mount") });

// registerPorts(app);