import { Elm } from "./src/Main.elm";
import { registerCustomElement } from "elm-mapbox";

import "mapbox-gl/dist/mapbox-gl.css";

registerCustomElement({
  token: ""
});


document.addEventListener("DOMContentLoaded", () => {
  const app = Elm.Main.init({ flags: {}, node: document.getElementById("elm-mount") });
  app.ports.projectPoint.subscribe(lngLat => {
    app.ports.projectedPoint.send(document.querySelector("elm-mapbox-map")._map.project(lngLat));
  });
});

// registerPorts(app);
