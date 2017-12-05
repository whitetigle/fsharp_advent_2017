module App

open Fable.Import.Browser
open Fable.Pixi


let path = "./img"

// Load assets and start game
CustomLoader.load Config.resources path ( fun loader res ->

  // now that our assets are ready: hide our spinner
  let spinner : HTMLDivElement = document.getElementById "spinner" :?> HTMLDivElement
  spinner.style.display <- "none"

  // start game
  Game.start 0. |> Async.StartImmediate
)
