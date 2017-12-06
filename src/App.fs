module App

open Fable.Import.Browser
open Fable.Pixi


let path = "./assets"

// Load assets and start game
CustomLoader.load Config.resources path (fun loader res ->

  // now that our assets are ready: hide our spinner
  // Note: the spinner is a div element located in index.html
  let spinner : HTMLDivElement = document.getElementById "spinner" :?> HTMLDivElement
  spinner.style.display <- "none"

  // start game
  Game.start 0. |> Async.StartImmediate
)
