module App

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.Animejs
open Fable.Import.Pixi
open Fable.Import.Pixi.Particles
open Fable.Pixi
open Types


let awaitAnimationFrame() =
 Async.FromContinuations(fun (cont,_,_) ->
   window.requestAnimationFrame(Browser.FrameRequestCallback cont) |> ignore)

let startGame =

  let events = ResizeArray()

  let rec stateMachine (smodel: StateModel) (rmodel: RenderModel option) lastDelta: Async<unit> = async {

   let! delta = awaitAnimationFrame()
   let diff = delta - lastDelta

   let newState =
      (smodel, events)
      ||> Seq.fold (fun smodel ev ->
        match ev with
        | DonePreparing -> Run
        | _ -> ev
      )
   events.Clear()
   let newRender = GameRenderer.render newState rmodel events.Add diff
   return! stateMachine newState (Some newRender) delta
  }

  let state = Init
  stateMachine state None


// start our main loop
let init() =

  let onLoad loader res =

    // hide spinner
    let spinner : HTMLDivElement = document.getElementById "spinner" :?> HTMLDivElement
    spinner.style.display <- "none"

    // start our game
    let lastDelta = 0.
    startGame lastDelta |> Async.StartImmediate

  // Load assets and start our animation
  let path = "./img"
  CustomLoader.load Config.resources path onLoad

init()
