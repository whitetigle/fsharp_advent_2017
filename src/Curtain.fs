module Curtain

open Fable.Import.Pixi
open Fable.Import.Animejs
open Fable.Pixi
open Fable.Core.JsInterop
open Types

// The curtain is basically what we call a mask
// everything masked by a shape will be visible
// So here we will display everything that's inside
// our root container
let prepare() =
  let curtain = PIXI.Graphics()
  let root = Layers.useLayer "root"
  root.mask <- !!curtain
  {Graphics=curtain;Radius=0.}

// The animation is pretty straightforward
// we make the circle grow so that a larger part of th root container is visible
let rise gameWidth gameHeight onComplete (curtain:Curtain)=

  let options = jsOptions<AnimInput> (fun o ->

    // update the Radius value of our Curtain object
    o.Item <- "Radius",gameWidth * 0.7
    o.targets <- Some !!curtain
    o.duration <- !!4000. // 4 secs
//          o.elasticity <- !!500. // fun!
    o.easing <- !!EaseInCirc
  )

  let instance : AnimInstance = Fable.AnimeUtils.GetInstance (Some options)

  // make our mask circle grow
  instance.run <- fun _ ->
    curtain.Graphics.beginFill(0xFFFFFF) |> ignore
    curtain.Graphics.drawCircle(gameWidth * 0.5,gameHeight * 0.5,curtain.Radius) |> ignore
    curtain.Graphics.endFill() |> ignore

  instance.complete <- onComplete
