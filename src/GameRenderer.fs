module GameRenderer

open Fable.Core.JsInterop
open Fable.Import.Animejs
open Fable.Import.Pixi.Sound
open Fable.Pixi
open Types

module SU = SpriteUtils

let render (stateModel: StateModel) (renderModel: RenderModel option) dispatch (delta:float): RenderModel =

  let extractSize (rmodel:RenderModel) =
    let w,h = RendererHelper.getSize rmodel.App
    rmodel,w,h

  // the very first thing we do is check if we've got a render model to play with
  let rmodel, gameWidth, gameHeight =
    match renderModel with
    | Some rmodel ->

      // Ok we've got a model, let's use it along with helper values
      rmodel |> extractSize

    | None ->

      // our model is not yet ready so before we start let's prepare things up
      let app,scale = ApplicationHelper.prepare Config.hostElementForPixiApp Config.BASE_WIDTH Config.BASE_HEIGHT None
      let w,h = RendererHelper.getSize app
      let centerX = w * 0.5
      let centerY = h * 0.5

      // prepare our layers
      let root = Layers.add "root" app.stage
      let addToLayer parent name = Layers.add name parent |> ignore
      [
        "world"
        "rearAnimLayer"
        "hillOne"
        "middleAnimLayer"
        "hillTwo"
        "frontAnimLayer"
        "border"
      ] |> Seq.iter (addToLayer root)

      // add our scenery
      // but hide it
      let background =
        SU.fromTexture "background"
        |> SU.addToLayer "world"
        |> SU.toggleVisible

      let back =
        SU.fromTexture "backMountain"
        |> SU.scaleTo scale scale
        |> SU.anchorTo SU.XAnchor.Left SU.YAnchor.Bottom
        |> SU.moveTo 0. h
        |> SU.addToLayer "hillOne"
        |> SU.toggleVisible

      let front =
        SU.fromTexture "frontMountain"
        |> SU.scaleTo scale scale
        |> SU.anchorTo SU.XAnchor.Left SU.YAnchor.Bottom
        |> SU.moveTo 0. h
        |> SU.addToLayer "hillTwo"
        |> SU.toggleVisible

      let border =
        SU.fromTexture "border"
        |> SU.scaleTo scale scale
        |> SU.anchorTo SU.XAnchor.Center SU.YAnchor.Middle
        |> SU.moveTo centerX centerY
        |> SU.addToLayer "border"
        |> SU.toggleVisible


      let newModel =
        {
           App= app
           Scale=scale
           SnowEmitters = Snow.prepare w
           Scenery=[background;back;front;border]
        }
      newModel |> extractSize

  // Now we have a RenderModel ready to use we'll do things according to the StateModel
  match stateModel with
  | Init ->

    // Ok, now we're all set, let's rise our curtain
    Prepare RiseCurtain |> dispatch
    rmodel

  | Prepare what ->
    match what with
    | PlayJingleBells ->
      let sound = PIXI.sound.Sound.from(!!"assets/xmas.ogg")
      sound.play() |> ignore

      DonePreparing |> dispatch
      rmodel

    | RiseCurtain ->

      // Like all our animations, we will use the Prepare state
      // basically we prepare our animation and change the state when done
      // so here we'll launch our title animation once the curtain's been raised
      let curtainUp _ = (Prepare LaunchTitle) |> dispatch

      Curtain.prepare()
      |> Curtain.rise gameWidth gameHeight curtainUp

      // now the curtain is ready bring back our scenery to visible
      rmodel.Scenery
      |> Seq.iter (SU.toggleVisible >> ignore)

      // No need to prepare things twice
      // just call our rendering
      DonePreparing |> dispatch

      rmodel

    | LaunchTitle ->
      let pixname char =
        let s = string char
        s.ToLower()

      let message = [M;E;R;R;Y;SPACE;C;H;R;I;S;T;M;A;S]
      let space = 100. * rmodel.Scale
      let maxSpace = (float message.Length) * space
      let xMargin = (gameWidth - maxSpace) * 0.5 + space * 0.5

      let targetPosition = gameHeight * 0.4
      message
      |> Seq.iteri( fun i char ->

        match char with
        | SPACE -> ()
        | _ ->
          let pixname = pixname char
          let x = xMargin + (float i) * space

          let s =
            SU.fromTexture pixname
            |> SU.scaleTo rmodel.Scale rmodel.Scale
            |> SU.anchorTo SU.XAnchor.Center SU.YAnchor.Top
            |> SU.moveTo x gameHeight
            |> SU.addToLayer "rearAnimLayer"

          let duration = 500.
          let startupDelay = 1000.
          let target = s.position
          let options = jsOptions<AnimInput> (fun o ->
            o.Item <- "y", targetPosition
            o.targets <- Some !!target
            o.duration <- !!duration
            o.elasticity <- !!500.
            o.easing <- !!EaseInCubic
            o.delay <- !!((float i) * duration + startupDelay)
          )
          let instance = Fable.AnimeUtils.GetInstance (Some options)

          // when we're done, spread green stars above the letter
          instance.complete <- fun _ ->
            Prepare (SpreadGreenStars( x,targetPosition + 15. * rmodel.Scale)) |> dispatch
      )

      Prepare PlayJingleBells |> dispatch
      rmodel

    | SpreadGreenStars(x,y) ->

      let textures = ["Snow50px"]
      let config = "letterEmitter" // located under img/letterEmitter.json
      let layer = "rearAnimLayer"


      ParticlesEmitter.add layer textures config x y
      |> ParticlesEmitter.start

      DonePreparing |> dispatch
      rmodel

  | Render ->

    // update our emitters
    ParticlesEmitter.update delta

    // update our snow effects and model accordingly
    { rmodel with
       SnowEmitters = Snow.update rmodel.SnowEmitters gameWidth delta
    }
  | _ -> rmodel
