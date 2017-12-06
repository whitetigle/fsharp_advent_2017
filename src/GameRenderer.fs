module GameRenderer

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.Animejs
open Fable.Import.Pixi
open Fable.Import.Pixi.Particles
open Fable.Pixi
open Types

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
        "border"
      ] |> Seq.iter (addToLayer root)

      // add our scenery
      SpriteUtils.fromTexture "background"
      |> SpriteUtils.addToLayer "world"
      |> ignore

      SpriteUtils.fromTexture "backMountain"
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Left SpriteUtils.YAnchor.Bottom
      |> SpriteUtils.moveTo 0. h
      |> SpriteUtils.addToLayer "hillOne"
      |> ignore

      SpriteUtils.fromTexture "frontMountain"
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Left SpriteUtils.YAnchor.Bottom
      |> SpriteUtils.moveTo 0. h
      |> SpriteUtils.addToLayer "hillTwo"
      |> ignore

      SpriteUtils.fromTexture "border"
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Center SpriteUtils.YAnchor.Middle
      |> SpriteUtils.moveTo centerX centerY
      |> SpriteUtils.addToLayer "border"
      |> ignore

      let newModel =
        {
           App= app
           Scale=scale
           SnowEmitters = Snow.prepare w
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
    | RiseCurtain ->

      // Like all our animations, we will use the Prepare state
      // basically we prepare our animation and change the state when done
      // so here we'll launch our title animation once the curtain's been raised
      let curtainUp _ = (Prepare LaunchTitle) |> dispatch

      Curtain.prepare()
      |> Curtain.rise gameWidth gameHeight curtainUp

      // No need to prepare things twice
      // just call our rendering
      DonePreparing |> dispatch

      rmodel

    | LaunchTitle ->
      let pixname char =
        let s = string char
        printfn "%s" s
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
            PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture pixname)
            |> SpriteUtils.scaleTo rmodel.Scale rmodel.Scale
            |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Center SpriteUtils.YAnchor.Top
            |> SpriteUtils.moveTo x gameHeight
            |> SpriteUtils.addToLayer "rearAnimLayer"

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
          instance.complete <- fun _ ->
            Prepare (AddLetterAnim( x,targetPosition + 15. * rmodel.Scale)) |> dispatch
      )
      DonePreparing |> dispatch
      rmodel

    | AddLetterAnim(x,y) ->

      let textures = ["Snow50px"]
      let config = "letterEmitter"
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
