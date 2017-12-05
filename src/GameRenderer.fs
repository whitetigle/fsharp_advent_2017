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

let render (smodel: StateModel) (rmodel: RenderModel option) dispatch (delta:float): RenderModel =


  let extractSize (rmodel:RenderModel) =
    let w,h = RendererHelper.getSize rmodel.App
    let centerX = w * 0.5
    let centerY = h * 0.5
    rmodel,w,h,centerX,centerY

  let rmodel, w, h, centerX,centerY =
    match rmodel with
    | Some rmodel ->
      rmodel |> extractSize

    | None ->

      let app,scale = ApplicationHelper.prepare Config.hostElementForPixiApp Config.BASE_WIDTH Config.BASE_HEIGHT None
      let w,h = RendererHelper.getSize app
      let centerX = w * 0.5
      let centerY = h * 0.5

      let root = Layers.add "root" app.stage

      let backLayer = Layers.add "world" root
      let rearAnimLayer = Layers.add "rearAnimLayer" root
      let hillOne = Layers.add "hillOne" root
      let middleAnimLayer = Layers.add "middleAnimLayer" root
      let hillTwo = Layers.add "hillTwo" root
      let frontAnimLayer = Layers.add "frontAnimLayer" root
      let border = Layers.add "border" root

      PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture "background")
      |> SpriteUtils.addToContainer backLayer
      |> ignore

      PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture "backMountain")
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Left SpriteUtils.YAnchor.Bottom
      |> SpriteUtils.moveTo 0. h
      |> SpriteUtils.addToContainer hillOne
      |> ignore

      PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture "frontMountain")
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Left SpriteUtils.YAnchor.Bottom
      |> SpriteUtils.moveTo 0. h
      |> SpriteUtils.addToContainer hillTwo
      |> ignore

      PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture "border")
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Center SpriteUtils.YAnchor.Middle
      |> SpriteUtils.moveTo centerX centerY
      |> SpriteUtils.addToContainer border
      |> ignore

      let texture = Fable.Pixi.SpriteUtils.getTexture "Snow50px"
      let config = AssetStore.getObj "snowEmitter"
      printfn "%A" config

      let snowEmitters =
        [
          (rearAnimLayer,0.,1.0)
          (middleAnimLayer,w,-1.0)
          (middleAnimLayer,w * 0.6,-1.0)
          (rearAnimLayer,0.,-1.0)
          (rearAnimLayer,w*0.5,1.0)
        ]
        |> Seq.map( fun (layer, x, way) ->
          let emitter = PIXI.particles.Emitter( layer, !![|texture|], config.Value )
          emitter.updateOwnerPos(0.,0.)
          emitter.emit <- true
          (emitter,{X=x;Y= w*0.5;Angle=0.;AngleVariation=500.;Way=way})
        )
        |> Seq.toArray

      let curtain = PIXI.Graphics()
      root.mask <- !!curtain

      let newModel =
        {
           App= app
           Scale=scale
           SimpleEmitters = []
           SnowEmitters = snowEmitters
           Curtain = Some {Graphics=curtain;Radius=0.}
        }
      newModel |> extractSize


  match smodel with
  | Init ->
    printfn "%A" smodel
    Prepare LaunchCurtain |> dispatch
    rmodel

  | Prepare what ->
    match what with
    | LaunchCurtain ->
      let curtain =
        match rmodel.Curtain with
        | Some c -> c
        | None -> failwith "No curtain found there"

      let duration = 4000.
      let options = jsOptions<AnimInput> (fun o ->
        o.Item <- "Radius",w * 0.7
        o.targets <- Some !!curtain
        o.duration <- !!duration
  //          o.elasticity <- !!500.
        o.easing <- !!EaseInCirc
      )
      let instance : AnimInstance = Fable.AnimeUtils.GetInstance (Some options)
      instance.run <- fun _ ->
        curtain.Graphics.beginFill(0xFFFFFF) |> ignore
        curtain.Graphics.drawCircle(w * 0.5,h * 0.5,curtain.Radius) |> ignore
        curtain.Graphics.endFill() |> ignore
      instance.complete <- fun _ -> (Prepare LaunchTitle) |> dispatch
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
      let xMargin = (w - maxSpace) * 0.5 + space * 0.5

      let targetPosition = h * 0.4
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
            |> SpriteUtils.moveTo x h
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

        let texture = Fable.Pixi.SpriteUtils.getTexture "Snow50px"
        let config = AssetStore.getObj "letterEmitter"
        let layer = Layers.get "rearAnimLayer"
        let updatedModel =
          match layer with
          | Some l ->
            let emitter = PIXI.particles.Emitter( l, !![|texture|], config.Value )
            emitter.updateOwnerPos(x,y)
            emitter.emit <- true
            { rmodel with SimpleEmitters=rmodel.SimpleEmitters @ [emitter]}

          | None -> rmodel

        DonePreparing |> dispatch
        updatedModel

  | Run ->

    for emitter in rmodel.SimpleEmitters do
      emitter.update (delta * 0.001)

    let updatedEmitters =
      [|
        for (emitter, data) in rmodel.SnowEmitters do
          let way = data.Way
          let x = data.X + ( 3. * way )
          let way =
            match way with
            | ww when ww < 0. ->
              if x < -500. then 1.0 else ww
            | ww when ww > 0. ->
              if x > ww + 500. then -1.0 else ww

          let angle = JS.Math.sin( data.Angle ) * data.AngleVariation
          let y = data.Y + angle

          emitter.updateOwnerPos(x,y)
          emitter.update (delta * 0.001)

          yield
            emitter,
            { data with
                X=x
                Angle=data.Angle + 1.0
                Way=way
            }

      |]

    { rmodel with
        SnowEmitters = updatedEmitters
    }
  | _ -> rmodel
