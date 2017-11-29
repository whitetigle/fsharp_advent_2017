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

let [<Literal>] BASE_WIDTH = 1920.
let [<Literal>] BASE_HEIGHT = 1080.


let render (smodel: StateModel) (rmodel: RenderModel option) dispatch  (app:PIXI.Application) scale (renderer:PIXI.WebGLRenderer) (delta:float): RenderModel =

  let centerX = renderer.width * 0.5
  let centerY = renderer.height * 0.5

  let rmodel =
    match rmodel with
    | Some rmodel -> rmodel
    | None ->
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
      |> SpriteUtils.moveTo 0. renderer.height
      |> SpriteUtils.addToContainer hillOne
      |> ignore

      PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture "frontMountain")
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Left SpriteUtils.YAnchor.Bottom
      |> SpriteUtils.moveTo 0. renderer.height
      |> SpriteUtils.addToContainer hillTwo
      |> ignore

      let margin = 30.

      PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture "date")
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Right SpriteUtils.YAnchor.Bottom
      |> SpriteUtils.moveTo (renderer.width-margin) (renderer.height-margin)
      |> SpriteUtils.addToContainer hillTwo
      |> ignore

      PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture "border")
      |> SpriteUtils.scaleTo scale scale
      |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Center SpriteUtils.YAnchor.Middle
      |> SpriteUtils.moveTo centerX centerY
      |> SpriteUtils.addToContainer border
      |> ignore

      let texture = Fable.Pixi.SpriteUtils.getTexture "Snow50px"
      let config = Assets.getObj "snowEmitter"
      printfn "%A" config

      let emitter = PIXI.particles.Emitter( rearAnimLayer, !![|texture|], config.Value )
      emitter.updateOwnerPos(0.,0.)
      emitter.emit <- true

      let emitter1 = PIXI.particles.Emitter( middleAnimLayer, !![|texture|], config.Value )
      emitter1.updateOwnerPos(0.,0.)
      emitter1.emit <- true

      let emitter2 = PIXI.particles.Emitter( middleAnimLayer, !![|texture|], config.Value )
      emitter2.updateOwnerPos(0.,0.)
      emitter2.emit <- true

      let emitter3 = PIXI.particles.Emitter( rearAnimLayer, !![|texture|], config.Value )
      emitter3.updateOwnerPos(0.,0.)
      emitter3.emit <- true

      let snowEmitters =
        [|
          (emitter,{X=0.;Y= renderer.height*0.5;Angle=0.;AngleVariation=400.;Way=1.0})
          (emitter1,{X=renderer.width * 0.5;Y= renderer.height*0.9;Angle=0.;AngleVariation=400.;Way= -1.0})
          (emitter3,{X=renderer.width * 0.5;Y= renderer.height*0.9;Angle=0.;AngleVariation=400.;Way= 1.0})
          (emitter2,{X=renderer.width;Y= renderer.height*0.5;Angle=0.;AngleVariation=400.;Way= -1.0})
        |]

      let curtain = PIXI.Graphics()
      root.mask <- !!curtain

      let newModel =
        {
           SnowEmitters = snowEmitters
           Curtain = Some {Graphics=curtain;Radius=0.}
        }
      newModel


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
        o.Item <- "Radius",renderer.width * 0.7
        o.targets <- Some !!curtain
        o.duration <- !!duration
  //          o.elasticity <- !!500.
        o.easing <- !!EaseInCirc
      )
      let instance : AnimInstance = Fable.AnimeUtils.GetInstance (Some options)
      instance.run <- fun _ ->
        curtain.Graphics.beginFill(0xFFFFFF) |> ignore
        curtain.Graphics.drawCircle(renderer.width * 0.5,renderer.height * 0.5,curtain.Radius) |> ignore
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
      let space = 100. * scale
      let maxSpace = (float message.Length) * space
      let xMargin = (renderer.width - maxSpace) * 0.5 + space * 0.5

      message
      |> Seq.iteri( fun i char ->

        match char with
        | SPACE -> ()
        | _ ->
          let pixname = pixname char

          let s =
            PIXI.Sprite (Fable.Pixi.SpriteUtils.getTexture pixname)
            |> SpriteUtils.scaleTo scale scale
            |> SpriteUtils.anchorTo SpriteUtils.XAnchor.Center SpriteUtils.YAnchor.Top
            |> SpriteUtils.moveTo (xMargin + (float i) * space ) renderer.height
            |> SpriteUtils.addToLayer "rearAnimLayer"

          let duration = 500.
          let startupDelay = 1000.
          let target = s.position
          let options = jsOptions<AnimInput> (fun o ->
            o.Item <- "y", renderer.height * 0.4
            o.targets <- Some !!target
            o.duration <- !!duration
            o.elasticity <- !!1000.
            o.easing <- !!EaseInOutElastic
            o.delay <- !!((float i) * duration + startupDelay)
          )
          Fable.AnimeUtils.GetInstance (Some options) |> ignore
      )
      DonePreparing |> dispatch
      rmodel

  | Run ->

    let updatedEmitters =
      [|
        for (emitter, data) in rmodel.SnowEmitters do
          let way = data.Way
          let x = data.X + ( 3. * way )
          let way =
            match way with
            | w when w < 0. ->
              if x < -500. then 1.0 else w
            | w when w > 0. ->
              if x > renderer.width + 500. then -1.0 else w

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


let awaitAnimationFrame() =
 Async.FromContinuations(fun (cont,_,_) ->
   window.requestAnimationFrame(Browser.FrameRequestCallback cont) |> ignore)


let startGame (app:PIXI.Application) scale (renderer:PIXI.WebGLRenderer) =

//  let veryFirstScreen = Transition (InterScreenType.PresentationScreen,None)
//  let mutable screen = veryFirstScreen

  let events = ResizeArray()
  let rec stateMachine (smodel: StateModel) (rmodel: RenderModel option) lastDelta: Async<unit> = async {
   let! delta = awaitAnimationFrame()
   let diff = delta - lastDelta
//   printfn "%f" (delta - lastDelta)
   let newState =
      (smodel, events)
      ||> Seq.fold (fun smodel ev ->
        match ev with
        | DonePreparing -> Run
        | _ -> ev
      )

   events.Clear()
   let newRender = render newState rmodel events.Add app scale renderer diff
   return! stateMachine newState (Some newRender) delta
  }
  let state = Init
  stateMachine state None


// start our main loop
let init() =

  let options = jsOptions<PIXI.ApplicationOptions> (fun o ->
    o.antialias <- Some true
  )

  let canvas : HTMLDivElement = (document.getElementById "xmasCanvas") :?> HTMLDivElement
  let scale = canvas.clientWidth / BASE_WIDTH
  let scale= if scale >= 1.0 then 1.0 else scale
  let app = PIXI.Application(BASE_WIDTH*scale, BASE_HEIGHT*scale, options)
  canvas.appendChild(app.view) |> ignore

  let renderer : PIXI.WebGLRenderer = !!app.renderer

  // We start by loading our assets
  let loader = PIXI.loaders.Loader()
  let path = "./img"
  [
    // sounds
//    ("hydro",sprintf "%s/hydro.ogg" path)

    // particles
    ("snowEmitter",sprintf "%s/snow.json" path)
    ("trailEmitter",sprintf "%s/trail.json" path)
    ("treepopEmitter",sprintf "%s/treepop.json" path)

  ]
  |> Seq.iter( fun (name,path) -> loader.add(name,path) |> ignore  )

  // add all our pictures
  Config.addAssetsToLoader Config.bigPicList "png" path loader

  loader.load( fun (loader:PIXI.loaders.Loader) (res:PIXI.loaders.Resource) ->

    // add our assets to our Asset Store
    Config.addTexturesToStore Config.bigPicList res

    // add our particle systems
    Assets.addObj "snowEmitter" res?snowEmitter?data
    Assets.addObj "trailEmitter" res?trailEmitter?data
    Assets.addObj "treepopEmitter" res?treepopEmitter?data

    // Let's have some fun now!
    startGame app scale renderer 0. |> Async.StartImmediate

  ) |> ignore

init()
