module Fable.Pixi

open Fable.Core
open Fable.Import.Pixi
open Fable.Import.Pixi.Particles
open Fable.Core.JsInterop
open Fable.Import.Browser
open System.Diagnostics.Tracing
open Fable.Import.Pixi.PIXI

type ExtendedSprite<'T> (texture:PIXI.Texture,data: 'T) =
  inherit PIXI.Sprite(texture)
  member this.Data = data

[<StringEnum>]
type ImgKind =
  | Png
  | Jpg
  | Gif

[<StringEnum>]
type SoundKind =
  | Ogg
  | Mp3

type AssetKind =
  | Img of ImgKind
  | Sound of SoundKind
  | Json

type AssetName = string
type AssetInfo = AssetName * AssetKind


[<Emit("$0[$1]")>]
let get (o:obj) (idx: string): obj = jsNative

[<RequireQualifiedAccess>]
module ResourceHelper =

  type ResourceObject = {
     data: obj
     texture: PIXI.Texture
  }

  let extractData obj fieldName =
    let typed : ResourceObject = (get obj fieldName) :?> ResourceObject
    typed.data

  let extractTexture obj fieldName =
    let typed : ResourceObject = (get obj fieldName) :?> ResourceObject
    typed.texture

[<RequireQualifiedAccess>]
module Event =

  type EventHandler = PIXI.interaction.InteractionEvent->unit

  [<StringEnum>]
  type PixiEvent =
    | Pointerdown
    | Pointerup
    | Pointeroutside
    | Pointermove
    | Pointertap

  let attach (ev: PixiEvent) (handler: EventHandler) (sprite: PIXI.Sprite) =
    sprite.on(!!(string ev), handler) |> ignore
    sprite

[<RequireQualifiedAccess>]
module AssetStore =
  let mutable textures = Map.empty<string,PIXI.Texture>
  let mutable objFiles = Map.empty<string,obj>

  let addTexture name texture =
    textures <- textures.Add(name,texture)

  let addObj name text =
    objFiles <- objFiles.Add(name,text)

  let getTexture name =
     textures.TryFind name

  let getObj name =
     objFiles.TryFind name

[<RequireQualifiedAccess>]
module RendererHelper =
  let getSize (app:PIXI.Application) =
      match app.renderer with
      | U2.Case1 r ->  (r.width,r.height)
      | U2.Case2 r->  (r.width,r.height)

[<RequireQualifiedAccess>]
module ApplicationHelper =
  let getApp width height (options:PIXI.ApplicationOptions option) =
    match options with
    | None ->
      PIXI.Application(width, height)
    | Some options ->
      PIXI.Application(width, height, options)

  let prepare elementId baseWidth baseHeight options =
    let canvas : HTMLDivElement = (document.getElementById elementId) :?> HTMLDivElement

    // Since the majority of screens are using a WIDE ratio,
    // we simply scale according to the element's width
    let scale = canvas.clientWidth / baseWidth
    let scale= if scale >= 1.0 then 1.0 else scale
    let app = getApp (baseWidth*scale)  (baseHeight*scale) options
    canvas.appendChild(app.view) |> ignore

    (app,scale)


[<RequireQualifiedAccess>]
module CustomLoader =

  let addToStore (assets:AssetInfo list) res =

    // add assets to our Asset store
    assets
      |> Seq.iter( fun (name,assetKind) ->
        match assetKind with
        | Img _ ->
          let texture = ResourceHelper.extractTexture res name
          AssetStore.addTexture name texture

        | Sound _ -> () // just don't do anything since sounds are now already available from pixi-sound

        | Json ->
          let data = ResourceHelper.extractData res name
          AssetStore.addObj name data

      )

  let load (assets:AssetInfo list) path (onLoad:PIXI.loaders.Loader->PIXI.loaders.Resource->unit) =

    let loader = PIXI.loaders.Loader()

    // add assets to PIXI loader load list
    assets
      |> Seq.map( fun (name,assetKind)->
        let extension =
          match assetKind with
          | Img k -> (string k).ToLower()
          | Sound s -> (string s).ToLower()
          | x -> (string x).ToLower()

        (name,sprintf "%s/%s.%s" path name extension)
      )
      |> Seq.iter( fun (name,path) -> loader.add(name,path) |> ignore  )

    // load assets using PIXI loader
    loader.load( fun (loader:PIXI.loaders.Loader) (res:PIXI.loaders.Resource) ->

      // store our assets in our AssetStore
      addToStore assets res

      // call callback
      onLoad loader res

    ) |> ignore


[<RequireQualifiedAccess>]
module Layers =
  let mutable layers = Map.empty<string,PIXI.Container>

  let add name (root:PIXI.Container) =
    let c = PIXI.Container()
    layers <- layers.Add(name,c)
    root.addChild c

  let useLayer name =
     let layer = layers.TryFind name
     match layer with
     | Some l -> l
     | None -> failwith (sprintf "Layer %s not found" name)

  let get name =
     layers.TryFind name

  let remove name =
     match (layers.TryFind name) with
     | Some layer ->

        // remove children if found any
        try
          layer.children
            |> Seq.iteri( fun i child ->
              layer.removeChild( layer.children.[i] ) |> ignore
            )
         with e -> printfn "No children where found in layer %s" name

        // remove layer from parent
        layer.parent.removeChild layer |> ignore

        layers <- layers.Remove name
     | None -> failwith (sprintf "unknwon layer %s" name)


[<RequireQualifiedAccess>]
module SpriteUtils =

  type YAnchor =
    | Top
    | Middle
    | Bottom

  type XAnchor =
    | Left
    | Center
    | Right


  let toggleVisible (sprite:PIXI.Sprite) =
    sprite.visible <- not sprite.visible
    sprite

  let hide (sprite:PIXI.Sprite) =
    sprite.visible <- false
    sprite

  let setVisible (sprite:PIXI.Sprite) =
    sprite.visible <- true
    sprite

  let getTexture name =
    let texture = AssetStore.getTexture name
    match texture with
    | Some t -> t
    | None ->  failwith (sprintf "unknown texture %s" name)

  let getObj name =
    let obj = AssetStore.getObj name
    match obj with
    | Some o -> o
    | None ->  failwith (sprintf "unknown object %s" name)

  let changeTexture name (sprite:PIXI.Sprite) =
    let texture = getTexture name
    sprite.texture <- texture
    sprite

  let fromTexture name =
    let texture = AssetStore.getTexture name
    match texture with
    | Some t ->
      PIXI.Sprite t
    | None ->  failwith (sprintf "unknown texture %s" name)

  let addToContainer (container:PIXI.Container) sprite =
    container.addChild sprite

  let addToLayer name sprite =
    let container = Layers.get name
    match container with
    | Some c ->
      c.addChild sprite
    | None ->  failwith (sprintf "unknown layer %s" name)

  let scaleTo x y (sprite:PIXI.Sprite) =
    let scale : PIXI.Point = !!sprite.scale
    scale.x <- x
    scale.y <- y
    sprite

  let setScale x y (sprite:PIXI.Sprite) =
    scaleTo x y sprite

  let moveTo x y (sprite:PIXI.Sprite) =
    let position : PIXI.Point = !!sprite.position
    position.x <- x
    position.y <- y
    sprite

  let setPosition x y (sprite:PIXI.Sprite) =
    moveTo x y sprite

  let setAnchor x y (sprite:PIXI.Sprite) =
    let anchor : PIXI.Point = !!sprite.anchor
    anchor.x <- x
    anchor.y <- y
    sprite

  let anchorTo xanchor yAnchor (sprite:PIXI.Sprite)=
    let anchorX =
      match xanchor with
      | Left -> 0.
      | Center -> 0.5
      | Right -> 1.

    let anchorY =
      match yAnchor with
      | Top -> 0.
      | Middle -> 0.5
      | Bottom -> 1.

    setAnchor anchorX anchorY sprite

  let setAlpha a (sprite:PIXI.Sprite) =
    sprite.alpha <- a
    sprite

  let makeInteractive (sprite:PIXI.Sprite) =
    sprite.interactive <- true
    sprite

  let rotate degrees (sprite:PIXI.Sprite) =
    sprite.rotation <- degrees * PIXI.Globals.DEG_TO_RAD
    sprite

  let makeButton (sprite:PIXI.Sprite) =
    sprite.interactive <- true
    sprite.buttonMode <- true
    sprite

  let addChild (sprite:PIXI.Sprite) (parent:PIXI.Sprite) =
    parent.addChild sprite

[<RequireQualifiedAccess>]
module ParticlesHelper =
  let mutable emitters :PIXI.particles.Emitter list = []

  let add layerName textures configName x y =
    let textures = [|
      for tname in textures do
        yield SpriteUtils.getTexture tname
    |]

    let config = SpriteUtils.getObj configName
    let layer = Layers.useLayer layerName

    let emitter = PIXI.particles.Emitter( layer, textures, config )
    emitter.updateOwnerPos(x,y)
    emitters <- emitters @ [emitter]
    emitter

  let start (emitter:PIXI.particles.Emitter) =
    emitter.emit <- true

  let update delta=
    for emitter in emitters do
      emitter.update (delta * 0.001)

    emitters <-
      emitters
      |> Seq.filter( fun e -> e.particleCount > 0. )
      |> Seq.toList
