module Snow

open Types
open Fable.Import
open Fable.Pixi
open Fable.Import.Pixi.Particles

let prepare screenWidth =

  let texture = SpriteUtils.getTexture "Snow50px"
  let config = AssetStore.getObj "snowEmitter"

  [
    ("rearAnimLayer",0.,Right)
    ("middleAnimLayer",screenWidth,Left)
    ("middleAnimLayer",screenWidth * 0.6,Left)
    ("rearAnimLayer",0.,Left)
    ("rearAnimLayer",screenWidth*0.5,Right)
  ]
  |> Seq.map( fun (layerName, x, way) ->
    let layer = Layers.useLayer layerName
    let emitter = PIXI.particles.Emitter( layer, [|texture|], config.Value )
    emitter.updateOwnerPos(0.,0.)
    emitter.emit <- true
    (emitter,{X=x;Y= screenWidth*0.5;Angle=0.;AngleVariation=500.;Way=way;Speed=3.0})
  )
  |> Seq.toArray

let update (emitters:SnowEmitters) gameWidth delta=
  [|
    for (emitter, data) in emitters do
      let x =
        data.X +
        match data.Way with
        | Left -> -data.Speed
        | Right -> data.Speed

      let way =
        match data.Way with
        | Left ->
          if x < -500. then Right else data.Way
        | Right ->
          if x > gameWidth + 500. then Left else data.Way

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
