module Types

open Fable.Import.Pixi
open Fable.Import.Pixi.Particles


type SnowTarget = {
  X : float
  Y : float
  Angle : float
  AngleVariation : float
  Way : float
}

type Radius = float
type Curtain = {
  Graphics: PIXI.Graphics
  Radius : Radius
}

type Model = {
  SnowEmitters : (PIXI.particles.Emitter*SnowTarget) []
  Curtain: Curtain
}

type States =
  | Init
  | Run of Model
  | LaunchCurtain of Model
  | DoNothing
