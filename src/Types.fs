module Types

open Fable.Core
open Fable.Import.Pixi
open Fable.Import.Pixi.Particles
open Fable.Import.Pixi.Sound


type Way = Left|Right

type SnowTarget = {
  X : float
  Y : float
  Angle : float
  AngleVariation : float
  Speed: float
  Way : Way
}

type Radius = float
type Curtain = {
  Graphics: PIXI.Graphics
  Radius : Radius
}

type SnowEmitters = (PIXI.particles.Emitter*SnowTarget) []

type RenderModel = {
  App: PIXI.Application
  SnowEmitters : SnowEmitters
  Scale:float
  Scenery: PIXI.Sprite list
}

[<StringEnum>]
type Chars =
  | M
  | E
  | R
  | Y
  | C
  | H
  | I
  | S
  | T
  | A
  | SPACE

type SubState =
  | RiseCurtain
  | LaunchTitle
  | PlayJingleBells
  | SpreadGreenStars of float * float

type StateModel =
  | Init
  | Render
  | Prepare of SubState
  | DonePreparing
