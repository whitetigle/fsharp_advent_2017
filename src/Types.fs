module Types

open Fable.Core
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

type RenderModel = {
  SimpleEmitters : PIXI.particles.Emitter list
  SnowEmitters : (PIXI.particles.Emitter*SnowTarget) []
  Curtain: Curtain option
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
  | LaunchCurtain
  | LaunchTitle
  | AddLetterAnim of float * float

type StateModel =
  | Init
  | Run
  | Prepare of SubState
  | DonePreparing
