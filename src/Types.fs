module Types

open Fable.Core
open Fable.Import.Pixi
open Fable.Import.Pixi.Particles
open Fable.Import.Pixi.Sound


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

type Emitters = PIXI.particles.Emitter list
type SnowEmitters = (PIXI.particles.Emitter*SnowTarget) []

type RenderModel = {
  App: PIXI.Application
  SimpleEmitters :Emitters
  SnowEmitters : SnowEmitters
  Curtain: Curtain option
  Scale:float
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
