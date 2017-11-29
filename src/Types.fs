module Types

open Fable.Core
open Fable.Import.Pixi
open Fable.Import.Pixi.Particles
open System.Runtime.InteropServices.ComTypes
open System


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

type Event=
  | PointerDown

type SubState =
  | LaunchCurtain
  | LaunchTitle

type StateModel =
  | Init
  | Run
  | Prepare of SubState
  | DonePreparing
