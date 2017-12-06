module Config

open Fable.Pixi

// This is our list of assets
let resources =
  [
    "background", Img Png
    "backMountain",Img Png
    "border", Img Png
    "date", Img Png
    "frontMountain", Img Png
    "particle", Img Png
    "Snow50px", Img Png
    "snowflake", Img Png
    "star", Img Png
    "title",Img Png
    "backShadow",Img Png
    "m",Img Png
    "e",Img Png
    "r",Img Png
    "y",Img Png
    "c",Img Png
    "h",Img Png
    "i",Img Png
    "s",Img Png
    "t",Img Png
    "a",Img Png
    "letterEmitter",Json
    "snowEmitter",Json
    "trailEmitter",Json
    "treepopEmitter",Json
    "xmas",AssetKind.Sound Ogg
  ]

let [<Literal>] BASE_WIDTH = 1920.
let [<Literal>] BASE_HEIGHT = 1080.
let [<Literal>] hostElementForPixiApp = "xmasCanvas"
