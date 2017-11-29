module Config

open Fable.Import.Pixi
open Fable.Core
open Fable.Core.JsInterop

[<Emit("$0[$1]")>]
let Item (res:obj) (idx: string): obj = jsNative



let bigPicList =
  [
    "background"
    "backMountain"
    "border"
    "date"
    "frontMountain"
    "particle"
    "Snow50px"
    "snowflake"
    "star"
    "title"
    "backShadow"
    "m"
    "e"
    "r"
    "y"
    "c"
    "h"
    "i"
    "s"
    "t"
    "a"
  ]

let addAssetsToLoader assets extension path (loader:PIXI.loaders.Loader) =
  assets
    |> Seq.map( fun name ->
      (name,sprintf "%s/%s.%s" path name extension)
    )
    |> Seq.iter( fun (name,path) -> loader.add(name,path) |> ignore  )

let addTexturesToStore names res =

  names
    |> Seq.iter( fun name ->
      let texture = !!(Item res name)?texture
      Fable.Pixi.Assets.addTexture name texture
    )
