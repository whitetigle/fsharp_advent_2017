module Config

open Fable.Import.Pixi
open Fable.Core
open Fable.Core.JsInterop

[<Emit("$0[$1]")>]
let Item (res:obj) (idx: string): obj = jsNative


let snowConfig =
  """
{
	"alpha": {
		"start": 0.73,
		"end": 0.09
	},
	"scale": {
		"start": 0.15,
		"end": 0.001,
		"minimumScaleMultiplier": 0.5
	},
	"color": {
		"start": "#ffffff",
		"end": "#ffffff"
	},
	"speed": {
		"start": 200,
		"end": 200,
		"minimumSpeedMultiplier": 1
	},
	"acceleration": {
		"x": 4,
		"y": 1
	},
	"maxSpeed": 0,
	"startRotation": {
		"min": 50,
		"max": 70
	},
	"noRotation": false,
	"rotationSpeed": {
		"min": 1,
		"max": 100
	},
	"lifetime": {
		"min": 4,
		"max": 4
	},
	"blendMode": "normal",
	"ease": [
		{
			"s": 0,
			"cp": 0.379,
			"e": 0.548
		},
		{
			"s": 0.548,
			"cp": 0.717,
			"e": 0.676
		},
		{
			"s": 0.676,
			"cp": 0.635,
			"e": 1
		}
	],
	"frequency": 0.024,
	"emitterLifetime": -1,
	"maxParticles": 500,
	"pos": {
		"x": 0,
		"y": 0
	},
	"addAtBack": false,
	"spawnType": "rect",
	"spawnRect": {
		"x": 0,
		"y": -300,
		"w": 500,
		"h": 0
	}
}
"""

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
