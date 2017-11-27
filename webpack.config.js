var fs = require("fs");
var path = require("path");
var webpack = require("webpack");
var fableUtils = require("fable-utils");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
  "presets": [
    ["env", {
      "targets": {
        "browsers": ["last 2 versions", "safari >= 7"]
      },
      "modules": false
    }]
  ]
});

var isProduction = process.argv.indexOf("-p") >= 0;
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

module.exports = {
  devtool: "source-map",
  entry: resolve('./src/game.fsproj'),
  output: {
    filename: 'bundle.js',
    path: resolve('public'),
    publicPath: '/'
  },
  resolve: {
    modules: [resolve("node_modules/")]
  },
  node: {
    fs: 'empty'
  },
  externals: {
    "PIXI": "PIXI",
    "PIXI.extras": "PIXI.extras",
    "PIXI.loaders": "PIXI.loaders",
    "PIXI.settings": "PIXI.settings",
    "PIXI.filters": "PIXI.filters",
    "PIXI.interaction": "PIXI.interaction",
    "PIXI.mesh": "PIXI.mesh",
    "PIXI.particles":"PIXI.particles",
    "PIXI.sound":"PIXI.sound",
    "anime":"anime"
  },
  devServer: {
    contentBase: resolve('public'),
    port: 8080,
    inline: true
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader",
          options: {
            babel: babelOptions,
            define: isProduction ? [] : ["DEBUG"]
          }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      },
      {
        test: /\.sass$/,
        use: [
          "style-loader",
          "css-loader",
          "sass-loader"
        ]
      }
    ]
  }
};
