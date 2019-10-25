var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var CopyWebpackPlugin = require("copy-webpack-plugin");

var isProduction = !process.argv.find(
  v => v.indexOf("webpack-dev-server") !== -1
);
console.log(
  "Bundling for " + (isProduction ? "production" : "development") + "..."
);

var CONFIG = {
  indexHtmlTemplate: "./src/index.html",
  fsharpEntry: "./src/RealWorldFable.fsproj",
  outputDir: "./output",
  assetsDir: "./public",
  devServerPort: 8080,
  // When using webpack-dev-server, you may need to redirect some calls
  // to a external API server. See https://webpack.js.org/configuration/dev-server/#devserver-proxy
  devServerProxy: undefined,
  // Use babel-preset-env to generate JS compatible with most-used browsers.
  // More info at https://github.com/babel/babel/blob/master/packages/babel-preset-env/README.md
  babel: {
    presets: [
      [
        "@babel/preset-env",
        {
          modules: false,
          useBuiltIns: "usage",
          corejs: 3
          // This saves around 4KB in minified bundle (not gzipped)
          // "loose": true,
        }
      ]
    ]
  }
};

var commonPlugins = [
  new HtmlWebpackPlugin({
    filename: "index.html",
    template: CONFIG.indexHtmlTemplate
  })
];

module.exports = {
  mode: isProduction ? "production" : "development",
  entry: CONFIG.fsharpEntry,
  output: {
    path: path.join(__dirname, "./public"),
    filename: isProduction ? "[name].[hash].js" : "[name].js",
    devtoolModuleFilenameTemplate: info =>
      path.resolve(info.absoluteResourcePath).replace(/\\/g, "/")
  },
  devServer: {
    contentBase: "./public",
    port: 8080
  },
  devtool: isProduction ? "source-map" : "eval-source-map",
  optimization: {
    // Split the code coming from npm packages into a different file.
    // 3rd party dependencies change less often, let the browser cache them.
    splitChunks: {
      cacheGroups: {
        commons: {
          test: /node_modules/,
          name: "vendors",
          chunks: "all"
        }
      }
    }
  },
  plugins: isProduction
    ? commonPlugins.concat([
        new CopyWebpackPlugin([{ from: CONFIG.assetsDir }])
      ])
    : commonPlugins.concat([new webpack.HotModuleReplacementPlugin()]),
  resolve: {
    // See https://github.com/fable-compiler/Fable/issues/1490
    symlinks: false
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader",
          options: {
            babel: CONFIG.babel
          }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: CONFIG.babel
        }
      }
    ]
  }
};
