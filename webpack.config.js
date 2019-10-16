var path = require("path");

module.exports = {
  mode: "development",
  entry: "./src/RealWorldFable.fsproj",
  output: {
    path: path.join(__dirname, "./public"),
    filename: "bundle.js"
  },
  devServer: {
    contentBase: "./public",
    port: 8080
  },
  devtool: "eval-source-map",
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: "fable-loader"
      }
    ]
  }
};
