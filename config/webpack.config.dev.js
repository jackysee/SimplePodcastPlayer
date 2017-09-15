const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const paths = require('../config/paths');

module.exports = {

  devtool: 'eval',

  entry: [

    // WebpackDevServer client.
    require.resolve('webpack-dev-server/client') + '?/',

    // Replacement runtime.
    require.resolve('webpack/hot/dev-server'),

    paths.entry
  ],
  output: {

    pathinfo: true,

    // The build folder.
    path: paths.dist,

    // Generated JS files.
    filename: 'dist/js/bundle.js',

    publicPath: '/'
  },
  resolve: {
    modules: ['node_modules'],
    extensions: [ '.js', '.elm' ],
    enforceExtension: false
  },
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.elm$/,
        exclude: [ /elm-stuff/, /node_modules/ ],
        use: [
          'elm-hot-loader',
          {
            loader: 'elm-webpack-loader',
            options: {
              verbose: true,
              warn: true,
              pathToMake: paths.elmMake,
              cwd: paths.cwd,
              forceWatch: true
            }
          }
        ]
      },
      {
        test: /\.css$/,
        use: [ 'style-loader', 'css-loader' ]
      },
      {
        test: /\.scss$/,
        use: [
          'style-loader',
          'css-loader',
          'sass-loader'
        ]
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      inject: true,
      template: paths.template,
      favicon: paths.favicon
    }),
    new CopyWebpackPlugin([
      {from: paths.assets, to: 'assets'}
    ]),
    new webpack.HotModuleReplacementPlugin()
  ]
};
