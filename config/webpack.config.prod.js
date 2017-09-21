const webpack = require('webpack');
const paths = require('../config/paths');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const HappyPack = require('happypack');
const happyThreadPool = HappyPack.ThreadPool({ size: 5 });

const root = process.cwd();

module.exports = {
  bail: true,
  entry: [
    paths.entry
  ],
  output: {

    // The build folder.
    path: paths.dist,

    // Generated JS files.
    filename: 'js/[name].[chunkhash:8].js'
  },
  resolve: {
    modules: [ 'node_modules' ],
    extensions: [ '.js', '.elm' ],
    enforceExtension: false
  },
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.elm$/,
        exclude: [ /elm-stuff/, /node_modules/ ],
        use: [ 'happypack/loader?id=elm' ]
      },
      {
        test: /\.css$/,
        use: [ 'happypack/loader?id=css' ]
      },
      {
        test: /\.scss$/,
        use: [ 'happypack/loader?id=scss' ]
      }
    ]
  },
  plugins: [
    new HappyPack({
      id: 'elm',
      threadPool: happyThreadPool,
      loaders: [
        {
          loader: 'elm-webpack-loader',
          options: {
            pathToMake: paths.elmMake
          }
        }
      ]
    }),

    new HappyPack({
      id: 'css',
      threadPool: happyThreadPool,
      loaders: [ 'style-loader', 'css-loader' ]
    }),

    new HappyPack({
      id: 'scss',
      threadPool: happyThreadPool,
      loaders: ['style-loader','css-loader','sass-loader']
    }),

    new CleanWebpackPlugin([ 'dist' ], {
      root: root,
      verbose: true,
      dry: false
    }),

    new webpack.optimize.UglifyJsPlugin({
      compress: {
        warnings: false
      },
      output: {
        comments: false
      }
    }),

    new HtmlWebpackPlugin({
      inject: true,
      template: paths.template,
      favicon: paths.favicon,
      minify: {
        removeComments: true,
        collapseWhitespace: true,
        removeRedundantAttributes: true,
        useShortDoctype: true,
        removeEmptyAttributes: true,
        removeStyleLinkTypeAttributes: true,
        keepClosingSlash: true,
        minifyJS: true,
        minifyCSS: true,
        minifyURLs: true
      }
    }),

    new CopyWebpackPlugin([
      {from: paths.assets, to: 'assets'}
    ])

  ]
};
