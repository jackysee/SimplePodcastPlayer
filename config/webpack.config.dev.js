const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const paths = require('../config/paths');
const HappyPack = require('happypack');
const happyThreadPool = HappyPack.ThreadPool({ size: 5 });

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
    }),

    new HappyPack({
      id: 'css',
      threadPool: happyThreadPool,
      loaders: [ 'style-loader', 'css-loader' ]
    }),

    new HappyPack({
      id: 'scss',
      threadPool: happyThreadPool,
      loaders: [ 'style-loader', 'css-loader', 'sass-loader' ]
    }),

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
