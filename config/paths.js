const path = require('path');

module.exports = {
  cwd: path.resolve('.'),
  entry: path.resolve('./src/index'),
  assets: path.resolve('./src/assets'),
  dist: path.resolve('./dist'),
  template: path.resolve('./src/index.html'),
  favicon: path.resolve('./src/favicon.png'),
  ownModules: path.resolve(__dirname, '../node_modules'),
  scripts: path.resolve(__dirname, '../scripts'),
  elmMake: path.resolve(__dirname, '../node_modules/.bin/elm-make')
};
