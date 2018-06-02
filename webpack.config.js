const webpack = require('webpack');
const path = require('path');

module.exports = () => {
  const plugins = [];

  const config = {
    module: {
      rules: [{
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader',
        options: {}
      }],
    },

    resolve: {
      extensions: ['.elm', '.js'],
    },

    entry: './src/index.js',

    output: {
      filename: 'index.js',
      path: path.resolve(__dirname, 'dist'),
      publicPath: '/dist',
    },

    devtool: 'source-map',

    devServer: {
      host: '0.0.0.0',
      port: 3000,
      stats: {
        modules: false,
      },
      disableHostCheck: true,
    }
  };

  return config;
};