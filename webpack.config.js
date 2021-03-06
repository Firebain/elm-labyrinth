module.exports = (env, argv) => {
  let isProduction = argv.mode === "production"

  return {
    entry: './src/index.js',

    output: {
      path: __dirname + '/docs',
      filename: 'app.js'
    },

    resolve: {
      extensions: ['.js', '.elm']
    },

    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: {
            loader: 'elm-webpack-loader',
            options: {
              optimize: isProduction,
              debug: !isProduction
            }
          }
        }
      ],

      noParse: /\.elm$/
    },

    devServer: {
      contentBase: './docs',
      hot: true,
      inline: true,
      stats: 'errors-only'
    }
  }
};