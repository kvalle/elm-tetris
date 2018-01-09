const path = require('path');
const merge = require('webpack-merge');
const common = require('./webpack.common.js');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const ExtractTextPlugin = require("extract-text-webpack-plugin");
const CleanWebpackPlugin = require('clean-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = merge(common, {
    devtool: 'inline-source-map',

    plugins: [
        new CleanWebpackPlugin([path.join(__dirname, 'build')]),

        new ExtractTextPlugin({
            disable: true,
        }),

        new HtmlWebpackPlugin({
            template: './static/index.html',
        }),
    ],

    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-hot-loader!elm-webpack-loader?verbose=true&warn=true&debug=true&cwd='+__dirname,
            },
        ]
    },

    devServer: {
        inline: true,
        historyApiFallback: true,
        stats: { colors: true },
        contentBase: './src',
    },
});
