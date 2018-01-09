const path = require('path');
const webpack = require('webpack');
const merge = require('webpack-merge');
const common = require('./webpack.common.js');

const CopyWebpackPlugin = require('copy-webpack-plugin');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const ExtractTextPlugin = require("extract-text-webpack-plugin");
const CleanWebpackPlugin = require('clean-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = merge(common, {

    plugins: [
        new CleanWebpackPlugin([path.join(__dirname, 'build')]),

        new ExtractTextPlugin({
            filename: "main.[hash:8].css",
            disable: false,
        }),

        new UglifyJsPlugin({
            sourceMap: false,
            compress: {
                warnings: false,
            }
        }),

        new webpack.DefinePlugin({
            'process.env': {
                'NODE_ENV': JSON.stringify('production')
            }
        }),

        new HtmlWebpackPlugin({
            template: './static/index.html',
        })
    ],

    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack-loader',
             },
        ]
    }

});
