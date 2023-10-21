
const path = require("path");
const packagejson = require("./package.json")

const libraryName = packagejson.name
/** @type {import('webpack').Configuration} */
module.exports = {
    mode: "none",
    entry: './src/index.ts',
    module: {
        rules: [
            {
                test: /\.ts$/,
                use: 'ts-loader',
                exclude: /node_modules/,
            },
        ],
    },
    plugins: [
        function () {
            this.hooks.done.tap({
                name: "dts-bundler"
            }, stats => {
                var dts = require('dts-bundle');
                dts.bundle({
                    name: libraryName,
                    main: './dist/types/index.d.ts',
                    out: '../index.d.ts',
                    removeSource: true,
                    outputAsModuleFolder: true // to use npm in-package typings
                });
            })
        }
    ],
    devtool: 'source-map',
    resolve: {
        extensions: [".ts", ".js"],
    },
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: 'index.js',
        library: {
            name: {
                root: libraryName,
                amd: libraryName,
                commonjs: libraryName,
            },
            type: 'umd',
        },

        // prevent error: `Uncaught ReferenceError: self is not define`
        globalObject: 'globalThis',

    }
};

