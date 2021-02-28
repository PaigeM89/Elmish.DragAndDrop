
var path = require("path");

var CONFIG = {
    //indexHtmlTemplate: './src/index.html',
    fsharpEntry : './DragAndDrop.fs.js',
    outputDir: './output'
};

module.exports = {
    mode: "development",
    entry: "./Elmish.DragAndDrop.fsproj",
    // entry: {
    //     app: [resolve(CONFIG.fsharpEntry)]
    // },
    output: {
        //path: path.join(__dirname, "./public"),
        path: resolve(CONFIG.outputDir),
        filename: "bundle.js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./public",
        port: 8090,
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        },
        {
            test: /\.css$/i,
            use: ["style-loader", "css-loader"],
        }]
    }
}

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}