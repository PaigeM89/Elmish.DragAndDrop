# Drag And Drop Examples

Install dependencies with `yarn install`.

Run the project with `yarn start` from this directory, then navigate to `localhost:8091` to see the examples.

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher
* [node.js](https://nodejs.org) with [npm](https://www.npmjs.com/)
* An F# editor like Visual Studio, Visual Studio Code with [Ionide](http://ionide.io/) or [JetBrains Rider](https://www.jetbrains.com/rider/).
* Preferably `yarn` to install the dependencies & run the project.

## Building and running the app

* Install JS dependencies: `yarn install`
* Install F# dependencies: `yarn start`
* After the first compilation is finished, in your browser open: http://localhost:8091/

Any modification you do to the F# code will be reflected in the web page after saving.

## Project structure

### npm

JS dependencies are declared in `package.json`, while `package-lock.json` is a lock file automatically generated.

### Webpack

[Webpack](https://webpack.js.org) is a JS bundler with extensions, like a static dev server that enables hot reloading on code changes. Fable interacts with Webpack through the `fable-loader`. Configuration for Webpack is defined in the `webpack.config.js` file. Note this sample only includes basic Webpack configuration for development mode, if you want to see a more comprehensive configuration check the [Fable webpack-config-template](https://github.com/fable-compiler/webpack-config-template/blob/master/webpack.config.js).

### Web assets

The `index.html` file and other assets like an icon can be found in the `public` folder.
