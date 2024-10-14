# Curry Language Server

<img align="right" src="images/logo.svg">

[![Build](https://github.com/fwcd/curry-language-server/actions/workflows/build.yml/badge.svg)](https://github.com/fwcd/curry-language-server/actions/workflows/build.yml)
![Haskell](https://img.shields.io/badge/language-Haskell-7363a3.svg)
![BSD3 License](https://img.shields.io/badge/license-BSD3-333333.svg)

An experimental [language server](https://microsoft.github.io/language-server-protocol/) providing IDE support for the functional logic programming language [Curry](https://en.wikipedia.org/wiki/Curry_(programming_language)).

![Screenshot](images/screenshot.png)

## Building

To build the language server, you will need the build tool [Haskell Stack](https://docs.haskellstack.org). Once installed, you can run `stack build` to build the language server.

The final executable will be located in `$(stack path --dist-dir)/build/curry-language-server`.

If you wish to use the language server in an editor, you can also use `stack install` to install the binary into `~/.local/bin`. By adding this directory to your `PATH`, invoking `curry-language-server` will work from any directory.

## Editor Integration

To use the language server, you will need an editor that supports LSP. This usually involves pointing the LSP client towards the built executable and setting the transport method to `stdio`.

For Visual Studio Code, [this extension](https://github.com/fwcd/vscode-curry) can be used.

## Known Issues

If the language server has trouble locating an interface for the `Prelude`, you may need to add your Curry compiler's `lib` directory to your import paths, e.g. in your config under `curry.languageServer.importPaths` or in `<your project path>/.curry/language-server/paths.json` (which is a string array of import paths). Alternatively, you may also place a compiled version (`Prelude.icurry`) in the folder `<your workspace path>/.curry/language-server`.
