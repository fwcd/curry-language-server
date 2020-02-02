# Curry Language Server
An experimental [language server](https://microsoft.github.io/language-server-protocol/) providing IDE support for the functional logic programming language [Curry](https://en.wikipedia.org/wiki/Curry_(programming_language)).

![Icon](images/icon.png)

![Screenshot](images/screenshot.png)

## Known Issues
If the language server has trouble locating an interface for the `Prelude`, you may have to place a compiled version (`Prelude.fcy`, `Prelude.fint`, `Prelude.icurry`) in the folder `[your project path]/.curry`.
