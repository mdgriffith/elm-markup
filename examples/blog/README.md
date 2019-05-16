# Using External Markup

Our markup files are kept in `articles`, and our elm application is kept in `src`.

You'll need to install:

- The [`elm-markup`](https://github.com/mdgriffith/elm-markup-cli) CLI.
- and [`elm-live`](https://github.com/wking-io/elm-live) for a preview server.

Running `elm-markup` in this directory will list any errors that `articles/Article.emu` has.

You can run a preview server by installing [`elm-live`](https://github.com/wking-io/elm-live) and running:

```shell
elm-live src/Main.elm --open
```

The Elm application will make a `GET` request for the source of the article, and parse it when it arrives.

# Integration with VS Code

If you install the [VS Code Elm Markup extension](https://github.com/mdgriffith/elm-markup-vscode), you'll also get syntax highlighting and inline error reporting.