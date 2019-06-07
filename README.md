# A Markup that's best friends with Elm!

Elm Markup is a markup language that integrates closely with Elm.

You describe your document and get a parser that can parse that document or hand you nice error messages.

![Syntax Highlighted Elm Markup](https://github.com/mdgriffith/elm-markup/blob/master/examples/highlighted-code-small.png?raw=true)

![Example Error Message](https://github.com/mdgriffith/elm-markup/blob/master/examples/example-error-small.png?raw=true)

Check out the talk I gave at [Oslo Elm Day](https://www.youtube.com/watch?v=8Zd3ocr9Di8). Or just [read the v3 release article](https://github.com/mdgriffith/elm-markup/blob/master/design-decisions/WHY-ELM-MARKUP-V3.md), it gives more detail on why this project might be more than it seems.

## Getting Started

To get started, check out the `/examples` folder:

- `Basic` - A single file that defines a document and where the markup source is written in a literal string.  This should be useful to get you used to creating a document and rendering it.
- `Blog` - The same as `basic`, but with external markup files and a basic setup to use the `elm-markup` CLI.
- `Editor` - The same as `Blog`, except we've made a basic rich text editor for our document.

## Elm Markup CLI

There'a also an [`Elm Markup CLI`](https://github.com/mdgriffith/elm-markup-cli), which can be used to check errors on external elm markup files.

## VSCode Extension

There's an [`Elm Markup VSCode Extension`](https://github.com/mdgriffith/elm-markup-vscode) which has syntax highlighting and automatic error detection using the `elm-markup` cli.


