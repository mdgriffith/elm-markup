# Elm Markup

Elm Markup is about describing the structure you want for a document, and then having a parser that can parse that document or generate nice error messages.

It's about bringing the niceness we have around types to a markup language.

This will allow you to

1. Render Elm views in the middle of a markup document.
2. Parse a markup document to any data type you see fit. Markdown requires CSS to be styled because it parses to HTML.  However if you want to use `elm-ui` with `elm-markup`, just parse to `Element msg`!  Or to WebGL!  Or whatever!


In that way, this isn't really a replacement for Markdown or Asciidoc.  You define documents, meaning you can add blocks as you see fit, and you'll then have a parser for that document.

Let's check out an example.  Here's a `Mark.Document` which results in `Html msg`, and parses a `Title`, `Image`s, and `Text`.

```elm
document : Mark.Document (Html msg)
document =
    Mark.document
        (Html.article [])
        (Mark.manyOf
            [ title
            , image

            -- Toplevel Text
            , Mark.map (Html.p []) text
            ]
        )

text = 
    Mark.text <|
        \styles string ->
           if styles.bold || styles.italic || styles.strike then
                Html.span
                    [ Attr.classList
                        [ ( "bold", styles.bold )
                        , ( "italic", styles.italic )
                        , ( "strike", styles.strike )
                        ]
                    ]
                    [ Html.text string ]

            else
                Html.text string  

title =
    Mark.block "Title"
        (Html.h1 [])
        text

image =
    Mark.record "Image"
        (\src description ->
            Html.img
                [ Html.Attributes.src src
                , Html.Attributes.alt description
                ] []
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "description" Mark.string
        |> Mark.toBlock
```

Then we can write a document that matches:

![Syntax Highlighted Elm Markup](https://github.com/mdgriffith/elm-markup/blob/master/examples/highlighted-code.png?raw=true)


You can see that `Blocks` begin with `|>` followed by the name of the block, and finally some indented content.  All indentation is based on spaces and is always a multiple of 4.

To get started, check out the `/examples` folder:

- `Basic` - A single file that defines a document and where the markup source is written in a literal string.  This should be useful to get you used to creating a document and rendering it.
- `Blog` - The same as `basic`, but with external markup files and a basic setup to use the `elm-markup` CLI.
- `Editor` - The same as `Blog`, except we've made a basic rich text editor for our document.

## Elm Markup CLI

There'a also an [`Elm Markup CLI`](https://github.com/mdgriffith/elm-markup-vscode), which can be used to check errors on external elm markup files.

## VSCode Extension

There's an [`Elm Markup VSCode Extension`](https://github.com/mdgriffith/elm-markup-vscode) which has syntax highlighting and automatic error detection using the `elm-markup` cli.


