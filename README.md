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
            [ Mark.block "Title"
                (Html.h1 [])
                text
            , Mark.record2 "Image"
                (\src description ->
                    Html.img
                        [ Html.Attributes.src src
                        , Html.Attributes.alt description
                        ] []
                )
                (Mark.field "src" Mark.string)
                (Mark.field "description" Mark.string)

            -- Toplevel Text
            , Mark.map (Html.p []) text
            ]
        )

-- ignore `text` for a moment, it's covered later.
```

Then we can write a document that matches:

```elm-markup
| Title
    My fancy cat blog article


Welcome!  Have you heard about /cats/?  They're great.

| Image
    src = http://placekitten/200/500
    description = Here's a great picture of my cat, pookie.

How much do I like cats?  A bunch.

Let me begin my 87 part argument in favor of cats.

```

You can see that `Blocks` begin with `|` followed by the name of the block, and finally some indented content.  All indentation is based on spaces and is always a multiple of 4.

## Basic Text Markup

There is only a limited set of special characters for formatting text.

- `/italic/` _italic_
- `*bold*` **bold**
- `~strike~` ~~strike~~

Though again, the main power of the language You can also define custom inline elements.  The syntax for a custom inline element

- `{Highlight| some highlighted text }`

In the above document, there's a value called `text`, which captures how to format text.  Here's how it's defined:

```elm

text =
    Mark.text
        { view =
            (\(Mark.Text styles str) ->
                Html.span (List.map toStyles styles) [ Html.text str ]
            )
        -- define a custom inline
        , inlines =
            [ Mark.inline "Highlight"
                (\txt ->
                   Html.span [ Html.Attributes.class "highlight" ]
                        [ Html.text txt ]
                )
                |> Mark.inlineText
            ]
        -- we can replace strings before parsing them.
        , replacements = [ Mark.replacement "..." "…" ]
        }

toStyles style =
    case style of
        Mark.Bold ->
            [ Font.bold ]

        Mark.Italic ->
            [ Font.italic ]

        Mark.Strike ->
            [ Font.strike ]

```

## Principles

Here are the ideas I used to put this library together:

- There is one _unambiguous_ way to construct a given element.

- Block names are explicit.

- This library inherits Elm's wariness of infix or symbol based operators.  Instead of having a bunch of symbols and custom syntax rules to denote concepts, we can just use real words.

- The following should _always_ be easy:

  - Writing markup
  - Reading markup
  - Modifying markup

- In order to do this, the parser needs to be strict about form.  This means that invalid markup will make the parser fail with a nice error message instead of rendering half correct and half wonky.  Think of this as "No runtime errors" but for elm-markup and layout.

- You can add `custom blocks` and `custom inline elements`, which means embedding whatever elm `view` you'd like. This library is about letting you be expressive in creating content, just with some ground rules about the form it will take.

- On the flip side, we avoid directly embedding any other languages like HTML. My current feeling is that a document like this benefits hugely from being **high level** and embedding code can get really messy with weird details. Fortunately custom blocks are pretty convenient.  Note that I mean embedding other languages that affect the presentation, you could create a block to show some syntax highlighted HTML and that would be great!
