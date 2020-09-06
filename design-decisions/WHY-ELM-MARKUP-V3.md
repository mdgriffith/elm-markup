Hello!

It's finally come time to release [v3.0 of `elm-markup`](https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/)!  I'm very excited, this has been quite the endeavor.

I got really interested creating a markup language for two reasons.

1. Markdown doesn't play nicely with `elm-ui` or interactive views written in Elm.
2. I wanted to write interactive fiction for a video game I'm designing, which means intermingling styled text and structured data.  ...or just realizing everything should be structured/known data, but also lightweight and easy to write.

So, what is Elm Markup?

![Syntax Highlighted Elm Markup](https://github.com/mdgriffith/elm-markup/blob/master/examples/highlighted-code.png?raw=true)

Well, I gave a talk at [Oslo Elm Day](https://www.youtube.com/watch?v=8Zd3ocr9Di8) which might be a good place to start (Side note, Olso Elm Day is a *fantastic* conference.)

But to give a brief-ish summary:

1. Elm Markup is a lightweight markup language where the structure is defined beforehand.
2. You define your document in Elm in a similar way you'd define a JSON decoder.
3. Once defined, you can parse the markup and get a rendered result (which could be html, `elm-ui` Elements, or even just a list of records)

That's the basic functionality for a markup language, but you can go further.

Because we know the structure that the document should be, we can get nice Elm-esque errors:

![Example Error Message](https://github.com/mdgriffith/elm-markup/blob/master/examples/example-error.png?raw=true)

Beyond this, I wanted some other capabilities I didn't usually associate with a markup language:

- To parse your markup into an intermediate data structure called a `Mark.Parsed` (basically an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree))
- Make edits to `Parsed` through `Mark.Edit.update`.  (They will be checked to ensure they are valid edits)
- Render that `Parsed` into your result.
- Convert `Parsed` back into an elm-markup source.

This is cool because it means `elm-markup` can be the "brain" of a rich text editor.  Or really any sort of editor you'd like to make.  Graph-based note taker?  Definitely.  Embed an interactive drawing program in your blogpost?  Why not?

This goes back to reason `2.` that I was making this, which is that many game engines benefit from having bespoke, embedded editors in the game.

And because apparently I go nuts when I do a project, I also created

- An [Elm Markup CLI](https://github.com/mdgriffith/elm-markup-cli) which will check your markup source files (*.emu) for errors using any exposing `Mark.Document`s in your elm project.
- A [VS Code Extension](https://marketplace.visualstudio.com/items?itemName=mdgriffith.elm-markup-vscode) with syntax highlighting and error reporting based on the output of the CLI.

If your interested in getting started, [checkout the examples](https://github.com/mdgriffith/elm-markup/tree/master/examples)

I'm particularly excited about the [editor](https://github.com/mdgriffith/elm-markup/tree/master/examples/editor) example, which is the beginning of a rich text editor in Elm!

![Example of Rich Text Editor](https://github.com/mdgriffith/elm-markup/blob/master/examples/elm-markup-editor.gif?raw=true)

Though a caveat, it needs a number of improvements which I've listed in the Readme. My hope is that the example shows that some of the most difficult parts about creating an editor are taken care of. And my other hope is that all of this sparks your creativity.

Whew, this project has quite a lot of surface area, so let me know in the issues if I missed something.

Also, small appologies for the long post :).  I didn't even get into the other cool things that came along with this project like building a tolerant parser that can continue after a parsing error...maybe I should write that up too.

<3
