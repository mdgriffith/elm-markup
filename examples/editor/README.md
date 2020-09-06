# Building an Editor

Just like the `Blog` example, the files here are kept in `articles`, and our elm application is kept in `src`.

To start up this example, use 

```shell
elm-live src/Main.elm --open --start-page=editor.html -- --output=assets/editor.js
```

But! The exciting part of this example is that it shows how to build a basic web-editor for your document.

To do this we're going to be using `Mark.Edit` and `Mark.New`.

The first thing to look at is `Mark.Edit.update`, which will attempt to update the document based on an edit you might have.


## Getting Oriented

There's a lot going on in this example, but I think there are really only two new things.

1. We manage the `Selection` entirely in Elm and only rely on the browser for character positions via `Selection.CharacterLayout`.
    - Oh boy, I tried a lot of other ways before landing on this one. Probably needs a blog post.
2. `Mark.Edit.update` updates our document if the edit is valid.


## Improvements

There are a lot of subtleties to making a text editor, even a limited one.  Who knew? :)

Here are some improvements that could be made to this editor example that I figured I'd list here in case you're interested in trying your hand at one.

I've tried ranking the estimated difficulty.

1. `easy` - `Shift + Arrow` should expand the selection range
2. `easy` - Resizing the window messes up the `Charlayout`.  I think we'd want to make it so that the `Charlayout` is not affected by window size instead of just rescanning the document on `Browser.Event.onResize`
3. `medium` - The text selection highlight should be rendered as one single polyline in Svg instead of a bunch of text boxes.  This will likely improve performance a bit as well. Ideally we'd like it to have the standard shape of a text selection. I think capturing multiple paragraphs might be a little tricky, but who knows!  Check out [MakePad](https://makepad.github.io/makepad/) for an example of a single "blob" shape for text selection.
4. `involved` - The `Selection.CharLayout` data structure could use some love.  My suspicion is that it's slow for larger documents.  It's a bounding box for every character after all. So, *after writing a few benchmarks to see if/when it's actually an issue*, the data structure likely needs to:
   1. Look up a range of boxes between two points.
      1. Subsequent lookups are likely near each other. 
   2. Be updated/recreated incrementally
5. `involved` - consider adopting an incremental strategy for rescanning the document (`Ports.send Ports.Rescan`).  Depends on `4.`
   - Just rescan what's needed.
   - Incrementally update `model.characterLayout`.
6. `?` - Expand the UI of the editor to allow inserting `Mark.Block`s, such as images. 
7. `?` - Use more `Html.Lazy` in document rendering if possible.  `Mark.Parsed` might make that a bit difficult, so we'll need to double check that we're getting the benefit beyond just sprinkling lazy everywhere.


I'm sure there's a bunch of other stuff.
