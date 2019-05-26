# Building an Editor

Just like the `Blog` example, the files here are kept in `articles`, and our elm application is kept in `src`.

To start up this example, use 

```shell
elm-live src/Main.elm --open --start-page=editor.html -- --output=assets/editor.js
```

But! The exciting part of this example is that it shows how to build a basic web-editor for your document.

To do this we're going to be using `Mark.Edit` and `Mark.New`.


## Improvements

There are a lot of subtleties to making a text editor, even a limited one.  Who knew? :)

Here are some improvements that could be made to this editor example that I figured I'd list here in case you're interested in trying your hand at one.

I've tried ranking the estimated difficulty.

1. `easy` - `Shift + Arrow` should expand the selection range
2. `medium` - The text selection highlight should be rendered as one single polyline in Svg instead of a bunch of text boxes.  This will likely improve performance a bit as well. Ideally we'd like it to have the standard shape of a text selection. I think capturing multiple paragraphs might be a little tricky, but who knows!
3. `involved` - The `Selection.CharLayout` data structure could use some love.  My suspicion is that it's slow for larger documents.  It's a bounding box for every character after all.  The data structure likely needs to:
   1. Write a few benchmarks to see if/when it's actually an issue.
   2. Look up a range of boxes between two points.
      1. Subsequent lookups are likely near each other. 
   3. Be updated/recreated incrementally
4. `involved` - consider adopting an incremental strategy for rescanning the document (`Ports.send Ports.Rescan`).  Depends on `3.`
   - Just rescan what's needed.
   - Incrementally update `model.characterLayout`.
5. `?` - Expand the UI of the editor to allow inserting `Mark.Block`s, such as images. 
