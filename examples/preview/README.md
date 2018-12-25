# An Example Previewer

It's pretty easy to set up a preview server, here's an example one!


Start it by installing [`elm-live`](https://github.com/wking-io/elm-live).  

Then run the following from this directory:

```bash
elm-live src/Main.elm --open true --pushstate
```

You can then preview the documents in `article` by going to 

`http://localhost:8000/preview/articles/MyArticle`

which will parse `articles/MyArticle.emu`.

Make sure to leave off the `.emu` suffix.