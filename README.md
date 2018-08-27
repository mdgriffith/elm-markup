# Elm Markup

This is a parser for a succinct markup language allows you to write content and intermix Elm view functions.


## Basic Text Markup

We only use a very limited set of formatting characters.

- `/italic/` _italic_
- `*bold*` **bold**
- `~strike~` ~~strike~~
- `\`code\`` `code`
- `[link text](http://fruits.com)` to create a link.



## Blocks

Everything else is marked using blocks, which begin with `|` and the name of the block.

Here's the beginning of a blog post with a `title` block, which will render as an `h1`, some text, and then an image, some text and a list.

```
| title
    My fancy blog article

Welcome.  Have you hear about /cats/?  They're great.

| image http://placekitten/200/500
    Here's a great picture of my cat, pookie.

How much do I like cats?  Let's make a list.

| list
    - They're great.
    - Seriously, so great.
        - But, lists are pretty good too.

```

Blocks that come with the library are:

- `title` - The title of your document.  This is equivalent to an `h1`.  You should only have one of them.
- `header` - A header in your document, which is equivalent to `h2`.
- `list` - A nested list with an expected indentation of 4 spaces per level. As far as icons:
    - `-` indicates a bullet
    - `->` indicates an arrow
    - `1.` indicates it should be numbered.  Any number can work.

- `image` - Expects two strings, first the src, and then a description of the image.

But one of the great powers of this library is in writing custom blocks that suite your specific domain or style needs.

You can also restyle any aspect of existing or new blocks using `Mark.parseWith`.


## Reclaiming Typography

We can also reclaim some useful typography that is a bit awkward to handle otherwise.  Normal text will have the following transformations applied.

- `...` is converted to the ellipses unicode character.
- `"` Straight double quotes are [replaced with curly quotes](https://practicaltypography.com/straight-and-curly-quotes.html)
- `'` Single Quotes are replaced with apostrophes.  In the future we might differentiate between curly single quotes and apostrophes.
- `--` is replaced with an en dash `–`
- `---` is replaced with an em dash: `—`
- `<>` - will create a non-breaking space (`&nbsp;`).  This is not for manually increasing space(sequential `<>` tokens will only render as one `&nbsp;`), but to signify that the space between two words shouldn't break when wrapping.  Think of this like glueing two words together.

Escaping the start of any of these characters will cause the transformation to be skipped in that instance.

**Note** If you're not familiar with `en-dash` or `em-dash`, I definitely [recommend reading a small bit about it](https://practicaltypography.com/hyphens-and-dashes.html)—they're incredibly useful.



