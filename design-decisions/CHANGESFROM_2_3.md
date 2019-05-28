# Elm Markup v3.0

There are a number of changes to `elm-markup` in `3.0`


## A move to compilation


In 2.0 you'd parse a document using `Mark.parse`

```elm
parse :
    Document result
    -> String
    -> Result (List (DeadEnd Context Problem)) result
```

But in 3.0 we're moving to compilation.


``` elm
{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure

{-| -}
type alias Partial data =
    { errors : List Error
    , result : data
    }

-- 
compile : Document data -> String -> Outcome (List Error) (Partial data) data

--
parse : Document data -> String -> Outcome (List Error) (Partial Parsed) Parsed
render : Document data -> Parsed -> Outcome (List Error) (Partial data) data
```


This allows a few cool features like
    1. Saving the intermediate AST(`Mark.Parsed`) that is created after a document is parsed but before it's rendered.
       1. We can then send `Edit`s to that intermediate structure and have it update really fast(basically we skip parsing, which is generally slow).  This allows us to make cool editors really easily.
    2. Being able to render a document even if it has errors in it via `Partial`

**Note** The parser was about 2x faster than 2.0 last time I checked due to how it's handling records.  So, that's nice!

### Errors are opaque, with renderers.

In `2.0`, the `Context` and `Problem` were both previously exposed.

In `3.0`, you'll get an `Error` and be able to render it with:

```elm
errorToString : Error -> String

type Theme
    = Dark
    | Light

errorToHtml : Theme -> Error -> List (Html.Html msg)
```

I'm also just realizing it probably makes sense to be able to convert an `Error` into a record such as 

```elm
type alias Details = 
    { title : String
    , message : List String
    , source : Maybe String
    }

errorToRecord : Error -> Details
```


## Adjustments to block syntax


Blocks moved from 

```elm
| MyBlock
    with some content

```
 to
```elm
|> MyBlock
    with some content

```

This makes the block names easier to read in documents, especially with syntax highlighing and ligatures such as in `Fira Code`.

I was finding that in common circumstances the single `|`, especially top-level, was hard to see.

It also kinda works because the arrow gives a visual clue that the content will be indented.


### Adjustments to Text

Inlines moved from 

```elm
{Link| some styled text | url = elm-lang.org }
```

to `Mark.annotation`, which looks like this:

```elm
[some styled text]{link| url = elm-lang.org }
```

Again due to readability and this probably feels familiar.  Actually part of me is wondering what I was thinking with that first syntax :sweat_smile:

**Newlines in pargraphs**

Newlines are now allowed in text blocks.  Only a full blank line will break a text block.

```
First paragraph.
Still first paragraph.

Second paragraph.


Third paragraph.
```

**Verbatim Text**

`Mark.verbatim` text is new for inlines.

```
Here is my sentence `where /some/ part is taken verbatim`
```

The string within te backtics will be taken verbatim, no interpretation of styling characters like `/`.

A verbatim selection can also have attributes attached to it:


```
Here is my sentence `where /some/ part is taken verbatim`{ highlight | color = Blue }
```


## Constraining values

You can now add constraints to any part of your document by using 
```elm
{-| -}
type alias CustomError =
    { title : String
    , message : List String
    }


{-| -}
verify : (a -> Result CustomError b) -> Block a -> Block b
```

This is pretty cool because you can now make:
- urls constrained to be valid and pointing at what you want.
- dates to be at the level of resolution you want.  Do you just want a year?  Year/month?  year/month/time?
- constrain `int`s and `float`s to a certain range.  Only positive ints?  Floats between 0 and 1?  Only prime numbers?
- Lists that have length restrictions.

All kinds of stuff!


## Recovering from errors

If a block does have an error in it, previously the parsing of the whole document would fail.  Not so great if you want to make an editor, or a realtime document previewer.

Now you can let the document know how to recover from an error using `onError`:

```elm
onError : a -> Block a -> Block a
```

This will render a block with a certain value if it has failed in some way.


## Editing

More on this once it's finished.  Suffice to say you'll be able to send updates to the `Mark.Parsed` data structure.

## Mark.andThen is removed

It's actually not possible to write because parsing and rendering are now separate.  So, we can't change parsing based on a rendered piece of data because we dont have it yet.

I'm not sure there was a strong usecase for it anyway, I think we just wanted `Mark.verify` instead.







## 2 -> 3 Diff:

```
This is a MAJOR change.

---- ADDED MODULES - MINOR ----

    Mark.Edit
    Mark.Error
    Mark.New


---- REMOVED MODULES - MAJOR ----

    Mark.Default


---- Mark - MAJOR ----

    Added:
        type Enumerated item
            = Enumerated
                  { icon : Mark.Icon, items : List.List (Mark.Item item) }
        type Icon  = Bullet | Number
        type Item item
            = Item
                  { index : ( Basics.Int, List.List Basics.Int )
                  , content : List.List item
                  , children : Mark.Enumerated item
                  }
        type Outcome failure almost success
            = Success success
            | Almost almost
            | Failure failure
        type alias Block data = Mark.Internal.Description.Block data
        type alias Document data = Mark.Internal.Description.Document data
        type alias Parsed = Mark.Internal.Description.Parsed
        type alias Partial data =
            { errors : List.List Mark.Error.Error, result : data }
        type alias Record a = Mark.Internal.Description.Record a
        type alias Replacement = Mark.Internal.Parser.Replacement
        type alias Styles =
            { bold : Basics.Bool, italic : Basics.Bool, strike : Basics.Bool }
        annotation :
            String.String
            -> (List.List ( Mark.Styles, String.String ) -> result)
            -> Mark.Record result
        commonReplacements : List.List Mark.Replacement
        compile :
            Mark.Document data
            -> String.String
            -> Mark.Outcome
                   (List.List Mark.Error.Error)
                   (Mark.Partial data)
                   data
        documentWith :
            (metadata -> body -> document)
            -> { metadata : Mark.Block metadata, body : Mark.Block body }
            -> Mark.Document document
        idToString : Mark.Edit.Id -> String.String
        onError : a -> Mark.Block a -> Mark.Block a
        record : String.String -> data -> Mark.Record data
        render :
            Mark.Document data
            -> Mark.Parsed
            -> Mark.Outcome
                   (List.List Mark.Error.Error)
                   (Mark.Partial data)
                   data
        stringToId : String.String -> Maybe.Maybe Mark.Edit.Id
        textWith :
            { view : Mark.Styles -> String.String -> rendered
            , replacements : List.List Mark.Replacement
            , inlines : List.List (Mark.Record rendered)
            }
            -> Mark.Block (List.List rendered)
        toBlock : Mark.Record a -> Mark.Block a
        toString : Mark.Parsed -> String.String
        tree :
            String.String
            -> (Mark.Enumerated item -> result)
            -> Mark.Block item
            -> Mark.Block result
        verbatim :
            String.String -> (String.String -> result) -> Mark.Record result
        verify :
            (a -> Result.Result Mark.Error.Custom b)
            -> Mark.Block a
            -> Mark.Block b
        withId : (Mark.Edit.Id -> a -> b) -> Mark.Block a -> Mark.Block b

    Removed:
        type Block data
        type Context  = InBlock String | InInline String | InRecordField String
        type Document result
        type Field value
        type Inline result
        type Nested item
            = Nested { content : item, children : List (Nested item) }
        type Problem
            = ExpectingIndent Int
            | InlineStart
            | InlineEnd
            | BlockStart
            | Expecting String
            | ExpectingBlockName String
            | ExpectingInlineName String
            | ExpectingFieldName String
            | NonMatchingFields { expecting : List String, found : List String }
            | MissingField String
            | RecordError
            | Escape
            | EscapedChar
            | Newline
            | Space
            | End
            | Integer
            | FloatingPoint
            | InvalidNumber
            | UnexpectedEnd
            | CantStartTextWithSpace
            | UnclosedStyles (List Style)
            | UnexpectedField
                  { found : String, options : List String, recordName : String }
        type Replacement
        type Style  = Bold | Italic | Strike
        type Text  = Text (List Style) String
        advanced : Parser Context Problem result -> Block result
        andThen : (a -> Block b) -> Block a -> Block b
        exactly : String -> value -> Block value
        inline : String -> result -> Inline result
        inlineString : String -> Inline (String -> result) -> Inline result
        inlineText : Inline (List Text -> result) -> Inline result
        multiline : Block String
        nested :
            { item : Block item, start : Block icon }
            -> Block (List (Nested ( icon, List item )))
        record10 :
            String
            -> (
            one
            -> two
            -> three
            -> four
            -> five
            -> six
            -> seven
            -> eight
            -> nine
            -> ten
            -> data
            )
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Field seven
            -> Field eight
            -> Field nine
            -> Field ten
            -> Block data
        record2 :
            String
            -> (one -> two -> data)
            -> Field one
            -> Field two
            -> Block data
        record3 :
            String
            -> (one -> two -> three -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Block data
        record4 :
            String
            -> (one -> two -> three -> four -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Block data
        record5 :
            String
            -> (one -> two -> three -> four -> five -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Block data
        record6 :
            String
            -> (one -> two -> three -> four -> five -> six -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Block data
        record7 :
            String
            -> (one -> two -> three -> four -> five -> six -> seven -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Field seven
            -> Block data
        record8 :
            String
            -> (
            one -> two -> three -> four -> five -> six -> seven -> eight -> data
            )
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Field seven
            -> Field eight
            -> Block data
        record9 :
            String
            -> (
            one
            -> two
            -> three
            -> four
            -> five
            -> six
            -> seven
            -> eight
            -> nine
            -> data
            )
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Field seven
            -> Field eight
            -> Field nine
            -> Block data
        startWith :
            (start -> rest -> result)
            -> Block start
            -> Block rest
            -> Block result
        stub : String -> result -> Block result

    Changed:
      - field : String -> Block value -> Field value
      + field :
            String.String
            -> Mark.Block value
            -> Mark.Record (value -> result)
            -> Mark.Record result

      - parse :
            Document result
            -> String
            -> Result (List (DeadEnd Context Problem)) result
      + parse :
            Mark.Document data
            -> String.String
            -> Mark.Outcome
                   (List.List Mark.Error.Error)
                   (Mark.Partial Mark.Parsed)
                   Mark.Parsed

      - text :
            { view : Text -> rendered
            , inlines : List (Inline rendered)
            , replacements : List Replacement
            }
            -> Block (List rendered)
      + text :
            (Mark.Styles -> String.String -> text)
            -> Mark.Block (List.List text)

```