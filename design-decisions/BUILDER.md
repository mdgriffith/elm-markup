# Building New Blocks

One of the challenges in making a compiled markup language is that I want there to be some ability to construct new, valid blocks programattically and insert them into a document.


One way to do this is to construct these new blocks manually such as:

```elm

import Mark.New as New

myNewBlock =
    New.record "Circle"
        [ New.field "x" (New.int 5)
        , New.field "y" (New.int 10)
        , New.field "radius" (New.int 20)
        ]

```

Then, we can attempt to add this somewhere in the document.  Though, of course, that edit can fail if it doesn't match what the document is expecting.


Alternatively, I was wondering if we could build up a constructor function when we create the block in the document.

```elm
myBlock =
    Mark.Edit.record3 "Circle"
        (\make x y radius -> 
            let
                -- where we can create a new circle via `make`
                -- the new circle could then be added to the document by attaching it to a `Msg` for an `onClick` handler for example.
                newCircle = make 10 10 10

            in
            { x = x
            , y = y
            , radius = radius
            }
        )
        (Mark.field "x" Mark.int)
        (Mark.field "y" Mark.int)
        (Mark.field "radius" Mark.int)

```

In order to do something like that, we need something like this in `Block`

```

type Block data 
    = Block
        { builder : data -> New
        , converter : Description -> data
        }
```

But this falls apart if the data being produced is opaque like `Html msg`, because there would be no way to go from `Html msg -> String` or `Html msg -> Float`.

As well, because we can put arbitrary constraints on our data via `Mark.verify`, it's likely that `data -> New` would actually have to be `data -> Maybe New`.  I.e. what happens if you have an `Int` that is now constrained to be only positive.  We now have to create a function `Int -> New`, but if it's negative it should fail.

