# Handling Fields

For `Mark.Record`, we have the slightly weird `Mark.close` to stop defining fields for a record.


```elm
Mark.record "Test"
    (\one two three -> { one = one, two = two, three = three })
    |> Mark.field "one" Mark.string
    |> Mark.field "two" Mark.string
    |> Mark.field "three" Mark.string
    |> Mark.close
```

It shows up because we can't make a field function taht adds a field to any given `Mark.Block`.  If that block was an `int` and you added a field to it, what would that actually mean?

So, we have to restrict being able to add fields to a new type, `Record`, and convert it to a `Block` as the last step.

