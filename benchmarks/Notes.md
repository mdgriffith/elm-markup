# General Results from Benchmarking


There's some issue with running Benchmarks normally, possibly due to the operation taking ~160ms to run.

So, I put together a LowLevel benchmark to get a general idea of how the rewrite went.  Looks pretty nice!


- `166.4ms` `elm-markup 2.0` - parsing and rendering a decent sized blogpost
- `81ms` `elm-markup rc-3.0` - parsing to an intermediate AST and rendering a decent sized blogpost.
- `0.47ms` `elm-markup rc-3.0` - Document is already parsed, just the conversion from AST -> Result

```
2.0: String -> Result for 100 iterations at 155.08ms/call
3.0: String -> Result for 100 iterations at 76.33ms/call
3.0: AST -> Result for 100 iterations at 0.43ms/call
```