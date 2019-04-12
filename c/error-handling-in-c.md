# Error handling

Questions

What will the receiver do with the error?
How much detail does the receiver need to know about why the error happened?

Options

1. Use naturally invalid answers as flags to indicate different kinds of errors e.g. returning negative numbers from some functions
1. Return an ad-hoc struct from the function which contains an `error` field of some form (Go uses this pattern)
