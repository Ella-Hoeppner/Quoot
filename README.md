# Untitled

WIP clojure-ish metaprogramming-focused lisp

## to do
* parser for the core lisp
  * only needs a few types:
      * numbers (done I think!!)
      * strings
        * maybe support string interpolation with like `#(` and `)#`?
          * prob defer this to the parser for the manin language, rather than core
      * symbols
        * includes what would normally be keywords like :example
        * also includes pretty much every character but whitespace or `(` or `)`
    * lists
      * delinated with `(` and `)`
      * won't have any other form types for the core lisp
