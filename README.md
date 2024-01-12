# Untitled

WIP clojure-ish metaprogramming-focused lisp

## to do
* parser for the core lisp
  * only needs a few types:
    * numbers (done!)
    * strings (done!)
    * symbols
      * includes what would normally be keywords like :example
      * also includes pretty much every character but whitespace or `(` or `)`
    * lists
      * delinated with `(` and `)`
      * won't have any other form types for the core lisp
