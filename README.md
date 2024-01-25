# Untitled

WIP clojure-ish metaprogramming-focused lisp

## to do
### high priority
* fix bug where delimiters with the same closer cause MismatchedCloser error
* introduction of custom delimiters with `(#delimit <opener> <closer> <tag> ...)`
* introduction of custom prefix operators with `(#prefix <prefix> <tag>)` 

### low priority
* in parser, use `Opening.char_index` to give more descriptive reader errors
