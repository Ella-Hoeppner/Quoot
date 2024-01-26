# Quoot
WIP clojure-ish lisp, with an emphasis on flexible metaprogramming and DSL construction

## to do
### high priority
* Make ParserState keep track of a stack of Sexps, avoid recursive sexp_insert
* Convert raw Sexps into more detailed ASTs
  * Have a Qexp struct with a from_sexp method
    * Should distinguish numbers, strings, and symbols
* start work on the runtime
  * eval should take an expression and an environment, return an expression and environment chunk
    * environments should be hashmaps between symbols (Strings) and values
      * environment chunks will be merged into the global environment for future evaluations
* tests for parser
* introduction of custom delimiters with `(#delimit <opener> <closer> <tag> ...)`, and custom prefix operators with `(#prefix <prefix> <tag> ...)` 
  * Wait... I don't think this can be handled by the parser, because it wouldn't work well with macros
    * consider a macro like 

      ```
      (defmacro process-dsl-program [program]
        (#delimit < > alligator ...))
      ```
    
      intended to process a dsl that uses `<` and `>` as a custom delimiter type. If the parser handles the expansion of `<...>` forms then the `program` value passed into this macro couldn't actually use those delimiters, only things literally inside the `(#delimit < > alligator ...)` within the macro definition itself would be affected.
    * So I guess this needs to be delegated to the expander... but then how can the parser be told to add the new delimiters in the middle of the string?

### low priority
* in parser, use character indexes to give more descriptive parser errors
