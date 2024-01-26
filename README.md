# Untitled

WIP clojure-ish lisp, with an emphasis on flexible metaprogramming DSL construction

## to do
### high priority
* introduction of custom delimiters with `(#delimit <opener> <closer> <tag> ...)`, and custom prefix operators with `(#prefix <prefix> <tag>)` 

  * Wait... I don't think this can be handled by the parser, because it wouldn't work well with macros

    * consider a macro like 

      ```
      (defmacro process-dsl-program [program]
        (#delimit < > alligator ...))
      ```
    
      intended to process a dsl that uses `<` and `>` as a custom delimiter type. If the parser handles the expansion of `<...>` forms then the `program` value passed into this macro couldn't actually use those delimiters, only things literally inside the `(#delimit < > alligator ...)` within the macro definition itself would be affected.
    * So I guess this needs to be delegated to the expander... but then how can the parser be told to add the new delimiters in the middle of the string?

### low priority
* in parser, use `Opening.char_index` to give more descriptive parser errors
