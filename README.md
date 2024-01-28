# Quoot
WIP clojure-ish lisp, with an emphasis on flexible metaprogramming and DSL construction

## to do
### high priority
* ParserState can probably be simplified/cleaned up a bit now that it only needs to handle one expression at a time
  * probably don't need to start the expression_stack with an empty vector?
* use &str in place of &[char] in parser
  * should be doable with char_indices
* make function application use the rest of the values in the list as args
* create a default environment
  * for now can just contain arithmetic stuff
    * +, -, *, /, mod, quot
* Make lists use rpds persistent list rather than Vec
* add vectors, hashmaps, and sets
  * pull in the rpds crate for this
  * add constructors fns for the corresponding names to the default environment
  * add `nth` and `get` to the default environment

### low priority
* Use rustyline crate for a nicer repl
* in parser, use character indexes to give more descriptive parser errors
* add ordered hashmaps
  * I guess these might just have to consist of a hashmap and a list/vector?
* figure out how to handle custom delimiter/prefix introduction
  * If it happens at the parser stage it wouldn't very be ergonomic because it couldn't be included in macros
   * consider a macro like 

      ```
      (defmacro process-dsl-program [program]
        (#delimit < > alligator ...))
      ```
    
      intended to process a dsl that uses `<` and `>` as a custom delimiter type. If the parser handles the expansion of `<...>` forms then the `program` value passed into this macro couldn't actually use those delimiters, only things literally inside the `(#delimit < > alligator ...)` within the macro definition itself would be affected.
  * But it can't really be delegated to the macroexpansion stage, because we can't even construct the AST to feed into the macros without first knowing what the delimiters are...
  * Probably need some special syntax to make this work
    * What if you always have to declare the existence of a delimiter/prefix on it's own top-level form, then prefix other top-level(?) forms that use that use that delimiter/prefix with some indicator (specific to that edlimiter/prefix)
      * As long as the parser stopped between each top-level form, and the Interpreter kept track of the globally declared delimiters and has a step to update that list between each top-level form, I think this could work