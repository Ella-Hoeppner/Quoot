# Quoot
WIP clojure-ish lisp, with an emphasis on flexible metaprogramming and DSL construction

## to do
### high priority
* make parser stop after reading one full form, return just that one form rather than a list of all parsed forms
  * should also probably return the index stopped at
* add value lookups from environment to the interpreter
  * this isn't working yet, need to figure out how to pull values from the hashmap without cloning the entire thing every time
    * apparently it's something with Rc and RefCell?
* Use hashbrown crate to have much faster hashmaps
* consider using &str in place of &[char] in parser?
* add function application to the interpreter
  * need to have a function type in QuootValue
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

### low priority
* in parser, use character indexes to give more descriptive parser errors
