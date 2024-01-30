# Quoot
WIP clojure-ish lisp, with an emphasis on flexible metaprogramming and DSL construction

## to do
### high priority
* implement arity-0 range, as a test for laziness
  * e.g should be able to do like (map + (range) (list 5 5 5)) and not infinitely loop
    * will need to change transpose function for this
  * probably need to add another value type like QuootValue::LazyList or smth to make this work
* iterate
* make map lazy
  * as a test, (take 5 (map inc (range))) should work
* When you try to call a list as a function, treat it as an invocation of nth
  * should be able to handle this adding a clause to as_fn for lists
  * will use similar behavior for vectors and hashmaps eventually
* maybe get rid of Sexp in parser, just use a subset of QuootValue?
* ParserState can probably be simplified/cleaned up a bit now that it only needs to handle one expression at a time
  * probably don't need to start the expression_stack with an empty vector?
* use &str in place of &[char] in parser
  * should be doable with char_indices
* add vectors, hashmaps, and sets
  * add constructors fns for the corresponding names to the default environment
  * add `get` to the default environment
* lambdas
* quoting

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
  * The best approach I can think of right now is:
    * Have all delimiters/prefixes be declared as top-level forms.
    * Allow macros to somehow be tagged with which delimiters/prefixes apply to the forms enclosed inside them
      * At the reader stage, when one of these tagged macros is encountered, apply those delimiters/prefixes until that macros closes
        * This will happen at the reader stage so it will be before any kind of macroexpansion, but I think that's fine. Just means that the reader will need to be aware of the declared macros, which seems totally doable.