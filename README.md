# Quoot
WIP interpreted clojure-looking lisp with a vau-calculus-like evaluation model, with an emphasis on flexible metaprogramming, DSLs, and code generation (for other languages)

## to do
### high priority
* implement list functions take and drop for lazy lists
  * QuootValue::List should contain it's own enum, with a type for LazyList and StrictList
    * as_list should return this, rather than just a List
  * functions to change:
    * take
    * drop 
    * first
    * last
    * rest
    * butlast
    * cons
    * concat
    * list?
  * need to think carefully for each of these about whether or not it should be lazy, potentially conditional on laziness/strictness of the inputs
* implement as_fn for QuootValue::List
  * treat it as a call to nth
* more standard library functions:
  * \>, \<, \>=, \<=
  * drop-last, take-last
  * map (lazy)
  * filter (lazy)
  * repeat (lazy)
  * iterate (lazy)
  * partition (lazy)
  * interleave (lazy)
  * get-back
    * like nth but indexes go backwards from the end of the list
  * some
  * find
    * like some, but the input fn should just return a bool, and it returns the argument that produced the first true from that fn
  * flatten
  * reduce
  * subvec
    * probably rename this
      * sublist?
      * maybe just sub?
      * between?
  * skip
    * basically the opposite of subvec
    * 2 args: list and an index, returns a list without the value at that index
    * 3 args: list, start, end, returns a list skipping the values between start and end
    * should work with lazy lists
  * set
    * like clojure's assoc, just shorter syntax
      * not sure what to rename clojure's "set" to, maybe "hash-set"?
        * a bit annoying to have this name collision, but "map" is already overloaded and doesn't refer to the data structure either so not having "set" be a constructor/caster doesn't seem like a big deal
    * maybe called "with" instead, set sounds side-effectful
  * update
  * and, or, not, xor
    * should do the clj thing where these return the actual value, not the casted truth value
      * eg (or nil 5) should give 5
    * at some point maybe these should be lazy/short-circuitable? probably need to wait for macros for that to be possible tho
  * if
    * will also eventually want a lazy/short-circuitable version of this
  * rest
  * butlast
  * sort
  * sort-by
  * min-by
  * max-by
* should have functions for adding values to the front/back, with both possible argument orders
  * cases:
    * front, (list value) = conj
    * back, (list value) = ?
    * front, (value list) = cons
    * back, (value list) = ?
  * tbh I don't really love the names cons or conj...
  * maybe pushf, pushb, fpush, and bpush, respectively?
* Use &str rather than String for string objects
* str function
  * also substr
  * make get return a char
    * should we have char as it's own type or just treat them as one-character strings?
* maybe get rid of Sexp in parser, just use a subset of QuootValue?
* ParserState can probably be simplified/cleaned up a bit now that it only needs to handle one expression at a time
  * probably don't need to start the expression_stack with an empty vector?
* use &str in place of &[char] in parser
  * should be doable with char_indices
* fork imbl
  * impl Hash on Hashmap
* QuootValue::Hashmap
  * hashmap constructor fn
  * implement get, set cases
  * merge
  * maybe make skip work on hashmaps too?
    * could take an arbitrary number of args for this case
  * QuootValue::Hashmap.as_fn
* add hashmaps and sets
  * add constructors fns for the corresponding names to the default environment
  * add `get` to the default environment
  * map? or hashmap? function
* lambdas
* once lambdas are implemented, translate my cljs kd-tree implementation to Quoot
  * run benchmarks for cljs, clj, and Quoot
* let forms
* quoting
* unquoting
  * just inside quotes for now, don't worry about top-level unquoting yet
* eval
  * this will involve implementing the ability to spin up a new interpreter arbitrarily, which will also be useful later for top-level unquoting
* top-level unquoting

### low priority
* Use rustyline crate for a nicer repl
* in parser, use character indexes to give more descriptive parser errors
* ordered hashmaps
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
* think about how transducers might fit in
