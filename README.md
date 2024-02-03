# Quoot
WIP interpreted clojure-looking lisp with a vau-calculus-like evaluation model, with an emphasis on flexible metaprogramming, DSLs, and code generation (for other languages)

## to do
### high priority
* change lazy lists to have an internal mutable "state" vector 
  * this is necessary for partial realization of things like filter, where if realization is interrupted in the middle of the original list then we need to know where to pick up from next time
    * implement filter as a test for this
* implement list functions handling of lazy lists
  * functions to change:
    * range, take, drop, rest, and concat, and cons should return lazy lists
    * cons should be able to accept a lazy as the second arg
    * last shouldn't clone when calling fully_realize
* def
  * not sure how to handle global scope in environments...
    * could have `global_bindings` and `local_bindings` as fields in Env rather than just having `bindings`
      * but `global_bindings` would need to be mutable...
    * maybe if `def` can only be used at the top level it would make things easier, but that feels like it might close off some interesting metaprogramming possibilities...
* eval function
* quoting
  * should basically just be an identity vau (i.e. doesn't eval it's arguments)
    * at least until we need to do unquoting
* vau
  * maybe call this `operator` instead to avoid jargon?
* lambdas
  * just a vau but it automatically evals it's arguments so it works like a lamba in a normal lisp
  * once this is done, translate my cljs kd-tree implementation to Quoot
    * run benchmarks for cljs, clj, and Quoot
* think about whether there's a way to make a spread/unroll operator work
  * it's kinda like a macro but that applies to the parent form of where it's called... is there a way to fit this into the evaluation model?
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
    * should always return a lazy list
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
* string functions
  * mainly str and substr
  * make get return a char for strings
    * should we have char as it's own type or just treat them as one-character strings?
      * probably as it's own type
* fork imbl
  * impl Hash on Hashmap
* QuootValue::Hashmap
  * hashmap constructor fn
  * implement get, set, count cases
  * functions:
    * map?
    * merge
    * zipmap
    * keys
    * vals
  * make skip work with it, should take an arbitrary number of keys to remove as args
  * QuootValue::Hashmap.as_fn
  * map? function
  * as_fn case
* add a case to as_fn for symbols that treats them like accessors in hashmaps, similar to clojure's keywords, e.g. `('hello {'hello 1})` would work like clojure's `(:hello {:hello 1})`
* QuootValue::Hashset
  * set constructor fn
  * implement get, set, count cases
  * functions:
    * set?
    * union
    * intersection
    * difference
  * make skip work with it, like with hashmaps
  * QuootValue::Hashmap.as_fn
  * map? function
  * as_fn case
* unquoting
  * just inside quotes, for now
* figure out how to handle custom delimiter/prefix introduction
  * If it happens at the parser stage it wouldn't very be ergonomic because it couldn't be included in macros
    * consider a macro like 

      ```
      (defmacro alligator-dsl [program]
        (#delimit < > alligators ...))
      ```
    
      intended to process a dsl that uses `<` and `>` as a custom delimiter type. If the parser handles the expansion of `<...>` forms then the `program` value passed into this macro couldn't actually use those delimiters, only things literally inside the `(#delimit < > alligators ...)` within the macro definition itself would be affected.
  * Probably need some special syntax to make this work
    * plan right now (very subject to change if I come up with something better):
      * delimiters and prefixes are declared at the top-level, with special syntax along the lines of:
        * `(#delimiter < > alligators)`
        * `(#prefix @ dereference)`
      * these don't affect the parsing of the normal code in rest of the file by default (though there could be an optional extra argument to make them do that)
        * instead, you can tag certain symbols or prefixes, using a top-level `(#syntax name ...)` form, as being markers of a scope where they should apply
      * examples:
        ```
        (#delimiter < > alligators)
        (def alligator-dsl
             (macro [program]
               ...))
        (#syntax alligator-dsl alligators)
        (alligator-dsl '<are scary!>)
        ```
          * in the call to `alligator-dsl`, `program` would take on a value of `(alligators are scary!)`
        ```
        (#prefix @ quote)
        (#delimiter < > alligators)
        (#syntax @ alligators)
        @<are scary!>
        ```
          * here the last expression would parse to `(quote (alligators are scary!))`, and this would evaluate to `(alligators are scary!)`. `@` simply acts as a quote operator within which alligator brackets can be used
      * maybe could allow delimiters to be directly declared inside `#syntax` for brevity, and potentially have `#syntax` forms just result in their first argument (the name of the symbol being tagged) when processed by the reader so they can be inlined into other forms
        * in that case the first example above could be rewritten as:
        
          ```
          (def alligator-dsl
               (macro [program]
                 ...))
          (#syntax alligator-dsl (#delimiter < > alligators))
          (alligator-dsl '<are scary!>)
          ```

          or even as

          ```
          (def (#syntax alligator-dsl (#delimiter < > alligators))
               (macro [program]
                 ...))
          (alligator-dsl '<are scary!>)
          ```
* top-level unquoting
* QuootValue::Foreign
  * represents foreign rust objects
    * basically should just be able to call functions on it with syntax like `(foreign-object.method)` or `(.method foreign-object)`
      * I think we can do without `.-` accessors like clojure has, can just rely on getter/setter methods for everything where that would be needed
  * ideally should do this in a way that makes it very easy to straightforwardly wrap rust structs with bindings to make them accessible from Quoot

### low priority
* what about allowing for `.` and `|` to be used as infix operators, within symbol names?
  * so like `+.-` would be equivalent to `(comp + -)`, and `+|1` would be `(partial + 1)`
  * I don't think we'll be using `|` for anything else so I guess that would be fine, takes it out of the pool of usable characters for symbol names tho...
    * `.` will be also used for foreign object. These cases could be distinguished based on the type of the thing to the left of the `.`, but that means the expansion couldn't be done at reader-time...
      * maybe use `:` for foreign functions instead?
  * could also include `!` as a shorthand for apply, then allow `!f` as a shorthand for `(apply f)`
  * `even?` = `zero?.!mod.reverse.list|2`
    * or maybe event `even?` = `zero?.#(mod % 2)` if we allow it to apply to non-whitespaced delimiters...
  * if this is all done as reader-time expansions it would start to limit a lot of symbols from being used in DSLs tho...
    * what about just having a generic way to add infix operators like there will be for delimiters and prefixes, so that if the user wants this kind of thing they can easily declare it at the top of their file?
  * would also be nice to have some syntax for reversing the arguments to a function
    * maybe a preceding `?`
      * I guess this could be implemented with a prefix+macro
* maybe get rid of Sexp in parser, just use a subset of QuootValue?
* ParserState can probably be simplified/cleaned up a bit now that it only needs to handle one expression at a time
  * probably don't need to start the expression_stack with an empty vector?
* use &str in place of &[char] in parser
  * should be doable with char_indices
* in parser, use character indexes to give more descriptive parser errors
* ordered hashmaps
  * I guess these might just have to consist internally of a hashmap and a list/vector?
* think about how transducers might fit in
* think about concurrency
