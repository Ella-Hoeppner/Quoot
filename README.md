# Quoot (working title)
Early WIP interpreted lisp with a focus on flexible metaprogramming, embedded DSLs, and code generation for other programming languages.

## Motivation

The main intention of Quoot is to be a metaprogramming language - a language that can be used to metaprogram embedded DSLs and generate code for other programming languages.

Many languages come with their own special-purpose metaprogramming system, but often these systems are quite disconnected from the rest of the language, and thus have a significant learning curve of their own on top of that of the core language. However, these various metaprogramming systems are usually trying to accomplish roughly the same thing - letting the user manipulate the AST of the core language. But why should every programming language have it's own system for accomplishing this same goal? Why not have a single common language that sits on top of other languages as a metaprogramming layer? That's the niche that Quoot aims to fill.

The main way this will be accomplished is with lisp-syntax DSLs embedded in Quoot that are in close correspondence to the syntax of other languages. For an example of what this might look like in practice, see my ClojureScript DSL [kudzu](https://github.com/Ella-Hoeppner/kudzu) that compiles to GLSL, the shader language used in WebGL. As an example, here is a kudzu program and the corresponding compiled GLSL code:

```
'{:precision {float highp}
  :outputs {frag-color vec4}
  :uniforms {resolution vec2}
  :main
  ((= frag-color
      (-> gl_FragCoord.xy
          (/ resolution)
          (vec4 0 1))))}
```

```
#version 300 es
precision highp float;
uniform vec2 resolution;
out vec4 frag_color;
void main()
{
  frag_color = vec4((gl_FragCoord.xy / resolution), 0., 1.);
}
```

This example is simple enough to write in either language, but there are many kinds of programs that are easy to write in kudzu that would be very difficult to write in raw glsl. For instance:

```
'{:precision {float highp}
  :outputs {frag-color vec4}
  :uniforms {resolution vec2}
  :main
  ((=float closest-distance 100)
    (=vec2 pixel-position (/ gl_FragCoord.xy resolution))
    ~@(map (fn [position]
            '(= closest-distance
                (min closest-distance
                      (distance pixel-position (vec2 ~@position)))))
           (repeatedly 500 (fn [] [(rand) (rand)])))
    (= frag-color
      (vec4 (vec3 (/ (+ 1 (* 10 closest-distance))))
            1)))}
```

This program renders a voronoi diagram of 500 random points, which are generated by the ClojureScript expression `(repeatedly 500 (fn [] [(rand) (rand)]))` and inlined as the AST is constructed. Quoting and unquoting makes it trivial to seamlessly integrate ClojureScript code and kudzu code. The resulting GLSL code (abridged, as it's over 500 lines) looks like this:

```
#version 300 es
precision highp float;
precision highp usampler2D;
precision highp int;
uniform vec2 resolution;
out vec4 frag_color;
void main()
{
  float closest_distance = 100.;
  vec2 pixel_position = (gl_FragCoord.xy / resolution);
  closest_distance = min(closest_distance, distance(pixel_position, vec2(<inlined random floats>)));
  closest_distance = min(closest_distance, distance(pixel_position, vec2(<inlined random floats>)));
  ...
  frag_color = vec4(vec3((1./(1. + (10. * closest_distance)))), 1.);
}
```

Of course, it would also be possible to generate a program like this with some other language that's hosting the GLSL app, like javascript, using string interpolation and/or concatenation. However, the corresponding program would be more complicated, less elegant, harder to read and maintain, and more prone to mistakes (e.g. a missing parentheses in the generated glsl, which can never happen when using kudzu). Working directly with ASTs can make it easy to build sophisticated metaprogramming systems that would be impossible to build in the core language, and infeasible to build with direct string manipulation in other languages. Quoot intends to leverage the power of lisp-style metaprogramming for languages that are indepdent of the its own runtime.

Quoot is still in early development and is missing many core features, but as soon as it's mature enough, there are several DSLs I plan to make:
  * A DSL for WGSL, the shader language for WebGPU, so that Quoot can be used to build cross-platform high-performance graphics programs, acting as a metaprogramming layer that allows for powerful new abstractions that are unavailable in normal shader code.
  * A DSL for javascript (similar to my ClojureScript project [jast](https://github.com/Ella-Hoeppner/jast)), allowing Quoot to be used as a layer for building powerful abstractions in webapp front-ends, while maintaining a small footprint, as there will be no library that needs to be shipped with the generated javascript code.
  * A DSL for LLVM IR. This will allow Quoot to be used to generate optimally performant cross-platform assembly code. This DSL may be useful directly for programming some sufficiently simple algorithms, but mainly will exist to provide a convenient compilation target for other, higher-level Quoot DSLs.
  * Potentially, a DSL for some higher-level systems language like C, Zig, or Rust.

In addition to these goals, Quoot also aims to be a fairly performant general-purpose programming language with a relatively light-weight runtime. Needless to say, Quoot will be as good for metaprogramming itself as it will be for metaprogramming other languages, and aims to be as ergonomic and easy to use as the best Lisps available today, so there shouldn't be any issue with using Quoot for building sophisticated software, rather than being used purely for metaprogramming embedded DSLs. Quoot is an interpreted language and will itself never be able to match the performance of high-performance systesm languages like C, and likely will never even be competitive with highly optimized compiled lisps like Common Lisp. However, Quoot can be performant enough to a *part* of a piece of a high-performance software, acting as a light-weight, easy to use scripting layer that connects highly optimized special-purpose sub-programs written with Quoot DSLs.

## Feature goals:
* Clojure-like syntax and ergonomics. This includes special delimiters for data structures like persistent vectors, sets, and hashmaps, and the ability to invoke these data structures as functions. Quoot also follows clojure in that functions mostly have names that are real words rather than Lisp jargon from the 70s, e.g. `first` and `rest` rather than `car` and `cdr`.
  * However, Quoot's built-in data structures and syntax will diverge slightly from Clojure's.
    * While Clojure has two sequential data types, lists and vectors, Quoot simply has one, which are referred to as "lists" for simplicty but are implemented as [RRB Vectors](https://dl.acm.org/doi/10.1145/2858949.2784739), using the [imbl crate](https://github.com/jneem/imbl). These support very fast lookups, modification, splitting, concatenation, and pushing and popping on both the front and the back, and therefore support the best features of Clojure's lists and vector simultaneously. Quoot also maintains support for lazy versions of these lists, whereas Clojure has no lazy vectors, only lazy lists. Since there is no separate vector type that would need its own syntax, Quoot uses the `[...]` delimiters as a syntax for list literals.
    * Additionally, Quoot will include for ordered hashmaps, using `#[...]` delimiters.
* An evaluation model inspired by [the vau calculus and the Kernel programming language](https://web.cs.wpi.edu/~jshutt/kernel.html), involving `operator`s that blurs the line between macros and functions.
  * In addition to `(fn [...] ...)` which defines a conventional lambda, `(operator [...] ...)` defines an operator that takes in it's arguments unevaluated, allowing for arbitrary processing of the internal AST (this is sometimes called an "fexpr" in older Lisps). `operator`s can always call `eval` on their arguments, and `fn` can be thought of as the special case of `operator` that always calls `eval` on all of it's arguments. Unlike macros, Quoots `operator`s are first-class objects that can be interacted with at runtime.
  * Among other consequences, this means that operators like `or` can seamlessly be used as arguments to higher-order functions, which isn't the case in most other Lisps where they're implemented as macros. If the first argument to `or` evaluates to `true`, then it doesn't matter what the rest of the arguments evaluate to, so their evaluation can be skipped to save performance. But it isn't possible to express this behavior inside a lambda, and so most lisps implement `or` as a macro instead. However, that means that code like `(map or '(true false) '(true true))` doesn't work, as `or` isn't a function and therefore can't be passed to `map`. But in quoot, `operator`s can decide for themselves whether or not to evaluate their arguments, so `or` can avoid unnecessarily evaluating it's arguments while still acting like as a first-class object that has no trouble being composed with higher-order functions. `(map or '(true false) '(true true))` will run fine in Quoot, but not in Scheme, Common Lisp, or Clojure (at least, not without creating a lambda that wraps `or`, which is ugly and inelegant).
  * This does mean that syntactic abstractions defined with `operator` have an associated run-time cost, rather than being fully expanded at compile-time like traditional macros. However, the semantics of traditional macros are, just like lambdas, a special case of the semantics of operators. Therefore, Quoot will also include a `macro` special form that encapsulates this special case and can be expanded in a pre-processing stage for cases where maximal performance is important.
  * While Quoot's evaluation model is inspired by the vau calculus, it differs in important ways. The primary practical difference is that Quoot does not have an equivalent Kernel's `wrap` or `unwrap` operatives. Also, Quoot encourages the use of quoting and unquoting operators, whereas Kernel does not implement them, and strongly discourages their use.
* Powerful, ergonomic, and concise metaprogramming via a top-level unquote operator.
  * Using an unquote at the top-level (i.e. not inside a quoted form), which would simply cause an error in most Lisps, instead indicates that the unquoted form should be evaluated in a pre-processing step before the rest of the code is evaluated. This can accomplish things similar to traditional macros, but can be much more concise while still being very readable (arguably moreso than traditional macros in many cases). This can often allow you to eliminate local repeated code in a very concise way, which otherwise would require a one-off macro that looks clunky and crowds up the namespace.
  * Consider a clojure expression like:
    ```
    (let [a (map (fn [x y]
                    (let [z (* x 3)]
                      (str z y)))
                  (range 4 7)
                  ["a" "b" "c"])
          b (map (fn [x y]
                    (let [z (* x 3)]
                      [y z]))
                  (range 13 18)
                  [2 -3 4 10 1])
          c (map (fn [x y]
                    (let [z (* x 3)]
                      (Math/pow y z)))
                  (range 100)
                  (repeatedly rand))]
      ...)
    ```
    There is quite a bit of repetition in the definition of these bindings: all three call `map` with three arguments, where the second argument is a call to `range`, though with varying arguments, and the function passed to `map` creates a local binding `z` equal to the first argument times 3, then returns a value constructed from `z` and the second argument. It would be nice to be able to express this program in a way that abstracts away this repetition. One way to do that would be by defining a new function that accepts all the differing bits between the different bindings as arguments, and define `a` `b` and `c` by mapping with that function:
    ```
    (let [[a b c] (map (fn [inner-fn range-args y-values]
                          (map (fn [x y]
                                  (let [z (* x 3)]
                                      (inner-fn z y)))
                                (apply range range-args)
                                y-values))
                        [[str [4 7] ["a" "b" "c"]]
                        [#(vector %2 %1) [13 18] [2 -3 4 10 1]]
                        [#(Math/pow %2 %1) [100] (repeatedly rand)]])]
      ...)
    ```
    This avoids the repetition, but not without a cost: this code will perform slightly slower than the original, as it involves an extra invocation of `map` as well as the creation of several additional data structures and lambdas. This may be a negligble cost in this example, but for a more complicated example it may not be, and regardless it's unfortunate that avoiding repetition and expressing the shared logic of different parts of our code abstractly comes with a performance cost.

    Another way to avoid the repetition would be with a macro:
    ```
    (defmacro my-macro [inner-expression range-args y-values]
      `(map (fn [x y]
                (let [z (* x 3)]
                  ~inner-expression))
          (apply range ~@range-args)
          ~y-values))
    (let [a (my-macro (str z y) [4 7] ["a" "b" "c"])
          b (my-macro (vector y z) [13 18] [2 -3 4 10 1])
          c (my-macro (Math/pow y z) [100] (repeatedly rand))]
                  
      ...)
    ```
    This solution avoids the runtime performance cost of the prior approach because this code turns into the original code during macroexpansion, but it seems clunky to define an entire macro for something so specific that will never be used in another part of the code base. Additionally, it makes the code harder to read because the macro is defined separately from where it's being used, and the expressions in the call to the macro involve the symbol `z`, which isn't bound where the macro is called and doesn't have any clear meaning without looking at the details of the macro definition. Further, It also gives us a naming problem that we didn't have with the previous map-based solution - what should this macro be called? Here I've called it `my-macro`, but that wouldn't be a good name to include in a real codebase. But it's very difficult to come up with a reasonable name that accurately describes what role this macro actually serves, because it's purpose is so specific to this one circumstance.

    However, there is a way to avoid the runtime hit and the clunkiness that comes with defining a macro. Imagine that this entire form existed inside a quasiquote, say as part of some broader enclosing macro. This would allow us to use unquotes, which would open up a new way of writing something that expands at compile time to the original code:
    ```
    (let [~@(interleave [a b c] 
                        (map (fn [inner-expression range-args y-values]
                              `(map (fn [x y]
                                      (let [z (* x 3)]
                                          ~inner-expression))
                                    (apply range ~@range-args)
                                    ~y-values))
                            [[(str z y) [4 7] ["a" "b" "c"]]
                             [(vector y z) [13 18] [2 -3 4 10 1]]
                             [(Math/pow y z) [100] (repeatedly rand)]]))]
      ...)
    ```
    This looks very similar to the first solution using a runtime call to `map`, but here the call to `map` evaluates at macroexpansion phase, and the resulting code at runtime is identical to what we started with, just like with the macro-based solution. And yet this approach avoids most of the issues of the macro solution, as the metaprogramming logic is expressed right where it's being used, there's no need to pick a name for a macro, and the use of the unbound `z` wouldn't be as confusing to someone looking at this code for the first time because it occurs inside an unquoted form, making it clear that some kind of metaprogramming shenanagins are going on.
    
    Unfortunately, in Clojure and other Lisps, this expression would only be valid if the entire let block was itself enclosed in a greater quasi-quoted form, otherwise the unquote operator is undefined. So this kind of elegant metaprogramming solution wouldn't be available most of the time. But why does it need to be that way? The intended semantics of what's going on in the above code block are perfectly clear regardless of whether or not it ocurrs inside a broader quoted form: the unquoted code should be evaluated at a stage before, and then inlined into, the surrounding code. When used outside a quoted form, this simply means that it should happen before the interpreter even starts running the rest of the code, as with a macro invocation.

    Quoot simply reifies this straightforward extension of the unquote semantics, and lets you use unquotes at the top level as a way to signify that the code should be run a stage before normal evaluation. In fact, you can even use multiple nested unquotes at the top-level, to have multiple stages of pre-processing before the normal code is evaluated.
* Radically extensible syntax via custom delimiters and prefixes.
  * By default quoot will have the delimiters `(...)` for lists, `[...]` for literal lists (i.e. lists that don't get treated as a function application), `{...}` for hashmaps, `#[...]` for ordered hashmaps, and `#{...}` for sets. However, you can also introduce arbitrary new delimiters in your own code, using the `(#delimiter opening closing tag)` special form.
  * For instance, `<` and `>` are normally treated as regular characters that can be used as part of symbols. But if you were writing a DSL and wanted a delimiter that represented some special kind of form or data structure, you could declare `(#delimiter < > alligators)`. From then on, the parser would treat `<` and `>` as the opening and closing delimiters for a new type of form. Forms using these delimiters will be parsed as lists with the provided tag as their first element, e.g. a string like `<are scary!>` would be parsed as a list `(alligators are scary!)`.
  * Similarly, `(#prefix prefix-marker tag)` can be used to introduce new unary prefix operators, like the quote and unquote operators `'` and `~`, such that forms marked with these prefixes expand to tagged lists. For instance, if you used `(#prefix @ dereference)`, then the string `@my-atom` will be parsed as `(dereference my-atom)`.
  * Both custom delimiters and custom prefixes can be limited to only apply in certain scopes, rather than to the entire proceeding file. For instance, if you have an embedded DSL and some function or operator `process-my-dsl` for processing quoted programs in that DSL, you will be able to denote that the custom delimiters and prefixes for that language only apply inside `(process-my-dsl ...)` forms, and elsewhere the parsing will be unaffected.
  * Unfortunately, this capability might make static analaysis very difficult or impossible.
* Integrable with other Rust libraries. It should be easy to make Quoot bindings for arbitrary Rust structs. This will provide a path for creating tools that allows Quoot to be more closely integrated with the runtimes of the foreign-language programs that it constructs.

## To Do:
### high priority
* port cljs kd-tree implementation to quoot for a performance test
* Use &str rather than String for string objects
  * actually might I run into borrowing/ownership problems if I try to do this?
    * not really sure, probably worth trying
* string functions
  * mainly str and substr
  * make get return a char for strings
    * should we have char as it's own type or just treat them as one-character strings?
      * probably as it's own type
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
  * QuootValue::Hashmap.as_op
  * map? function
  * as_op case
* add a case to as_op for symbols that treats them like accessors in hashmaps, similar to clojure's keywords, e.g. `('hello {'hello 1})` would work like clojure's `(:hello {:hello 1})`
* fork imbl
  * impl Hash on Hashmap
* `local-env`, `namespace-env`, `update-namespace-env`, and `with-env` operators
  * `local-env` no args, returns hashmap of the local environment
  * `namespace-env` no args, returns the top-level environment of the namespace
  * `update-namespace-env` one argument, a fn that takes the current namespace environment and returns a new one
  * `with-env` accepts a hashmap and a body and makes everything in the hashmap available as bindings in the body
* 2-argument case for `eval`, taking a hashmap as an environment
* make `operator` have a special extra argument that's bound to the local environment so that it can call `eval` with local bindings from where it's invoked
  * or wait, could an `operator` that wanted this behavior not just call `local-env`?
* unquoting
  * just inside quotes, for now
* think about whether there's a way to make a spread/unroll operator work
  * it's kinda like a macro but that applies to the parent form of where it's called... is there a way to fit this into the evaluation model?
* figure out how to handle custom delimiter/prefix introduction
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
    * maybe even try to do a #[derive] thingy?
* anonymous function shorthand syntax
  * want to use something other than clojure's `#(...)` to denote this, so that it can be used with data structures other than lists. E.g. in closure it would be nice if you could do `#[% 2]` to be the equivalent of `#(vector % 2)`. But this would break for hasmaps because `#{...}` overlaps with the set literal syntax. Would be nice to have another symbol that avoids that ambiguity so it can be composed with all the data structure literals.
    * Maybe just use `%(...)`? Since they can't be nested anyways this shouldn't cause any problems.
* destructuring
  * no idea how to approach this :O
* atoms, I guess? Kinda don't want to but I guess there should be at least some way of having mutable state :(
* loop/recur
  * maybe also trampoline? don't have a good idea of what trampolining allows for that couldn't also be handled by loop/recur
    * I guess it's better for mutual recursion or using existing functions
* maybe a special case of map for when all the arguments are strict
* more standard library functions:
  * interleave (lazy iff args are lazy)
  * map (lazy iff any arg is lazy)
  * partition (lazy iff args are lazy)
  * drop-last, take-last
  * get-back
    * like nth but indexes go backwards from the end of the list
  * some
  * find
    * like some, but the input fn should just return a bool, and it returns the argument that produced the first true from that fn
  * flatten
  * reduce
  * sublist
    * like clojure's subvec, but only needs the 2-arg case because otherwise it's just drop
    * lazy iff input is lazy
  * skip
    * takes >=1 args, first must be a list and rest are indeces to skip
    * lazy iff list is lazy
  * insert
    * 3 args: list, index, and value
  * skip-range
    * 3 args, basically the opposite of sublist
    * lazy iff list is lazy
  * set
    * like clojure's assoc, just shorter syntax
      * not sure what to rename clojure's "set" to, maybe "hash-set"?
        * a bit annoying to have this name collision, but "map" is already overloaded and doesn't refer to the data structure either so not having "set" be a constructor/caster doesn't seem like a big deal
    * maybe called "with" instead, set sounds side-effectful
    * I guess this should work with lazy lists?
      * this seems silly but also pretty cool maybe?
  * set-in, update-in
  * butlast
    * should work with lazy lists
  * sort
  * sort-by
  * min-by
    * takes a fn and a list, returns the item in list for which the fn returns the lowest value
      * if a third argument is supplied and is truthy, this will return a pair of the minimum number and the corresponding value in the list that produced the number
  * max-by
  * dotimes, doseq
  * reset! swap!
    * not sure if I want the exclamation marks...
      * it's nice to have stateful functions marked with a special character like that, but it would also be nice to leave `!` unused so that it can be a prefix for DSLs...
  * map-vals
    * takes a function and a hashmap, updates each value in the hashmap with the function, leaving the keys untouched
      * basically this:
        ```
        (defn map-vals [f m]
          (into {}
          (map (fn [[k v]]
                 [k (f v)])
               m)))
        ```
      * feels like this comes up pretty frequently in clj, enough that there should be this built-in convenience function
    * maybe also map-keys
  
* should have functions for adding values to the front/back, with both possible argument orders
  * cases:
    * front, (list value) = conj
    * back, (list value) = ?
    * front, (value list) = cons
    * back, (value list) = ?
  * tbh I don't really love the names cons or conj...
  * maybe pushf, pushb, fpush, and bpush, respectively?

### low priority
* QuootValue::Hashset
  * set constructor fn
  * implement get, set, count cases
  * functions:
    * set?
    * union
    * intersection
    * difference
  * make skip work with it, like with hashmaps
  * QuootValue::Hashmap.as_op
  * map? function
  * as_op case
* try to figure out how to avoid stack-overflows
  * the function `(fn f [x] (if (> x 0) (f (dec x)) x))` causes a stack overflow once the argument reaches about 1600, which isn't very deep, should be able to go much deeper than this
  * for comparison, the same code in clojure (babashka, at least) can get to a depth of about 34800 before crashing, and the javascript function `function f (x) { if (x>0) {return f(x-1)} else {return x} }` can get to as deep as 9800
  * can just use stacker to extend the stack, but that won't work on wasm...
    * would be better to just figure out how to take up less stack space
    * I guess the evaluation model of having the evals interleaved is making this issue worse...
* think about how to introduce (delimited?) continuations
* optimization:
  * `QuootList::as_strict` clones it's argument even when it's a strict list, seems inefficient
    * maybe it should take ownership of the argument? Or return a reference?
    * same with `QuootValue::as_num`, `as_list`, `as_op`
  * I feel like `QuootList::get` and `QuootLazyList::get` could probably just return a reference, as `QuootStrictList` naturally does
  * Can probably use the `QuootStrictList::from(vec![...])` constructor rather than repeated `push_front` or `push_back` calls in several places
  * in quoot_let, I don't think we need to create list_clone, can just call `.get` on the original reference rather than a bunch of `.pop_front`s
  * wherever I'm creating a vector iteratively with `.push_front` or `.push_back`, might be able to just use `Vector::from(vec![])`
  * realize_to is always implemented as repeated calls to realize, but many of the lazy realizer functions could easily be modified to support mutli-element realization in a single call, which would save on overhead
    * all lazy realizers could just accept another argument of the number of elements to realize, and the current realizers that can't easily be modified to support that could just iterate their current approach
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
