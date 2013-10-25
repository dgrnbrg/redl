# redl

Read-Eval-Debug-Loop

A better Clojure IDE integration experience!

Redl has been integrated into Vim, in dgrnbrg/vim-foreplay:omnicomplete

## Usage

Note: the namespaces are subject to refactoring, and will become separate in the future.

### Omnicompletion

`completions` finds fuzzy completions of a form in a given namespace. It treats `.`, `$`, and `/`
as separators, and requires an equal number and order of separators to appear in the match target.
The first character in each non-separator region must be the first character in the match;
however, the other characters can appear in any order.

Completions matches Clojure namespaces and aliases, namespace and alias-qualified vars, unqualified
vars, imported Java classes, methods of imported Java classes, qualified Java classes, and static
methods and fields of qualified and imported Java classes.

Metadata is also computed about the completions. Clojure functions and vars include docstrings
and arglists. Java classes include the type signatures of their constructors. Java methods
include all available type signatures.

The matches are scored and sorted, where the score is computed by looking at how many characters
that were not specified occurred between each character in the pattern, penalizing one large
filler group over several small ones, and prefering that the pattern characters appear towards
the front of the match.

Examples of matches:

- `clj./mp` matches `clojure.core/map`
- `./join` matches `clojure.string/join`
- `M/cos` matches `Math/cos`
- `.st` matches `.start`, since `Thread` is included by default.

More examples can be found in the tests.

### Debug Repl

`make-repl` creates a repl and returns its id. `repl-eval` evaluates a form on an existing
repl, and returns the stdout, stderr, return value, and other metadata about the evaluation.

Redl is a debug repl, which means that at any point you can call `break`, in which case
you'll drop into a sub-repl. This sub-repl has the same local scope as the `break` had,
and can be used to inspect and explode in the middle of a function evaluation. When you're
ready to return, invoke `continue`. You can have `break` return its optional invoke if you
invoke `continue` with no arguments, or you can have `break` return something else by invoking
`continue` with an argument. See the tests for more examples, or below for a sample session.

    redl.core=> (+ 1 2)
    3
    redl.core=> (+ 1 (break 3))
    Encountered break, waiting for input...
    redl.core:debug-1=> (continue 8)
    9
    redl.core=> (let [x 1 y 2] (* x y (break 3)))
    Encountered break, waiting for input...
    redl.core:debug-1=> (println x y)
    1 2
    nil
    redl.core:debug-1=> (continue)
    6

## Including in a Leiningen project

Note: these will be easier when I get around to it.

You'll need to clone this repository, then run `lein install` to include it in your
local maven cache. Next, you'll want to add the following keys to your `.lein/profiles.clj`:

- `:injections [(require '[redl core complete])]` ensure that redl is loaded on jvm startup
- `:dependencies [[redl "0.2.0"]]` ensures that redl is available on the classpath

## License

Copyright © 2012 David Greenberg

Distributed under the Eclipse Public License, the same as Clojure.
