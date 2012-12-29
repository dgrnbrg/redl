# Redl -- a debug repl

This describes how redl is organized.

# Main api

`make-repl` is how to create a debug repl. It creates a Thread that the debug repl will run on. It returns the id that assigned to that repl thread. Internally, 2 atoms are also maintained--these are where the current level's input and ouput are stored. These atoms allow the interaction with the repl to be late-bound, which is what makes `break` and `continue` possible.

`repl-eval` takes a form and a repl-id, and evaluates the form on the repl. It doesn't know anything about the debug state. By default, evaluation times out after 9.5 seconds, at which point the repl is assumed "lost". Further connections probably won't work on the repl if it's lost.

# User api

`break` causes a new sub-repl to be created when it is called. Rather than returning, it simply hijacks the IO of its parent repl, and allows for further interactive repl-ing. In the sub-repl, however, the locals that were visible to `break` will continue to be visible. `break` returns its argument by default. If no argument is provided, it will return `nil` by default.

`continue` resumes from the most recent `break`-point. If invoked with no argument, the call to `break` will return with whatever its default is. If invoked with an argument, however, the call to `break` will return the argument passed to `continue`.

# Internal structure

`repls` is a map from repl id to its metadata. Information like `*1`, `*2`, `*3`, and `*ns*` is stored locally in the `repl-entry-point` function.

`continue` works by throwing a special exception that allows the stack to be unwound.

`break` just captures the locals using `local-bindings`, then invokes `break*`. When `break*` returns, `break` decides what return value to use.

`break*` creates the sub-repl and watches for the continuation exception.

`repl-entry-point` creates a late-bound-repl-loop using binding conveyence, and returns the IO atoms as well as the bound repl fn. These IO atoms are dynamic vars, which is why the repl must be a bound-fn.

`late-bound-repl-loop` creates the late bound repl loop, and passes the state through.

`eval-with-state-and-locals` handles evaluating the code, but also deals with `*1`, `*2`, `*3`, `*ns*`, `*e`, `*out*`, `*err*`, and exceptions thrown by the evaluated code. The result of each evaluation is returned to the `late-bound-repl-loop`, which is what gives the result map to the out-promise. The final evaluation is delegated to `eval-with-locals`, however.

`eval-with-locals`, `view-locals`, `*locals*`, and `local-bindings` are borrowed from GeorgeJahad's debug-repl.

`*repl-depth*` is used to see how many levels of debugging deep we are, so that the prompt can be formatted to communicate this.
