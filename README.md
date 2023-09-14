## step 1- interpreter

### getting started with the repl

* Get `clj` (clojure, language) and `bb` (babashka, script runner/cli runtime) installed: [bb](https://babashka.org/) [clj](https://clojure.org/guides/install_clojure)
* run `bb nrepl` to start up a repl
* start an editor and "jack-in" or connect through port 5555 (noted in `.nrepl-port` for automatic configuration)

### notes on clojure interpreter implementation
* to start contributing seriously in clojure: I recommend getting comfortable with conjure.nvim, emacs, or vscode's Calva extension
* note: we are using clojure for initial prototyping speed, but we should be able to reuse parts of the parser/lexer and rewrite the backend in a different language if preferred. we can also rewrite the parser/lexer if desired. i just want to get something working first

