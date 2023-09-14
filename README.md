## step 1- interpreter

### getting started with the repl

* Install:
    * required: [clojure](https://clojure.org/guides/install_clojure) (language)
    * optional: [bb](https://babashka.org/) (script runner/Graavl VM clojure runtime)
* to start repl using bb:
    * run `bb nrepl`
* to start repl using clj:
    * run `clj -M:repl --port 5555`
* once you have cider/nrepl running on port 5555 start an editor and "jack-in" or connect through port 5555 (noted in `.nrepl-port` for automatic configuration)

### notes on clojure interpreter implementation
* to start contributing seriously in clojure: I recommend getting comfortable with conjure.nvim, emacs, Intellij's Curisve plugin, or vscode's Calva extension
* note: we are using clojure for initial prototyping speed, but we should be able to reuse parts of the parser/lexer and rewrite the backend in a different language if preferred. we can also rewrite the parser/lexer if desired. i just want to get something working first

