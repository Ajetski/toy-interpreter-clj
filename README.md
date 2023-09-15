## step 1- interpreter

### getting started

* depedencies:
    * required: [clojure](https://clojure.org/guides/install_clojure) (language)
    * optional: [bb](https://babashka.org/) (script runner/Graavl VM clojure runtime)
* editing:
    * to start repl using bb:
        * run `bb nrepl`
    * to start repl using clj:
        * run `clj -M:repl --port 5555`
    * once you have cider/nrepl running on port 5555 start an editor and "jack-in" or connect through port 5555
* run program via cli:
    * to run using bb:
        * `bb run :filename <path_to_file>`
            * for example `bb run :filename test_input/hello_world.txt`
    * to run using clj:
        * `clj -X core/run :filename <path_to_file>`
            * for example `clj -X core/run :filename test_input/hello_world.txt`

### notes on clojure interpreter implementation
* to start contributing seriously in clojure: I recommend getting comfortable with conjure.nvim, emacs, Intellij's Curisve plugin, or vscode's Calva extension
* note: we are using clojure for initial prototyping speed, but we should be able to reuse parts of the parser/lexer and rewrite the backend in a different language if preferred. we can also rewrite the parser/lexer if desired. i just want to get something working first

### status of parser

input:
```rs
fn a() -> i32 {
  5
}

fn b() -> i32 {
  6
}

fn main() -> i32 {
  a() + b()
}
```

output:
```edn
[:MODULE
 [:FUNC
  "fn"
  [:IDENT "a"]
  "("
  [:PARAMS]
  ")"
  "->"
  "i32"
  [:BLOCK "{" [:EXPR [:LITERAL "5"]] "}"]]
 [:FUNC
  "fn"
  [:IDENT "b"]
  "("
  [:PARAMS]
  ")"
  "->"
  "i32"
  [:BLOCK "{" [:EXPR [:LITERAL "6"]] "}"]]
 [:FUNC
  "fn"
  [:IDENT "main"]
  "("
  [:PARAMS]
  ")"
  "->"
  "i32"
  [:BLOCK
   "{"
   [:EXPR
    [:ADDITION
     [:EXPR [:FUNCCALL [:IDENT "a"] "(" [:PARAMS] ")"]]
     "+"
     [:EXPR [:FUNCCALL [:IDENT "b"] "(" [:PARAMS] ")"]]]]
   "}"]]]
```

grammar:
```
MODULE = FUNC+
     FUNC = 'fn' IDENT '(' PARAMS ')' '->' 'i32' BLOCK
     PARAMS = ''
     IDENT = 'a' | 'b' | 'main'
     BLOCK = '{' EXPR '}'
     EXPR = ADDITION | LITERAL | FUNCCALL
     FUNCCALL = IDENT '(' PARAMS ')'
     LITERAL = #'[0-9]+'
     ADDITION = EXPR '+' EXPR
```

