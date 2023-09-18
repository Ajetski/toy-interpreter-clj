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

### interpreter examples

_(last updated 9/18/23, run or read code for most up-to-date results)_

input:
```rs
fn a(num: i32) -> i32 {
  num + b()
}

fn b() -> i32 {
  6
}

fn main() -> i32 {
  2 * a(2) + 100 * b() + 3 * (-2 + 2)
}
```

output:
616

grammar ([docs](https://github.com/Engelberg/instaparse/tree/master#notation) on notation):
```
MODULE = (W? FUNC W?)+
FUNC = 'fn' W IDENT W? '(' W? PARAMS? W? ')' W? '->' W? 'i32' W? BLOCK
PARAMS = (PARAM {',' PARAM } ','?)? 
PARAM = IDENT W? ':' W? 'i32'
ARGS = (ARG {',' ARG } ','?)? 
ARG = W? EXPR W?
IDENT = #'[\\-_]*[a-zA-Z][a-zA-Z\\-_0-9]*'
BLOCK = '{' W? EXPR W? '}'
EXPR = TERM W? {('+'|'-') W? TERM W?}
TERM = FACTOR W? {('*'|'/') W? FACTOR W?}
FACTOR = FUNCCALL | LITERAL | ( '(' EXPR ')' ) | IDENT
FUNCCALL = IDENT '(' W? ARGS? W? ')'
LITERAL = #'-?[0-9]+'
W = #'[ \n]+'
```

