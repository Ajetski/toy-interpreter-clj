(ns core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [instaparse.core :as insta]))

;; ⚠️  beware ⚠️: global state... 👻 spooky 👻
(def function-table (atom {}))
(def call-stack     (atom []))

(def parser
  (insta/parser
   "MODULE = (W? FUNC W?)+
     FUNC = 'fn' W IDENT W? '(' W? PARAMS W? ')' W? '->' W? 'i32' W? BLOCK
     PARAMS = ''
     IDENT = 'a' | 'b' | 'main'
     BLOCK = '{' W? EXPR W? '}'
     EXPR = ADDITION | LITERAL | FUNCCALL
     FUNCCALL = IDENT '(' W? PARAMS W? ')'
     LITERAL = #'[0-9]+'
     ADDITION = EXPR W? '+' W? EXPR
     W = #'[\n ]+'"))

(defn interpret [ast]
  (pprint ast))

(defn run [opts]
  (if (contains? opts :filename)
    (let [cwd (java.io.File. ".")
          curr-path (.getAbsolutePath cwd)
          filename (:filename opts)
          filepath (str curr-path "/" filename)
          input (slurp filepath)
          ast (parser input)]
      (interpret ast))
    (println "use std in... to be implemented")))

(comment
  (run {:filename "test_input/hello_world.txt"}))
