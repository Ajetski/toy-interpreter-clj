(ns core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [instaparse.core :as insta]))

;; ⚠️  beware ⚠️: global state... 👻 spooky 👻
(def function-table (atom {}))
(def call-stack     (atom []))

(def language-grammar
  (insta/parser
   "MODULE = FUNC+
     FUNC = 'fn' IDENT '(' PARAMS ')' '->' 'i32' BLOCK
     PARAMS = ''
     IDENT = 'a' | 'b' | 'main'
     BLOCK = '{' EXPR '}'
     EXPR = ADDITION | LITERAL | FUNCCALL
     FUNCCALL = IDENT '(' PARAMS ')'
     LITERAL = #'[0-9]+'
     ADDITION = EXPR '+' EXPR"))

(defn lex-and-parse [raw-input]
  (let [input (string/replace raw-input #"[\n\s]" "")]
    (language-grammar input)))

(defn interpret [ast]
  (pprint ast))

(defn run [opts]
  (if (contains? opts :filename)
    (let [cwd (java.io.File. ".")
          cwd-str (.getAbsolutePath cwd)
          filename (:filename opts)
          filename-str (str cwd-str "/" filename)
          file-contents (slurp filename-str)
          ast (lex-and-parse file-contents)]
      (interpret ast))
    (println "use std in... to be implemented")))

(comment
  (run {:filename "test_input/hello_world.txt"}))
