(ns core
  (:require [clojure.pprint :refer [pprint]]
            ; [clojure.string :as string]
            [instaparse.core :as insta]))

;;; PARSER ;;;
(def parser
  (insta/parser
   "MODULE = (W? FUNC W?)+
     FUNC = 'fn' W IDENT W? '(' W? PARAMS W? ')' W? '->' W? 'i32' W? BLOCK
     PARAMS = ''
     IDENT = #'[a-zA-Z\\-_][a-zA-Z\\-_0-9]*'
     BLOCK = '{' W? EXPR W? '}'
     EXPR = ADDITION | LITERAL | FUNCCALL
     FUNCCALL = IDENT '(' W? PARAMS W? ')'
     LITERAL = #'[0-9]+'
     ADDITION = EXPR W? '+' W? EXPR
     W = #'[ \n]+'"))
(defn parse [input]
  (parser input))

;;; UTILS ;;;
(defn get-by-tag "returns the first element in a list that starts with the provided tag"
  [tag node]
  (first (drop-while #(not (= (get % 0) tag)) node)))

(defn get-all-by-tag "returns the first element in a list that starts with the provided tag"
  [tag node]
  (filter #(= (get % 0) tag) node))

;; ⚠️  beware ⚠️: global state... 👻 spooky 👻
(def function-table (atom {}))
(def call-stack     (atom []))

;;; INTERPRETER ;;;
(defmulti interpret
  #(get % 0))

(defmethod interpret :MODULE [modlue]
  (let [funcs (filter #(= (get % 0) :FUNC) modlue)]
    (doseq [func funcs] (interpret func))))

(defmethod interpret :FUNC [function]
  (let [ident (get-by-tag :IDENT function)
        fn-name (second ident)]
    (swap! function-table assoc
           fn-name
           function)))

(defmethod interpret :FUNCCALL [node]
  (let [ident (get node 1)
        fn-name (get ident 1)
        function (@function-table fn-name)
        block (get-by-tag :BLOCK function)
        expr (get-by-tag :EXPR block)]
    (interpret expr)))

(defmethod interpret :EXPR [expr]
  (interpret (second expr)))

(defmethod interpret :ADDITION [add]
  (let [exprs (get-all-by-tag :EXPR add)
        left (interpret (first exprs))
        right (interpret (second exprs))]
    (+ left right)))

(defmethod interpret :LITERAL [literal]
  (Integer/parseInt (second literal)))

(defmethod interpret :default [node]
  (println "UNKNOWN NODE:" node))

(defn invoke-fn [fn-name]
  (interpret [:FUNCCALL [:IDENT fn-name] "(" [:PARAMS] ")"]))

;;; ENTRYPOINT ;;;
(defn run [opts]
  (if (contains? opts :filename)
    (let [cwd (java.io.File. ".")
          curr-path (.getAbsolutePath cwd)
          filename (:filename opts)
          filepath (str curr-path "/" filename)
          input (slurp filepath)
          ast (parse input)]
      (interpret ast)
      (prn (invoke-fn "main")))
    (println "use std in... to be implemented")))

;;; REPL PLAYGROUND ;;;
(comment
   (run {:filename "test_input/hello_world.txt"}))

