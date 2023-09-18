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
     IDENT = #'[\\-_]*[a-zA-Z][a-zA-Z\\-_0-9]*'
     BLOCK = '{' W? EXPR W? '}'
     EXPR = TERM W? {('+'|'-') W? TERM W?}
     TERM = FACTOR W? {('*'|'/') W? FACTOR W?}
     FACTOR = FUNCCALL | LITERAL | ( '(' EXPR ')' ) | IDENT
     FUNCCALL = IDENT '(' W? PARAMS W? ')'
     LITERAL = #'-?[0-9]+'
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
(defmulti interpret first)

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
  (loop [terms (get-all-by-tag :TERM expr)
         ops (filter string? expr)
         val nil
         lastop nil]
    (let [term (first terms)
          term-val (interpret term)
          rest-terms (drop 1 terms)
          op (first ops)
          rest-ops (drop 1 ops)]
      (if (= (count terms) 1)
        (if (= lastop nil)
          term-val
          (if (= lastop "+")
            (+ val term-val)
            (- val term-val)))
        (recur rest-terms
               rest-ops
               (cond (= lastop "+") (+ (or val 0) term-val)
                     (= lastop "-") (- (or val 0) term-val)
                     :else term-val)
               op)))))

(defmethod interpret :TERM [term]
  (loop [factors (get-all-by-tag :FACTOR term)
         ops (filter string? term)
         val nil
         lastop nil]
    (let [factor (filter #(not (string? %)) (first factors))
          factor-val (interpret factor)
          rest-factors (drop 1 factors)
          op (first ops)
          rest-ops (drop 1 ops)]
      (prn val factor-val factor)
      (if (= (count factors) 1)
        (if (= lastop nil)
          factor-val
          (if (= lastop "*")
            (* val factor-val)
            (/ val factor-val)))
        (recur rest-factors
               rest-ops
               (cond (= lastop "*") (* (or val 1) factor-val)
                     (= lastop "/") (/ (or val 1) factor-val)
                     :else factor-val)
               op)))))

(defmethod interpret :FACTOR [factor]
  (interpret (second factor)))

(defmethod interpret :LITERAL [literal]
  (prn :LITERAL literal)
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
      ; (pprint ast)
      (prn (invoke-fn "main")))
    (println "use std in... to be implemented")))

;;; REPL PLAYGROUND ;;;
(comment
  (run {:filename "test_input/hello_world.txt"}))

