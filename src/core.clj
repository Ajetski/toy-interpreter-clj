(ns core
  (:require [instaparse.core :as insta]
            ; [clojure.pprint :refer [pprint]]
            ))

;;; PARSER ;;;
(def parser
  (insta/parser
   "MODULE = (W? FUNC W?)+
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
     W = #'[ \n]+'"))
(defn parse [input]
  (parser input))

;;; UTILS ;;;
(defn get-by-tag "returns the first element in a list that starts with the provided tag"
  [tag node]
  (first (drop-while #(not (= (get % 0) tag)) node)))

(defn get-all-by-tag "returns all elements in a list that starts with the provided tag"
  [tag node]
  (filter #(= (get % 0) tag) node))

(defn remove-whitespace "recursively removes all :W nodes in an ast"
  [node]
  (->> node
       (filter #(not (and (coll? %)
                          (= (first %) :W))))
       (map #(if (coll? %)
               (remove-whitespace %)
               %))
       (into [])))

;;; INTERPRETER ;;;
(def function-table (atom {}))
(def call-stack     (atom []))

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

(defmethod interpret :FUNCCALL [func-call]
  (let [ident (get func-call 1)
        func-name (get ident 1)
        func (@function-table func-name)
        block (get-by-tag :BLOCK func)
        expr (get-by-tag :EXPR block)
        args (get-all-by-tag :ARG (get-by-tag :ARGS func-call))
        params (get-all-by-tag :PARAM (get-by-tag :PARAMS func))
        vals (loop [args args
                    params params
                    vals {}]
               (if (= 0 (count args))
                 vals
                 (let [param (first params)
                       ident (second param)
                       param-name (second ident)
                       rest-params (drop 1 params)
                       arg (first args)
                       rest-args (drop 1 args)
                       expr (second arg)
                       ]
                   (recur rest-args
                          rest-params
                          (assoc vals param-name
                                 (interpret expr))))))]
    (swap! call-stack conj vals) ;; set up call-stack
    (let [res (interpret expr)]
      (swap! call-stack pop) ;; clean up call-stack
      res)))

(defmethod interpret :EXPR [expr]
  (loop [terms (get-all-by-tag :TERM expr)
         ops (filter string? expr)
         val 0
         lastop nil]
    (let [term (first terms)
          term-val (interpret term)
          rest-terms (drop 1 terms)
          op (first ops)
          rest-ops (drop 1 ops)]
      (if (= (count terms) 1)
        (cond (= lastop "+") (+ val term-val)
              (= lastop "-") (- val term-val)
              :else term-val)
        (recur rest-terms
               rest-ops
               (cond (= lastop "+") (+ val term-val)
                     (= lastop "-") (- val term-val)
                     :else term-val)
               op)))))

(defmethod interpret :TERM [term]
  (loop [factors (get-all-by-tag :FACTOR term)
         ops (filter string? term)
         val 1
         lastop nil]
    (let [factor (filter #(not (string? %)) (first factors))
          factor-val (interpret factor)
          rest-factors (drop 1 factors)
          op (first ops)
          rest-ops (drop 1 ops)]
      (if (= (count factors) 1)
        (cond (= lastop "*") (* val factor-val)
              (= lastop "/") (/ val factor-val)
              :else  factor-val)
        (recur rest-factors
               rest-ops
               (cond (= lastop "*") (* val factor-val)
                     (= lastop "/") (/ val factor-val)
                     :else factor-val)
               op)))))

(defmethod interpret :FACTOR [factor]
  (interpret (second factor)))

(defmethod interpret :LITERAL [literal]
  (Integer/parseInt (second literal)))

(defmethod interpret :IDENT [ident]
  (let [scope (last @call-stack)
        name (second ident)]
    (scope name)))

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
          ast (parse input)
          clean-ast (remove-whitespace ast)]
      ; (pprint clean-ast)
      (interpret clean-ast)
      (prn (invoke-fn "main")))
    (println "use std in... to be implemented")))

;;; REPL PLAYGROUND ;;;
(comment
  (run {:filename "test_input/hello_world.txt"}))

