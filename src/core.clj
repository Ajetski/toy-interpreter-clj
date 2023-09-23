(ns core
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]))

;;; PARSER ;;;
(def parse
  (insta/parser
   "MODULE = (W? FUNC W?)+
    FUNC = 'fn' W IDENT W? '(' W? PARAMS? W? ')' W? '->' W? 'i32' W? BLOCK
    PARAMS = (PARAM {',' PARAM } ','?)? 
    PARAM = IDENT W? ':' W? 'i32'
    ARGS = (ARG {',' ARG } ','?)? 
    ARG = W? EXPR W?
    IDENT = #'[\\-_]*([a-zA-Z][a-zA-Z\\-_0-9]*)?'
    BLOCK = '{' W? EXPR W? '}'
    EXPR = TERM W? {('+'|'-') W? TERM W?}
    TERM = FACTOR W? {('*'|'/') W? FACTOR W?}
    FACTOR = FUNCCALL | LITERAL | ( '(' EXPR ')' ) | IDENT
    FUNCCALL = IDENT '(' W? ARGS? W? ')'
    LITERAL = #'-?[0-9]+'
    W = #'[ \n]+'"))

;;; UTILS ;;;
(defn get-file-path [filename]
  (-> (java.io.File. ".")
      .getAbsolutePath
      (str "/" filename)))

(defn get-by-tag
  "returns the first element in a list that starts with the provided tag"
  [tag node]
  (->> node
       (drop-while #(not (= (get % 0) tag)))
       first))

(defn get-all-by-tag
  "returns all elements in a list that starts with the provided tag"
  [tag node]
  (filter #(= (get % 0) tag) node))

(defn remove-whitespace
  "recursively removes all :W nodes in an ast"
  [node]
  (->> node
       (filterv #(not (and (coll? %)
                           (= (first %) :W))))
       (mapv #(if (coll? %)
                (remove-whitespace %)
                %))))

(defn dbg-print
  "prints out data before returning it. useful in debugging chained expressions"
  [x]
  (pprint x)
  x)

;;; INTERPRETER ;;;
(defmulti interpret
  "takes in a context and an ast node
  multimethod returns whatever makes sense in evaluating that ast-node"
  (fn [_cx ast-node] (first ast-node)))

(defmethod interpret :MODULE [cx module]
  (loop [funcs (get-all-by-tag :FUNC module)
         cx cx]
    (if (empty? funcs)
      cx
      (recur
       (drop 1 funcs)
       (interpret cx (first funcs))))))

(defmethod interpret :FUNC [cx function]
  (update cx :function-table assoc
          (->> function
               (get-by-tag :IDENT)
               second)
          function))

(defmethod interpret :FUNCCALL [cx func-call]
  (->> ((cx :function-table) (-> func-call second second))
       (get-by-tag :BLOCK)
       (get-by-tag :EXPR)
       (interpret
        (update cx :call-stack conj
                (loop [args (->> func-call
                                 (get-by-tag :ARGS)
                                 (get-all-by-tag :ARG))
                       params (->> ((cx :function-table) (-> func-call second second))
                                   (get-by-tag :PARAMS)
                                   (get-all-by-tag :PARAM))
                       vals {}]
                  (if (empty? args)
                    vals
                    (recur (drop 1 args)
                           (drop 1 params)
                           (assoc vals
                                  (-> params first second second)
                                  (interpret cx (-> args first second))))))))))

(defmethod interpret :EXPR [cx expr]
  (loop [terms (get-all-by-tag :TERM expr)
         ops (filter string? expr)
         val 0
         lastop nil]
    (if (empty? terms)
      val
      (let [term (->> terms first (interpret cx))]
        (recur (drop 1 terms)
               (drop 1 ops)
               (cond (= lastop "+") (+ val term)
                     (= lastop "-") (- val term)
                     :else term)
               (first ops))))))

(defmethod interpret :TERM [cx term]
  (loop [factors (get-all-by-tag :FACTOR term)
         ops (filter string? term)
         val 1
         lastop nil]
    (if (empty? factors)
      val
      (let [factor (->> factors first (filter #(not (string? %))) (interpret cx))]
        (recur (drop 1 factors)
               (drop 1 ops)
               (cond (= lastop "*") (* val factor)
                     (= lastop "/") (/ val factor)
                     :else factor)
               (first ops))))))

(defmethod interpret :FACTOR [cx factor]
  (interpret cx (second factor)))

(defmethod interpret :LITERAL [_cx literal]
  (Integer/parseInt (second literal)))

(defmethod interpret :IDENT [cx ident]
  ((-> cx :call-stack last) (second ident)))

(defmethod interpret :default [_cx node]
  (println "UNKNOWN NODE:" node))

(defn invoke-fn [cx fn-name]
  (interpret cx [:FUNCCALL [:IDENT fn-name] "(" [:PARAMS] ")"]))

;;; ENTRYPOINT ;;;
(defn run [opts]
  (if (not (contains? opts :filename))
    (println "running default file: test_input/hello_world.txt"))
  (-> (interpret {:function-table {}
                  :call-stack []}
                 (-> (:filename opts)
                     (or "test_input/hello_world.txt")
                     get-file-path
                     slurp
                     parse
                     remove-whitespace))
      (invoke-fn "main")
      println))

;;; REPL PLAYGROUND ;;;
(comment
  (run {:filename "test_input/hello_world.txt"}))

