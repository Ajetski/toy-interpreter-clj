(ns core
  (:require [instaparse.core :as insta]))

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
(defn get-file [filename]
  (str (-> "."
           java.io.File.
           .getAbsolutePath)
       "/"
       filename))

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
  (let [func (@function-table (get (get func-call 1) 1))
        args (get-all-by-tag :ARG (get-by-tag :ARGS func-call))
        params (get-all-by-tag :PARAM (get-by-tag :PARAMS func))]
    (swap! call-stack conj
           (loop [args args
                  params params
                  vals {}]
             (if (= 0 (count args))
               vals
               (recur (drop 1 args)
                      (drop 1 params)
                      (assoc vals
                             (-> params first second second)
                             (-> args first second interpret))))))
    (let [res (->> func (get-by-tag :BLOCK) (get-by-tag :EXPR) interpret)]
      (swap! call-stack pop)
      res)))

(defmethod interpret :EXPR [expr]
  (loop [terms (get-all-by-tag :TERM expr)
         ops (filter string? expr)
         val 0
         lastop nil]
    (let [term (-> terms first interpret)]
      (if (= (count terms) 1)
        (cond (= lastop "+") (+ val term)
              (= lastop "-") (- val term)
              :else term)
        (recur (drop 1 terms)
               (drop 1 ops)
               (cond (= lastop "+") (+ val term)
                     (= lastop "-") (- val term)
                     :else term)
               (first ops))))))

(defmethod interpret :TERM [term]
  (loop [factors (get-all-by-tag :FACTOR term)
         ops (filter string? term)
         val 1
         lastop nil]
    (let [factor (->> factors first (filter #(not (string? %))) interpret)]
      (if (= (count factors) 1)
        (cond (= lastop "*") (* val factor)
              (= lastop "/") (/ val factor)
              :else  factor)
        (recur (drop 1 factors)
               (drop 1 ops)
               (cond (= lastop "*") (* val factor)
                     (= lastop "/") (/ val factor)
                     :else factor)
               (first ops))))))

(defmethod interpret :FACTOR [factor]
  (interpret (second factor)))

(defmethod interpret :LITERAL [literal]
  (Integer/parseInt (second literal)))

(defmethod interpret :IDENT [ident]
  ((last @call-stack) (second ident)))

(defmethod interpret :default [node]
  (println "UNKNOWN NODE:" node))

(defn invoke-fn [fn-name]
  (interpret [:FUNCCALL [:IDENT fn-name] "(" [:PARAMS] ")"]))

;;; ENTRYPOINT ;;;
(defn run [opts]
  (if (contains? opts :filename)
    (prn "no filename provided... running default file: test_input/hello_world.txt"))
  (interpret (->> (if (contains? opts :filename)
                    (get-file (:filename opts))
                    (get-file "test_input/hello_world.txt"))
                  slurp
                  parse
                  remove-whitespace))
  (prn (invoke-fn "main")))

;;; REPL PLAYGROUND ;;;
(comment
  (run {:filename "test_input/hello_world.txt"}))
