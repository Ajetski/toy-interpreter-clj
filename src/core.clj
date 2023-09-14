(ns core
  (:require [clojure.pprint :refer [pprint]]))

(defn run [opts]
  (println "1 + 2 =" (+ 1 2))
  (pprint opts))

(defn add [a b]
  (+ a b))

(comment
  (add 1 2))
