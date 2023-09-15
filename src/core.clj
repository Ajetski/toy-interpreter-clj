(ns core
  (:require [clojure.pprint :refer [pprint]]))

(defn parse [input]
  (println "the input is:")
  (pprint input))

(defn run [opts]
  (if (contains? opts :filename)
    (let [cwd (java.io.File. ".")
          cwd-str (.getAbsolutePath cwd)
          filename (:filename opts)
          file-str (str cwd-str "/" filename)]
      (println "file-str " file-str)
        (parse (slurp file-str)))
      (println "use std in")))

(comment
  (run {:filename "test_input/hello_world.txt"}))
