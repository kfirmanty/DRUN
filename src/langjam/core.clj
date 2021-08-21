(ns langjam.core
  (:gen-class)
  (:require [instaparse.core :as insta]))

(comment "
")

(defn -main [& args]
  (println "hello"))

(def parser (insta/parser "
S = TERM+
TERM = COMMENT | FN-DEF
COMMENT = '#' (#'\\s' | #'\\w')+ '\n'
FN-DEF = 'FN' (#'\\s' | #'\\w')+ 'END'") )
