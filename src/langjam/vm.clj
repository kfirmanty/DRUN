(ns langjam.vm
  (:require [langjam.parser :as p]))

(defn add [env & args])

(defn sub [env & args])

(defn print-msg [env & args])

(defn env []
  {:fn
   {:ADD add
    :SUB sub
    :PRINT print-msg}})

(defn execute-fn [env fn-code])
