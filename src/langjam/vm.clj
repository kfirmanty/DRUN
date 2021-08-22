(ns langjam.vm
  (:require [langjam.parser :as p]))

(defn fn-call-block->name [block]
  (-> block second second))

(defn fn-call-block->args [block]
  (drop 2 block))

(defn assignment-block->name [block]
  (-> block second second))

(defn assignment-block->val-block [block]
  (nth block 2))

(defn val-block->type [block]
  (-> block second first))

(defn val-block->value [block]
  (let [type (val-block->type block)
        raw-val (-> block second second)]
    (if (= type :NUMBER)
      (Integer/parseInt raw-val)
      raw-val)))

(defn var-block->name [block]
  (second block))

(defn fn-def-block->name [block]
  (-> block second second))

(defn fn-def-block->args [block]
  (let [args (rest (nth block 2))]
    (if (empty? args)
      []
      (map second args))))

(defn fn-def-block->body-blocks [block]
  (drop 3 block))

(defn fn-def? [block]
  (= (first block) :FN-DEF))

(defn fn-call? [block]
  (= (first block) :FN-CALL))

(defn var? [block]
  (= (first block) :VAR))

(defn assignment? [block]
  (= (first block) :ASSIGNMENT))

(defn val? [block]
  (= (first block) :VAL))

(defn return? [block]
  (= (first block) :RETURN))

(defn var->value [env var-name]
  (get-in env [:vars var-name :value]))

(defn add [env]
  [env (+ (var->value env "a")
          (var->value env "b"))])

(defn sub [env]
  [env (+ (var->value env "a")
          (var->value env "b"))])

(defn print-msg [env & args]
  (print (var->value env "val"))
  [env nil])

(defn env []
  {:fns
   {:ADD {:native? true
          :fn add
          :args ["a" "b"]}
    :SUB {:native? true
          :fn sub
          :args ["a" "b"]}
    :PRINT {:native? true
            :fn print-msg
            :args ["val"]}}
   :vars {}
   :modifiers {};;here will be modifiers from comments stored so they can be accesed by both code and interpreter
   })

(defn bind-variable [env var-name value]
  (assoc-in env [:vars var-name] {:value value
                                  :type "?"}))

(declare exec-fn-call)

(declare exec-return)

(defn exec-assignment-var [env assignment-block]
  (let [var-name (assignment-block->name assignment-block)
        val-block (assignment-block->val-block assignment-block)]
    (bind-variable env (var->value env (var-block->name val-block)))))

(defn exec-assignment-val [env assignment-block]
  (let [var-name (assignment-block->name assignment-block)
        val-block (assignment-block->val-block assignment-block)
        value (val-block->value val-block)]
    (bind-variable env var-name value)))

(defn exec-assignment-fn-call [env [_ var-block fn-call-block]]
  (let [[env value] (exec-fn-call env fn-call-block)
        var-name (var-block->name var-block)]
    (bind-variable env var-name value)))

(defn exec-assignment [env assignment-block]
  (let [val-block (assignment-block->val-block assignment-block)]
    (cond
      (fn-call? val-block) (exec-assignment-fn-call env assignment-block)
      (var? val-block) (exec-assignment-var env assignment-block)
      (val? val-block) (exec-assignment-val env assignment-block))))

(defn expr->val [env expr]
  (cond
    (fn-call? expr)
    (exec-fn-call env expr)
    (var? expr)
    [env (var->value env (var-block->name expr))]
    (val? expr)
    [env (val-block->value expr)]))

(defn bind-fn-variables [env args arg-names]
  (loop [env env [arg & args] args [arg-name & arg-names] arg-names]
    (if (nil? arg)
      env
      (let [[env value] (expr->val env arg)]
        (recur (bind-variable env arg-name value) args arg-names)))))

(defn exec-fn-body [env exprs]
  (loop [env env [expr & exprs] exprs return-val nil]
    (cond return-val [env return-val]
          (nil? expr) [env nil]
          :else (cond (fn-call? expr) (recur (first (exec-fn-call env expr)) exprs return-val)
                      (assignment? expr) (recur (exec-assignment env expr) exprs return-val)
                      (return? expr) (let [[env val] (expr->val env (second expr))] (recur env exprs val))))))

(defn exec-fn-call [env fn-call-block]
  (let [fn-name (fn-call-block->name fn-call-block)
        args (fn-call-block->args fn-call-block)
        old-vars (:vars env)
        fn-meta (get-in env [:fns (keyword fn-name)])
        arg-names (:args fn-meta)
        env (bind-fn-variables env args arg-names)
        [nenv ret-val] (if (:native? fn-meta)
                         ((:fn fn-meta) env)
                         (exec-fn-body env (:fn fn-meta)))]
    [(assoc nenv :vars old-vars) ret-val]))

(defn exec-fn-def [env fn-def-block]
  (let [fn-name (fn-def-block->name fn-def-block)
        args (fn-def-block->args fn-def-block)
        body (fn-def-block->body-blocks fn-def-block)]
    (assoc-in env [:fns (keyword fn-name)]
              {:native? false
               :fn body
               :args args})))

(defn prepare-env [code-str]
  (let [env (env)
        code (p/parser code-str)]
    (loop [env env terms code]
      (if (nil? (first terms))
        env
        (recur (exec-fn-def env (first terms)) (rest terms))))))

(defn call-main [env]
  (exec-fn-call env [:FN-CALL [:FN-NAME "MAIN"]]))
