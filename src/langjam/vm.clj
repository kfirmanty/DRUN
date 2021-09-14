(ns langjam.vm
  (:require [langjam.parser :as p]
            [langjam.transform :as t]))

(defn var->value [env var-name]
  (get-in env [:vars var-name :value]))

(defn add [env]
  [env (+ (var->value env "a")
          (var->value env "b"))])

(defn sub [env]
  [env (- (var->value env "a")
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

(defn exec-assignment-var [env assignment-block]
  (let [var-name (t/assignment-block->name assignment-block)
        val-block (t/assignment-block->val-block assignment-block)]
    (bind-variable env (t/var-block->name val-block) (var->value env))))

(defn exec-assignment-val [env assignment-block]
  (let [var-name (t/assignment-block->name assignment-block)
        val-block (t/assignment-block->val-block assignment-block)
        value (t/val-block->value val-block)]
    (bind-variable env var-name value)))

(defn exec-assignment-fn-call [env {:keys [name what]}]
  (let [[env value] (exec-fn-call env what)]
    (bind-variable env name value)))

(defn exec-assignment [env assignment-block]
  (let [val-block (t/assignment-block->val-block assignment-block)]
    (cond
      (t/fn-call? val-block) (exec-assignment-fn-call env assignment-block)
      (t/var? val-block) (exec-assignment-var env assignment-block)
      (t/val? val-block) (exec-assignment-val env assignment-block))))

(defn expr->val [env expr]
  (cond
    (t/fn-call? expr)
    (exec-fn-call env expr)
    (t/var? expr)
    [env (var->value env (t/var-block->name expr))]
    (t/val? expr)
    [env (t/val-block->value expr)]))

(defn bind-fn-variables [env args arg-names]
  (loop [env env [arg & args] args [arg-name & arg-names] arg-names]
    (if (nil? arg)
      env
      (let [[env value] (expr->val env arg)]
        (recur (bind-variable env arg-name value) args arg-names)))))

(defn exec-if-test [env if-test-block]
  (let [op (t/if-test-block->op if-test-block)
        left-val (second (expr->val env (t/if-test-block->left if-test-block)))
        right-val (second (expr->val env (t/if-test-block->right if-test-block)))
        op->fn {"==" =
                "!=" not=
                "<" <
                "<=" <=
                ">" >
                ">=" >=}]
    ((get op->fn op) left-val right-val)))

(defn exec-exprs [env exprs]
  (loop [env env [expr & exprs] exprs return-val nil]
    (cond return-val [env return-val]
          (nil? expr) [env nil]
          :else (cond (t/fn-call? expr) (recur (first (exec-fn-call env expr)) exprs return-val)
                      (t/assignment? expr) (recur (exec-assignment env expr) exprs return-val)
                      (t/return? expr) (let [[env val] (expr->val env (:expr expr))] (recur env exprs val))
                      (t/if-cond? expr) (let [test-val (exec-if-test env (t/if-cond-block->if-test-block expr))
                                              [env possible-ret-val] (exec-exprs env (if test-val
                                                                                         (t/if-cond-block->if-block expr)
                                                                                         (if (t/has-else-block? expr)
                                                                                           (t/if-cond-block->else-block expr)
                                                                                           nil)))]
                                          (recur env exprs (or possible-ret-val return-val)))))))

(defn exec-fn-call [env fn-call-block]
  (let [fn-name (t/fn-call-block->name fn-call-block)
        args (t/fn-call-block->args fn-call-block)
        old-vars (:vars env)
        fn-meta (get-in env [:fns (keyword fn-name)])
        arg-names (:args fn-meta)
        env (bind-fn-variables env args arg-names)
        [nenv ret-val] (if (:native? fn-meta)
                         ((:fn fn-meta) env)
                         (exec-exprs env (:fn fn-meta)))]
    [(assoc nenv :vars old-vars) ret-val]))

(defn exec-fn-def [env fn-def-block]
  (let [fn-name (t/fn-def-block->name fn-def-block)
        args (t/fn-def-block->args fn-def-block)
        body (t/fn-def-block->body-blocks fn-def-block)]
    (assoc-in env [:fns (keyword fn-name)]
              {:native? false
               :fn body
               :args args})))

(defn prepare-env [code-str]
  (let [env (env)
        code (filter (complement string?) (p/parse code-str))]
    (loop [env env [term & terms] code]
      (if (nil? term)
        env
        (recur (exec-fn-def env term) terms)))))

(defn call-main [env]
  (exec-fn-call env {:type :fn-call :name "MAIN"}))

(defn run [code]
  (-> code prepare-env call-main))
