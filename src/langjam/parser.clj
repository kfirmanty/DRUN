(ns langjam.parser
  (:require [instaparse.core :as insta]))

(def parser (insta/parser "
<S> = TERM+
<TERM> = (FN-DEF | COMMENT) '\n'?

COMMENT = <'#'> (<WS> | #'\\w+')+ <'\n'>

FN-DEF = <'FN'> <WS> FN-NAME ARG-LIST <WS> <'\n'?> (EXPRESSION <'\n'?>)+ <'END'>
FN-NAME = #'[A-Z0-9_]+'
ARG-LIST = <'('> (VAR <WS>?)* <')'>

<EXPRESSION> = ASSIGNMENT | FN-CALL | RETURN | IF-COND

ASSIGNMENT = VAR <WS>? <'='> <WS>? (FN-CALL | VAL)

RETURN = <'RETURN'> <WS> VAL-EXPR

FN-CALL = FN-NAME <'('> (VAL-EXPR <WS>?)* <')'>

IF-COND = <'IF'> <WS> <'('> IF-TEST?<')'> <WS> <'\n'?> IF-BLOCK <WS>? ELSE-BLOCK? <WS>? <'ENDIF'>
IF-TEST = VAL-EXPR <WS>? TEST-OP <WS>? VAL-EXPR <WS>?
TEST-OP = '==' | '<' | '<=' | '>=' | '>' | '!='
IF-BLOCK = (EXPRESSION <WS>? <'\n'?>)+
ELSE-BLOCK = <'ELSE'> <WS>? <'\n'?> (EXPRESSION <'\n'?>)+

<VAL-EXPR> = (VAR | FN-CALL | VAL)

VAR = #'[a-z0-9_]+'
VAL = NUMBER | STRING
NUMBER = #'[0-9]+'
STRING = <'\"'> #'[\\w\\s\\d]+' <'\"'>

ANL=<'\n'?>
WS = #'\\s+'"))

(defn parse [text]
  (->> text
       parser
       (insta/transform {:VAL (fn [[val-type val]] {:type :val
                                                  :value-type val-type
                                                  :value (if (= val-type :NUMBER)
                                                           (Integer/parseInt val)
                                                           val)})
                         :VAR (fn [var-name] {:type :var
                                              :name var-name})
                         :RETURN (fn [expr] {:type :return
                                             :expr expr})
                         :FN-CALL (fn [fn-name & args]
                                    {:type :fn-call
                                     :name fn-name
                                     :args args})
                         :ARG-LIST (fn [& args] (map :name args))
                         :FN-NAME identity
                         :FN-DEF (fn [fn-name args & body]
                                   {:type :fn-def
                                    :name fn-name
                                    :args args
                                    :body body})
                         :ASSIGNMENT (fn [to what]
                                       {:type :assignment
                                        :name (:name to)
                                        :what what})
                         :ELSE-BLOCK (fn [& exprs] exprs)
                         :IF-BLOCK (fn [& exprs] exprs)
                         :IF-TEST (fn [left [_ op] right]
                                    {:left left
                                     :op op
                                     :right right})
                         :IF-COND (fn [test then & [else]]
                                    {:type :if-cond
                                     :test test
                                     :then then
                                     :else else})})))
