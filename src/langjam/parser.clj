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

IF-COND = <'IF'> <WS> IF-TEST <WS>? <'\n'?> IF-BLOCK <WS>? ELSE-BLOCK? <WS>? <'ENDIF'>
IF-TEST = (VAL-EXPR <WS>?) TEST-OP <WS>? VAL-EXPR <WS>?
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
