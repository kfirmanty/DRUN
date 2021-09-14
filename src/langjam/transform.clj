(ns langjam.transform)

(defn fn-call-block->name [fn-call]
  (:name fn-call))

(defn fn-call-block->args [fn-call]
  (:args fn-call))

(defn assignment-block->name [block]
  (:name block))

(defn assignment-block->val-block [block]
  (:what block))

(defn val-block->type [val]
  (:value-type val))

(defn val-block->value [val]
  (:value val))

(defn var-block->name [var]
  (:name var))

(defn fn-def-block->name [fn-def]
  (:name fn-def))

(defn fn-def-block->args [fn-def]
  (:args fn-def))

(defn fn-def-block->body-blocks [fn-def]
  (:body fn-def))

(defn if-test-block->op [block]
  (:op block))

(defn if-test-block->left [block]
  (:left block))

(defn if-test-block->right [block]
  (:right block))

(defn if-cond-block->if-test-block [block]
  (:test block))

(defn if-cond-block->if-block [block]
  (:then block))

(defn if-cond-block->else-block [block]
  (:else block))

(defn fn-def? [fn-def]
  (= (:type fn-def) :fn-def))

(defn fn-call? [fn-call]
  (= (:type fn-call) :fn-call))

(defn var? [var]
  (= (:type var) :var))

(defn assignment? [block]
  (= (:type block) :assignment))

(defn val? [val]
  (= (:type val) :val))

(defn return? [return]
  (= (:type return) :return))

(defn if-cond? [block]
  (= (:type block) :if-cond))

(defn has-else-block? [if-block]
  (some? (:else if-block)))
