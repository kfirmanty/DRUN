(ns langjam.vm-test
  (:require [langjam.vm :as vm]
            [langjam.parser :as parser]
            [clojure.test :as t]))


(t/deftest should-properly-call-native-fn
  (t/is (= [(vm/env) 5] (vm/exec-fn-call (vm/env) [:FN-CALL [:FN-NAME "ADD"] [:VAL [:NUMBER "2"]] [:VAL [:NUMBER "3"]]]))))

(t/deftest should-properly-execute-simple-fn
  (let [env (vm/prepare-env "FN MAIN()
PRINT(\"test\n\")
PRINT(ADD(2 3))
PRINT(\"\n\")
RETURN 5
END")]
    (t/is (= [(assoc-in (vm/env)
                        [:fns :MAIN]
                        {:native? false,
                         :fn
                         '([:FN-CALL [:FN-NAME "PRINT" ""] [:VAL [:STRING "test\n"]]]
                           [:FN-CALL
                            [:FN-NAME "PRINT" ""]
                            [:FN-CALL
                             [:FN-NAME "ADD" ""]
                             [:VAL [:NUMBER "2"]]
                             [:VAL [:NUMBER "3"]]]]
                           [:FN-CALL [:FN-NAME "PRINT" ""] [:VAL [:STRING "\n"]]]
                           [:RETURN [:VAL [:NUMBER "5"]]]),
                         :args []})
              5]
             (vm/call-main env)))))

(t/deftest should-return-variable-val
  (let [env (vm/prepare-env "FN MAIN()
x = ADD(2 3)
RETURN x
END")]
    (t/is (= 5 (-> env vm/call-main second)))))
