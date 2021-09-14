(ns langjam.vm-test
  (:require [langjam.vm :as vm]
            [clojure.test :as t]))

(t/deftest should-properly-call-native-fn
  (t/is (= [(vm/env) 5] (vm/exec-fn-call (vm/env) {:type :fn-call,
                                                   :name "ADD",
                                                   :args
                                                   '({:type :val, :value-type :NUMBER, :value 2}
                                                     {:type :val, :value-type :NUMBER, :value 3})}))))

(t/deftest should-properly-execute-val-return
  (t/is (= [(vm/env) 2] (vm/exec-exprs (vm/env) [{:type :return
                                                  :expr {:type :val, :value-type :NUMBER, :value 2}}]))))

(t/deftest should-properly-execute-simple-fn
  (let [env (vm/prepare-env "FN MAIN()
PRINT(\"test\n\")
PRINT(ADD(2 3))
PRINT(\"\n\")
RETURN 5
END")]
    (t/is (= [(assoc-in (vm/env) [:fns :MAIN] {:native? false
                                               :fn '({:type :fn-call :name "PRINT"
                                                      :args ({:type :val, :value-type :STRING, :value "test\n"})}
                                                     {:type :fn-call, :name "PRINT",
                                                      :args ({:type :fn-call, :name "ADD",
                                                              :args ({:type :val, :value-type :NUMBER, :value 2}
                                                                     {:type :val, :value-type :NUMBER, :value 3})})}
                                                     {:type :fn-call, :name "PRINT",
                                                      :args ({:type :val, :value-type :STRING, :value "\n"})}
                                                     {:type :return, :expr {:type :val, :value-type :NUMBER, :value 5}}), :args '()}) 5]
             (vm/call-main env)))))

(t/deftest should-return-variable-val
  (let [env (vm/prepare-env "FN MAIN()
x = ADD(2 3)
RETURN x
END")]
    (t/is (= 5 (-> env vm/call-main second)))))

(t/deftest should-parse-all-fns-defined
  (let [env (vm/prepare-env "FN ADD_2(a)
RETURN ADD(a 2)
END
FN MAIN()
x = 3
RETURN ADD_2(x)
END")]
    (t/is (= 5 (-> env vm/call-main second)))))

(t/deftest should-execute-if-conditional
  (let [env (vm/prepare-env "FN MAIN()
x = 5
IF (x > 4) x = ADD(x 5) ELSE x = SUB(x 5) ENDIF
RETURN x
END")]
    (t/is (= 10 (-> env vm/call-main second)))))

(t/deftest should-execute-else-conditional
  (let [env (vm/prepare-env "FN MAIN()
x = 5
IF (x > 5) x = ADD(x 5) ELSE x = SUB(x 5) ENDIF
RETURN x
END")]
    (t/is (= 0 (-> env vm/call-main second)))))

(t/deftest should-skip-if-test-false-and-no-else-block
  (let [env (vm/prepare-env "FN MAIN()
x = 5
IF (x > 5) x = ADD(x 5) ENDIF
RETURN x
END")]
    (t/is (= 5 (-> env vm/call-main second)))))
