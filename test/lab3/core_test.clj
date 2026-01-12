(ns lab3.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [lab3.core :as core]
            [lab3.interpolation :as interp]))

(deftest parse-point-test
  (testing "Parsing input strings"
    (is (= {:x 1.0 :y 2.0} (core/parse-point "1 2")))
    (is (= {:x 1.0 :y 2.0} (core/parse-point "1;2")))
    (is (= {:x 1.0 :y 2.0} (core/parse-point "1,2")))
    (is (nil? (core/parse-point "")))
    (is (nil? (core/parse-point "abc def")))))

(deftest process-point-linear-test
  (testing "Stream processing: linear interpolation"
    (let [algorithms [:linear]
          opts {:step 1 :n 4}
          state0 (interp/init-state algorithms)

          {:keys [state outputs]} (interp/process-point algorithms opts state0 {:x 0 :y 0})
          outputs (map #(-> %
                            (update :x interp/normalize-zero)
                            (update :y interp/normalize-zero))
                       outputs)]

      (is (= [] outputs))

      (let [{:keys [_ outputs]} (interp/process-point algorithms opts state {:x 1 :y 1})
            outputs (map #(-> %
                              (update :x interp/normalize-zero)
                              (update :y interp/normalize-zero))
                         outputs)]

        (is (= [{:alg :linear :x 0.0 :y 0.0}
                {:alg :linear :x 1.0 :y 1.0}]
               outputs))))))
