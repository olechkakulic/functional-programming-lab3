(ns lab3.core
  (:require [clojure.string :as str]
            [lab3.interpolation :as interp])
  (:gen-class))

(defn parse-dbl [s]
  (Double/parseDouble s))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-args [args]
  (loop [algorithms []
         opts {:step 1.0 :n 4}
         args args]
    (if (empty? args)
      (do
        (when (empty? algorithms)
          (binding [*out* *err*]
            (println "Error: at least one of --linear or --newton must be given"))
          (System/exit 1))
        (when (<= (:step opts) 0)
          (binding [*out* *err*]
            (println "Error: --step must be > 0"))
          (System/exit 1))
        {:algorithms algorithms :opts opts})

      (let [[a & rest-args] args]
        (cond
          (= a "--linear")
          (recur (conj algorithms :linear) opts rest-args)

          (= a "--newton")
          (recur (conj algorithms :newton) opts rest-args)

          (= a "--step")
          (recur algorithms (assoc opts :step (parse-dbl (first rest-args))) (rest rest-args))

          (or (= a "-n") (= a "--n"))
          (recur algorithms (assoc opts :n (parse-int (first rest-args))) (rest rest-args))

          :else
          (throw (ex-info (str "Unexpected argument: " a) {})))))))

(defn parse-point [line]
  (let [t (str/trim line)]
    (when-not (str/blank? t)
      (let [parts (str/split t #"[;,\s\t]+")]
        (when (>= (count parts) 2)
          (try
            {:x (parse-dbl (first parts))
             :y (parse-dbl (second parts))}
            (catch NumberFormatException _ nil)))))))

(defn fmt3 [x]
  (let [rounded (/ (Math/round (* x 1000.0)) 1000.0)
        s (String/format java.util.Locale/US "%.3f"
                         (to-array [(double rounded)]))]
    (.replaceFirst s "\\.?0+$" "")))

(defn -main [& args]
  (let [{:keys [algorithms opts]} (parse-args args)]
    (loop [state (interp/init-state algorithms)]
      (if-some [line (read-line)]
        (if-let [p (parse-point line)]
          (let [{:keys [state outputs]}
                (interp/process-point algorithms opts state p)]
            (doseq [{:keys [alg x y]} outputs]
              (println (format "%s: %s %s"
                               (name alg) (fmt3 x) (fmt3 y))))
            (recur state))
          (recur state))
        (let [{:keys [outputs]} (interp/finalize-outputs algorithms opts state)]
          (doseq [{:keys [alg x y]} outputs]
            (println (format "%s: %s %s"
                             (name alg) (fmt3 x) (fmt3 y)))))))))
