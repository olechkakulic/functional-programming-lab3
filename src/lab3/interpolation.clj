(ns lab3.interpolation)

(defn limit-queue [q max-size]
  (loop [q q]
    (if (<= (count q) max-size)
      q
      (recur (pop q)))))

(defn normalize-zero [x]
  (let [d (double x)]
    (if (= d -0.0) 0.0 d)))

(defmulti add-point (fn [alg _state _point] alg))
(defmulti alg-ready? (fn [alg _opts _state] alg))
(defmulti interpolate (fn [alg _state _x _opts] alg))
(defmulti min-points (fn [alg _opts] alg))

(defn find-segment [points x]
  (when (>= (count points) 2)
    (some (fn [[p1 p2]]
            (when (and (<= (:x p1) x)
                       (<= x (:x p2)))
              [p1 p2]))
          (partition 2 1 points))))

(defn linear-value [points x]
  (when-let [[p1 p2] (find-segment points x)]
    (let [x1 (:x p1) y1 (:y p1)
          x2 (:x p2) y2 (:y p2)]
      (if (= x1 x2)
        y1
        (let [t (/ (- x x1) (- x2 x1))]
          (+ y1 (* t (- y2 y1))))))))

(defmethod add-point :linear [_ state point]
  (update state :points conj point))

(defmethod alg-ready? :linear [_ _opts state]
  (>= (count (:points state)) 2))

(defmethod min-points :linear [_ _opts]
  2)

(defmethod interpolate :linear [_ state x _opts]
  (when-some [y (linear-value (:points state) x)]
    {:alg :linear :x x :y y}))

(defn choose-window [points n x]
  (let [cnt (count points)]
    (cond
      (zero? cnt) []
      (<= cnt n)  (vec points)
      :else
      (let [closest-idx (->> points
                             (map-indexed (fn [i p]
                                            [i (Math/abs ^double (- x (:x p)))]))
                             (apply min-key second)
                             first)
            n' (min n cnt)
            half (quot (dec n') 2)
            raw-start (- closest-idx half)
            start (-> raw-start (max 0) (min (- cnt n')))]
        (subvec (vec points) start (+ start n'))))))

(defn divided-differences [points]
  (let [xs (mapv :x points)
        ys (mapv :y points)
        n  (count points)]
    (loop [k 0 table ys acc []]
      (if (= k n)
        acc
        (let [acc' (conj acc (first table))
              table' (if (= k (dec n))
                       []
                       (mapv (fn [i]
                               (/ (- (table (inc i)) (table i))
                                  (- (xs (+ i k 1)) (xs i))))
                             (range 0 (- n k 1))))]
          (recur (inc k) table' acc'))))))

(defn newton-eval [coeffs points x]
  (let [xs (mapv :x points)
        n  (count coeffs)]
    (loop [k (dec n)
           acc (nth coeffs (dec n))]
      (if (zero? k)
        acc
        (let [k' (dec k)]
          (recur k' (+ (nth coeffs k') (* (- x (xs k')) acc))))))))

(defn newton-value [points n x]
  (let [window (choose-window points n x)
        coeffs (divided-differences window)]
    (newton-eval coeffs window x)))

(defmethod add-point :newton [_ state point]
  (update state :points conj point))

(defmethod alg-ready? :newton [_ opts state]
  (>= (count (:points state)) (:n opts)))

(defmethod min-points :newton [_ opts]
  (:n opts))

(defmethod interpolate :newton [_ state x opts]
  (let [points (:points state)
        n (:n opts)]
    (when (and n (>= (count points) n))
      {:alg :newton :x x :y (newton-value points n x)})))

(defn make-alg-state []
  {:points clojure.lang.PersistentQueue/EMPTY
   :next-x nil})

(defn init-state [algorithms]
  (into {} (map (fn [alg] [alg (make-alg-state)]) algorithms)))

(defn produce-outputs-for-alg [alg opts alg-state max-x]
  (if-not (alg-ready? alg opts alg-state)
    {:state alg-state :outputs []}
    (let [step (:step opts)
          start-x (or (:next-x alg-state)
                      (some-> alg-state :points first :x))]
      (if (nil? start-x)
        {:state alg-state :outputs []}
        (loop [x start-x
               outs []]
          (if (> x max-x)
            {:state (assoc alg-state :next-x x) :outputs outs}
            (let [res (interpolate alg alg-state x opts)
                  outs' (if res (conj outs res) outs)
                  next-x (+ x step)]
              (recur next-x outs'))))))))

(defn produce-outputs [algorithms opts state max-x]
  (reduce
   (fn [{:keys [state outputs]} alg]
     (let [{new-alg-state :state alg-outputs :outputs}
           (produce-outputs-for-alg alg opts (get state alg) max-x)]
       {:state (assoc state alg new-alg-state)
        :outputs (into outputs alg-outputs)}))
   {:state state :outputs []}
   algorithms))

(defn limit-alg-state [alg opts alg-state]
  (let [max-points (inc (min-points alg opts))]
    (update alg-state :points limit-queue max-points)))

(defn process-point [algorithms opts state point]
  (let [state-with-point
        (reduce (fn [s alg]
                  (update s alg #(add-point alg % point)))
                state
                algorithms)

        state-limited
        (reduce (fn [s alg]
                  (update s alg #(limit-alg-state alg opts %)))
                state-with-point
                algorithms)

        max-x (:x point)
        {:keys [state outputs]} (produce-outputs algorithms opts state-limited max-x)]

    {:state state
     :outputs outputs}))

(defn finalize-outputs [algorithms opts state]
  (let [max-x (->> algorithms
                   (map #(some-> state (get %) :points last :x))
                   (remove nil?)
                   (apply max nil))]
    (if max-x
      (produce-outputs algorithms opts state max-x)
      {:state state :outputs []})))