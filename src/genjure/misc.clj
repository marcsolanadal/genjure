(ns genjure.misc)

(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defmacro mean
  [iterations function]
  (loop [n 0 counter 0]
    (if (= n iterations)
      (round 1 (double (/ counter iterations)))
      (if (true? (eval function))
        (recur (inc n) (inc counter))
        (recur (inc n) counter)))))

