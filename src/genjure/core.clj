(ns genjure.core
  (:gen-class))


(defn crossover
  "Produces a new vector based on the genotypes [gt1 gt2] taking the mask as
  the crossover guide {1 = copy, 0 = not-copy}. The genes from [gt2] are copyed
  to [gt1]."
  [gt1 gt2 mask]
  (let [inv-mask (mapv #(- 1 %) mask)
        masked-gt1 (mapv * gt1 mask)
        masked-gt2 (mapv * gt2 inv-mask)]
    (mapv + masked-gt1 masked-gt2)))

(crossover [1 2 3 4 5]
           [9 8 7 6 0]
           [0 1 0 1 0])


;; TODO: The mimimal swap window size is 2 genes. Maybe in the future that can
;; be improved to support 1 gene swaps.
;; FIXME: Strange behaviour regarding the hold=false transition. It seems that
;; something is delaying the application of the value from g1->g2.

;; The cross-points are needed to be inside a '(). Ex. (crossover g1 g2 '(2 5))

(defn crossover
  [genotype1 genotype2 cross-points]
  (loop [gene 0
         new-genotype []
         points cross-points
         hold false]
    (if (= gene (count genotype1))
      (do
        (printf "%s = [%s, %s, %s]\n" gene new-genotype points hold)
        new-genotype)
      (if (= gene (first points))
        (do
          (printf "%s = [%s, %s, %s]\n" gene new-genotype points hold)
          (recur (inc gene) (conj new-genotype (nth genotype2 gene)) (pop points) (not hold)))
        (if (true? hold)
          (do
            (printf "%s = [%s, %s, %s]\n" gene new-genotype points hold)
            (recur (inc gene) (conj new-genotype (nth genotype2 gene)) points hold))
          (do
            (printf "%s = [%s, %s, %s]\n" gene new-genotype points hold)
            (recur (inc gene) (conj new-genotype (nth genotype1 gene)) points hold)))))))

(defn generate-genotype
  [length]
  (loop [n 0
         genotype []]
    (if (= n length)
      genotype
      (recur (inc n) (conj genotype (rand-int 10))))))

(defn generate-population
  [size length]
  (loop [n 0
         population []]
    (if (= n size)
      population
      (recur (inc n) (conj population (generate-genotype length))))))

(defn evolve
  [population generation]
  (if (= 0 generation)
    ;; Show best genotype
    (evolve
      (replace-worst
        (evaluate
          (mutation
            (crossover
              (select-best)))))
      (dec generation))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



