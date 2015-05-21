(ns genjure.core
  (:gen-class))

(defn crossover
  "Takes the two genotypes [gt1, gt2] and crosses them using the vector
  of crossing points provided."
  ;; FIXME: That function is ugly as hell.
  [gt1 gt2 cross-points]
  (loop [gene (dec (count gt1))
         new-genotype []
         points (reverse (map #(- 19 %) cross-points))
         hold false]
    (if (< gene 0)
      new-genotype
      (if (= gene (last points))
        (recur (dec gene)
               (conj new-genotype (nth gt2 gene))
               (pop points)
               (not hold))
        (if (true? hold)
          (recur (dec gene)
                 (conj new-genotype (nth gt2 gene))
                 points hold)
          (recur (dec gene)
                 (conj new-genotype (nth gt1 gene))
                 points hold))))))


;; TODO: The mimimal swap window size is 2 genes. Maybe in the future that can
;; be improved to support 1 gene swaps.
(defn crossover2
  [genotype1 genotype2 cross-points]
  (loop [gene 0
         new-genotype []
         points cross-points
         hold false]
    (if (= gene (count genotype1))
      (do
        (printf "END - %s = [%s, %s, %s]\n" gene new-genotype points hold)
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



