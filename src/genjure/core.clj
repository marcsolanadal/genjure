(ns genjure.core
  (:gen-class))

;; This will define the encoding of the hole GA.
;; If we want to solve simple GA we should use binary encoding, permutation
;; encoding or value encoding.
;;
;; Binary encoding = [1 1 1 0 0]
;; Permutation encoding = [4 6 3 9 7 5]
;; Value encoding = [red black red red black]

;; For mor advanced uses such as GP we should use tree encoding.
;;
;; Tree encoding = (+ (- 2 4) 5)

;;(defn new-gene [] (* (rand) 100))

(defn new-gene [] (rand-int 10))

;; Simple fitness function for testing
(defn fitness-function [gt] [(apply * gt) gt])


;; This crossover function is independent of the encoding we are using.
;; It has the strenght of being able to change the mask vector. If we do so, we
;; can explore how different crossover points affect the performance of our GA.

(defn crossover
  "Produces a new vector based on the genotypes [gt1 gt2] taking the mask as
  the crossover guide {1 = copy, 0 = not-copy}. The genes from [gt2] are copyed
  to [gt1]."
  [gt1 gt2 mask]
  (let [inv-mask (mapv #(- 1 %) mask)
        masked-gt1 (mapv * gt1 inv-mask)
        masked-gt2 (mapv * gt2 mask)]
    (mapv + masked-gt1 masked-gt2)))

(defn mutate
  "Takes a genotype [gt] and modifies it based on the mutation-rate [rate].
  The rate input value must range between 0 and 1."
  [gt rate]
  (loop [n 0 mutated-gt gt]
    (if (= n (count gt))
      mutated-gt
      (if (<= (rand) rate)
        (recur (inc n) (assoc mutated-gt n (new-gene)))
        (recur (inc n) mutated-gt)))))

(defn generate-genotype
  "Generates a new genotype of a given length [len] with the gene structure
  defined in the (new-gene) function."
  [len]
  (loop [n 0 genotype []]
    (if (= n len)
      genotype
      (recur (inc n) (conj genotype (new-gene))))))

(defn generate-population
  "Generates a population of the specified number of genotypes [size]. Each
  genotype consisting in a chain of genes with length [len]."
  [size len]
  (loop [n 0
         population []]
    (if (= n size)
      population
      (recur (inc n) (conj population (generate-genotype len))))))

;; Steady-state selection
;; ----------------------
;; Only the best individuals are selected and breeded. The worst individuals are
;; are discardted and substituted for the new ones.

;; Rulette wheel selection
;; -----------------------
;; The better chromosomes are, the more chances to be selected they have.
;; This method have problems when the fitnesses differs very much.

;; Rank selection
;; --------------
;; First all population is ranked with his fitness function. Then, the worst
;; individuals are assigned with the lower fitness and the better individuals
;; are assigned with the greater fitness.
;;
;; Ex. Worst individual = 1
;;     Second worst infividual = 2
;;     Best indivudual = n

;; Elitism
;; -------
;; The best chromosomes are selected and copyed to the next generation.
;; This will be applyied before every crossover and mutation.


;; Return the best individuals for breeding without the fitness value.
;; TODO: Improve performance not calculating the fitness each time.
;; Provably the genotypes will change a lot and it's not worth it to
;; implement that optimization. Without it, the code is much cleaner.
;;(mapv second (filter #(<= threshold (first %)) sorted-pop))
(defn steady-state-selection
  [population threshold]
  (loop [n 0 eval-pop []]
    (if (< n (count population))

      ;; Evaluate all individuals.
      (do
        ;;(printf "%s - %s\n" n eval-pop)
        (recur (inc n) (conj eval-pop (fitness-function (nth population n)))))

      ;; Sort them according to it's fitness.
      (do
        (let [sorted-pop (vec (reverse (sort-by first eval-pop)))
              num-gt (count sorted-pop)]

          ;; FIXME: I have a problem with the types. population is a vector but
          ;; when (sort-by first eval-pop) is applyed is converted into a seq.
          ;; Something is not going well when I (pop best-gt) in the loop below.

          ;;(printf "\teval-pop: %s\n" eval-pop)
          ;;(printf "\tnum-gt: %s\n" num-gt)
          ;;(printf "\tsorted-pop: %s\n" sorted-pop)

          (loop [i 0 best-gt sorted-pop]
            (if (< i (/ num-gt 2))
              (recur (inc i) (pop best-gt))
              (if (odd? (count best-gt))
                (mapv second (pop best-gt))
                (mapv second best-gt)))))))))

(defn breed-next-gen
  [population]
    (loop [n 0 next-gen population] ;; Here we apply the concept of elitism.
      (if (>= n (count population))
        next-gen
        (let [new-gt1 (crossover (nth population n)
                                 (nth population (inc n))
                                 [1 1 1 1 1 0 0 0 0 0])
              new-gt2 (crossover (nth population n)
                                 (nth population (inc n))
                                 [0 0 0 0 0 1 1 1 1 1])]
          (recur (+ n 2) (conj next-gen new-gt1 new-gt2))))))

(defn evolve
  [population generations]
  (loop [n 0 current-gen population]
    (if (= n generations)
      (first current-gen)
      (let [selected-gen (steady-state-selection current-gen 2)
            breeded-gen (breed-next-gen selected-gen)
            mutated-gen (mapv #(mutate % 0.1) breeded-gen)]
        ;;(printf "current-gen: %s\n" current-gen)
        ;;(printf "\tselected-gen: %s\n" selected-gen)
        ;;(printf "\tbreeded-gen: %s\n" breeded-gen)
        ;;(printf "\tmutated-gen: %s\n" mutated-gen)
        (recur (inc n) mutated-gen)))))



(evolve (generate-population 500 10) 200)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (use 'criterium.core)

  ;; Sample genotypes for performance testings
  (def gt1 (generate-genotype 10))
  (def gt2 (generate-genotype 10))

  ;; (with-progress-reporting (bench
  ;;                          (crossover gt1 gt2 [1 1 1 0 0 1 0 1 0 0])
  ;;                           :verbose))
  ;; 25.55us

  ;; (with-progress-reporting (bench
  ;;                           (mutate [1 1 1 1 1 1 1 1 1 1] 0.1)
  ;;                           :verbose))
  ;; 1.52us

  ;;(defn generate-genotype1
  ;;  [len]
  ;;  (mapv #(* % (new-gene)) (vec (replicate len 1))))

  ;; (with-progress-reporting (bench (generate-genotype 1000) :verbose))
  ;; 542us

  ;; (with-progress-reporting (bench (generate-genotype2 10) :verbose))
  ;; 2.85us

)



