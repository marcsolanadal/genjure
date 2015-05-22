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

(defn new-gene [] (* (rand) 1))


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

(defn tournament-selection
  [population size p]
  (loop [n 0 contenders []]
    (if (= n size)
      (let [p (rand)]
        (cond
          ()
          )
        )

      )
    )
  ;; Select random individuals
  )

(defn evolve
  [population generation]
  (if (= 0 generation)
    ;; Show best genotype
    (evolve
      (replace-worst
        (evaluate
          (mutation
            (crossover
              (tournament-selection)))))
      (dec generation))))


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



