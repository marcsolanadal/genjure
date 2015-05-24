(ns genjure.simple-ga
  (:gen-class))


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


;; The mutation is used to mutate the genotypes of each individual after the
;; breeding of the next generation is done.
;; The function takes the genotype [gt] and goes through all the elmenets of it.
;; In each element a random number is generated. If this number is smaller or
;; equal to the mutation rate the (new-gene) function is called and a new gene
;; is assigned to the element of the genotype.
;; We are passing (new-gene) as a function parameter to abstact the genotypes
;; we are mutating.

(defn mutate
  "Takes a genotype [gt] and modifies it based on the mutation-rate [rate].
  The rate input value must range between 0 and 1. To modify  one of the
  elements it uses the [new-gene] function."
  [gt rate new-gene]
  (loop [n 0 mutated-gt gt]
    (if (= n (count gt))
      mutated-gt
      (if (<= (rand) rate)
        (recur (inc n) (assoc mutated-gt n (new-gene)))
        (recur (inc n) mutated-gt)))))


;; It creates a vector of length [len] filled by the elements returned by the
;; function [new-gene]. This function it is passed by the user in the parameter
;; map of the evolve function.
;; For simple GA the function is alright but it's possible that it will need
;; change in the future with the addition of trees for genetic programming.

(defn new-genotype
  "Generates a new genotype of a given length [len] with the gene structure
  defined in the [new-gene] function."
  [len new-gene]
  (loop [n 0 genotype []]
    (if (= n len)
      genotype
      (recur (inc n) (conj genotype (new-gene))))))


;; The population is created randomly at first using this function. Then it will
;; be evolved using the evolve function. This function will be used at the start
;; of the evolve function to create the first generation.
;; In case of multiple islands we will create different populations for each
;; island.

(defn random-population
  "Generates a population of the specified number of genotypes [size]. Each
  genotype consisting in a chain of genes with length [len].
  The parameter [new-gene] is required for the function (new-genotype)."
  [size len new-gene]
  (loop [n 0
         population []]
    (if (= n size)
      population
      (recur (inc n) (conj population (new-genotype len new-gene))))))


;; In this simple implementation we used stead-state selection, where only the
;; best individuals are selected and breeded. The worst individuals are
;; discardted and substituted for the new ones. We apply that concept using a
;; percent of winners for the tournament, the genotypes below that percent are
;; discarted.
;; We have also applyed the concept of elitism, where the best genotypes are
;; selected and copyed to the next generation.
;; First, it evaluates all the genotypes passed from the parameter [population].
;; Second, it sorts all the genotypes according to it's fitness. That fitness is
;; calculated using the [fitness-function] function from the parameters. We are
;; using vectors, so we need to reverse the vector in order to when we use (pop)
;; we drop the worst genotype not the best.
;; Third, we drop a percent of the total population using the function (pop).
;; The genotypes that are not droped will be the winners of the tournament for
;; this generation.
;; Finally, if the number of genotypes is odd we drop one more genotype to make
;; the number even. This allows us to have a much simplier breed-next-gen
;; function.
;; We should mention that we return the vector of the best genotypes without the
;; fitness value. This is done to simplify the other functions of the algorithm.
;; We also think that insead of slowing down the process that can help speeding
;; it. We think that because the genotypes will likely change a lot and the
;; access to a two level vector several times can be expensive.

(defn tournament
  ""
  [population fitness-function percent-winners]
  (loop [n 0 eval-pop []]
    (if (< n (count population))
      ;; Evaluate all individuals.
        (recur (inc n) (conj eval-pop (fitness-function (nth population n))))
      ;; Sort them according to it's fitness.
        (let [sorted-pop (vec (reverse (sort-by first eval-pop)))
              num-winners (/ (count sorted-pop) (/ 100 percent-winners))]
          (loop [i 0 best-gt sorted-pop]
            (if (< i num-winners)
              (recur (inc i) (pop best-gt)) ;; Here we apply the elitism.
              (if (odd? (count best-gt))
                (mapv second (pop best-gt))
                (mapv second best-gt))))))))


;; TODO: Parametrize the implementation of the mask.
(defn breed-next-gen
  [population]
    (loop [n 0 next-gen population]
      (if (>= n (count population))
        next-gen
        (let [new-gt1 (crossover (nth population n)
                                 (nth population (inc n))
                                 [1 1 1 1 1 0 0 0 0 0])
              new-gt2 (crossover (nth population n)
                                 (nth population (inc n))
                                 [0 0 0 0 0 1 1 1 1 1])]
          (recur (+ n 2) (conj next-gen new-gt1 new-gt2))))))


;; TODO: Implement a map as a list of all the relevant parameters for the
;; configuration of the GA.
(defn evolve
  [population generations]
  (loop [n 0 current-gen population]
    (if (= n generations)
      (first current-gen)
      ;; FIXME: The mutation is performed after the tournament, therefore the
      ;; last result can be a mutated solution not the ideal one.
      (let [selected-gen (steady-state-selection current-gen 2)
            breeded-gen (breed-next-gen selected-gen)
            mutated-gen (mapv #(mutate % 0.1) breeded-gen)]
        (recur (inc n) mutated-gen)))))
