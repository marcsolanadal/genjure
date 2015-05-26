(ns genjure.simple-ga
  (:gen-class))

;; It creates a vector of length [len] filled by the elements returned by the
;; function [gene-function]. This function it is passed by the user in the
;; parameter map of the evolve function.
;; For simple GA the function is alright but it's possible that it will need
;; change in the future with the addition of trees for genetic programming.
;;
;; We implemented two versions of this function. Due that this functions is only
;; used in the creation of the initial random population, we think that some
;; performance can be spared in order to improve readability.
;;
;; Function tested: (new-genotype 100 rand)
;; Tested implementations:
;;    loop/recur    16.32us
;;    idiomatic     46.99us <-- Preferred solution

(defn new-genotype
  "Generates a new genotype of a given length [len] with the gene structure
  defined in the [gene-function] function."
  [len gene-function] (vec (repeatedly len gene-function)))

;; The population is created randomly at first using this function. Then it will
;; be evolved using the evolve function. This function will be used at the start
;; of the evolve function to create the first generation.
;; In case of multiple islands we will create different populations for each
;; island.

(defn random-population
  "Generates a population of the specified number of genotypes [size]. Each
  genotype consisting in a chain of genes with length [len].
  The parameter [gene-function] is required for the function (new-genotype)."
  [size len gene-function]
  (loop [n 0 population []]
    (if (= n size)
      population
      (recur (inc n) (conj population (new-genotype len gene-function))))))

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

;; The following functions are mask generators for the function crossover.
;; They will be passed from the evolve map :mask and :inv-mask key values.
;;
;;    simple-mask           [1 1 1 1 0 0 0 0]
;;    inv-simple-mask       [0 0 0 0 1 1 1 1]
;;    interleaved-mask      [1 0 1 0 1 0 1 0]
;;    inv-interleaved-mask  [0 1 0 1 0 1 0 1]

(defn simple-mask [n] (vec (concat (repeat (/ n 2) 1) (repeat (/ n 2) 0))))
(defn inv-simple-mask [n] (vec (reverse (simple-mask n))))
(defn interleaved-mask [n] (vec (take n (interpose 0 (repeat 1)))))
(defn inv-interleaved-mask [n] (vec (reverse (interleaved-mask n))))

;; The mutation is used to mutate the genotypes of each individual after the
;; breeding of the next generation is done.
;; The function takes the genotype [gt] and goes through all the elmenets of it.
;; In each element a random number is generated. If this number is smaller or
;; equal to the mutation rate the (gene-function) function is called and a new
;; gene is assigned to the element of the genotype.
;; We are passing (gene-function) as a function parameter to abstact the
;; genotypes we are mutating.

(defn mutate
  "Takes a genotype [gt] and modifies it based on the mutation-rate [rate].
  The rate input value must range between 0 and 1. To modify  one of the
  elements it uses the [gene-function] function."
  [gt rate gene-function]
  (loop [n 0 mutated-gt gt]
    (if (= n (count gt))
      mutated-gt
      (if (<= (rand) rate)
        (recur (inc n) (assoc mutated-gt n (gene-function)))
        (recur (inc n) mutated-gt)))))


;; FIXME: This function will be called once each generation. The most expensive
;; function is the fitness-function. It is called for each genotype each
;; generation, so optimizing that function is the priority.

;; TODO: That loop need's to be as parallel as possible.

(defn evaluate
  [population fitness-function]
  (let [pop-size (count population)]
    (loop [n 0 eval-pop []]
      (if (< n pop-size)
        (recur (inc n) (conj eval-pop (fitness-function (nth population n))))
        eval-pop))))

;; In this simple implementation we used stead-state selection, where only the
;; best individuals are selected and breeded. The worst individuals are
;; discardted and substituted for the new ones. We apply that concept using a
;; percent of winners for the tournament, the genotypes below that percent are
;; discarted.
;; We have also applyed the concept of elitism, where the best genotypes are
;; selected and copyed to the next generation.
;; First, it sorts all the genotypes from the [evaluated-population] parameter
;; accordding to it's fitness value. This value is calculated using the
;; [fitness-function] function from the parameters. We are using vectors, so we
;; need to reverse the vector in order to when we use (pop) we drop the worst
;; genotype not the best.
;; Second, we drop a percent of the total population using the function (pop).
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
  "Takes all the genotypes [population] of the current generation and evaluates
  them with the [fitness-function]. The best genotypes are selected based on the
  [percent-winners] value and selected for breeding. The rest are discarted."
  [evaluated-population percent-winners]
  (let [sorted-pop (vec (reverse (sort-by first evaluated-population)))
        num-winners (/ (count sorted-pop) (/ 100 percent-winners))]
    (loop [i 0 best-gt sorted-pop]
      (if (< i num-winners)
        (recur (inc i) (pop best-gt)) ;; Here we apply elitism.
        (if (odd? (count best-gt))
          (mapv second (pop best-gt))
          (mapv second best-gt))))))

;; We breed the next generation using the best genotypes of the current
;; generation. We generate two genotypes from each pair of genotypes. We use the
;; mask in the parameters to do the crossovers between genotypes. Notices that
;; one mask is the inverse of the other. We pass the masks from the parameters
;; because we don't want to calculate them inside the loop. This is for
;; performance reasons.

(defn breed-next-gen
  "Generates the next generation based on the current one [population]. It takes
  a [mask] and [inv-mask] to do the crossover of genotypes."
  [population mask inv-mask]
  (loop [n 0 next-gen population]
    (if (>= n (count population))
      next-gen
      (let [new-gt1 (crossover (nth population n)
                               (nth population (inc n))
                               mask)
            new-gt2 (crossover (nth population n)
                               (nth population (inc n))
                               inv-mask)]
        (recur (+ n 2) (conj next-gen new-gt1 new-gt2))))))


;; TODO: Think a way to implement epoch for the async island method.
(defn evolve
  ""
  [parameters-map]
  (let [{size :population-size gt-len :genotype-length generations :generations
         gene-function :gene-function fitness-function :fitness-function
         mask :mask inv-mask :inv-mask xcent :tournament-percent-selected
         mut-prov :mutation-provability} parameters-map
        population (random-population size gt-len gene-function)]

    ;; Looping through all generations
    (loop [n 0 current-gen population]
      (if (= n generations)
        (first current-gen)
        (let [evaluated-gen (evaluate current-gen fitness-function)
              selected-gen (tournament evaluated-gen xcent)
              breeded-gen (breed-next-gen selected-gen mask inv-mask)
              mutated-gen (mapv #(mutate % mut-prov gene-function) breeded-gen)]
          (if (= n (- generations 1))
            (recur (inc n) selected-gen)
            (recur (inc n) mutated-gen)))))))




