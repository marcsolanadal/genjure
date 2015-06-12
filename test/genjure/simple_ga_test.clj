(ns genjure.simple-ga-test
  (:use midje.sweet)
  (:require [genjure.simple-ga :refer :all]
            [genjure.misc :refer :all]))

(facts "`generate-genotype`"
       (fact "returns a vector."
             (vector? (new-genotype 2 rand)) => true
             (vector? (new-genotype 0 1)) => true)
       (fact "has the desired size."
             (count (new-genotype 10 rand)) => 10
             (count (new-genotype 300 rand)) => 300))

(facts "`random-population`"
       (fact "returns a vector of vectors."
             (vector? (first (random-population 10 2 rand))) => true
             (vector? (last (random-population 10 2 rand))) => true)
       (fact "has the desired population size."
             (count (random-population 10 2 rand)) => 10
             (count (random-population 50 2 rand)) => 50)
       (fact "the genotypes have the correct size."
             (count (first (random-population 10 3 rand))) => 3
             (count (first (random-population 10 100 rand))) => 100))

(facts "`simple-mask`"
       (fact "generates a mask of length n."
             (count (simple-mask 10)) => 10
             (count (simple-mask 5)) => 6) ; This case is strange
       (fact "has a one in (n/2)-1 and a zero in n/2."
             (nth (simple-mask 10) 4) => 1
             (nth (simple-mask 10) 5) => 0)
       (fact "has ones and zeros in the rest of positions."
             (simple-mask 10) => [1 1 1 1 1 0 0 0 0 0]
             (simple-mask 5) => [1 1 1 0 0 0]))

(def gt1 [1 2 3 4])
(def gt2 [4 3 2 1])

(facts "`crossover`"
       (fact "returns a vector of the same size of genotype."
             (count (crossover gt1 gt2 [1 1 0 0])) => 4
             (count (crossover gt1 gt2 [2 2 0 0])) => 4) ; 2 Not allowed
       (fact "the ones in the mask are from the gt2 in the child gt."
             (first (crossover gt1 gt2 [1 1 0 0])) => (first gt2)
             (last (crossover gt1 gt2 [0 0 1 1])) => (last gt2))
       (fact "the zeroes in the mask are from the gt2 in the new gt."
             (first (crossover gt1 gt2 [0 0 1 1])) => (first gt1)
             (last (crossover gt1 gt2 [1 1 0 0])) => (last gt1)))

;; Seed fixing to avoid problems when testing.
(def rng (java.util.Random. 10000))

;; Mutation detection functions
(defn mutation? [gt1 gt2]
  (if (not= gt1 gt2) true false))

(defn detect-mutation [gt p]
  (mutation? gt (local-mutate gt p (. rng nextDouble))))

(facts "`mutate`"
       (fact "mutates the genotype with a given provability."
             (mean 1000 (detect-mutation gt1 0.0)) => 0.0
             (mean 1000 (detect-mutation gt1 0.3)) => 0.3
             (mean 1000 (detect-mutation gt1 0.5)) => 0.5
             (mean 1000 (detect-mutation gt1 0.6)) => 0.6
             (mean 1000 (detect-mutation gt1 1.0)) => 1.0))



