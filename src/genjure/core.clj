(ns genjure.core
  (:require [genjure.simple-ga :refer :all])
  (:gen-class))

(use 'criterium.core)

;; This example functions find the greatest combination of 10 numbers.
(defn gene-fn [] (rand))
(defn fitness-fn [gt] [(apply * gt) gt])

(defn -main

  "I don't do a whole lot ... yet."
  [& args]

  (def confmap {:population-size 1000
                :genotype-length 30
                :generations 300
                :gene-function gene-fn
                :fitness-function fitness-fn
                :mask (simple-mask 10)
                :inv-mask (inv-simple-mask 10)
                :tournament-percent-selected 50
                :mutation-provability 0.01})

  (/ (apply + (evolve confmap)) 10)

  (def p1 (random-population  10 2 rand))
  (def ep1 (evaluate p1 #(apply * %)))
  (println ep1)
  (tournament-selection ep1 4)

  (with-progress-reporting (bench
                             ;;(truncation-selection ep1 4) ;; 247.42 us
                             ;;(tournament-selection ep1 4) ;; 5.07 us
                             ;;(rank-selection ep1 4 0.6) ;; 5.982 us
                             ;;(random-population2 1000 10 rand)
                             :verbose))

)



