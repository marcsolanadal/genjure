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

  (def confmap {:population-size 250
                :genotype-length 10
                :generations 50
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
                             (rank-selection ep1 4)
                             :verbose))

  ;; Sample genotypes for performance testings
  ;;(def gt1 (generate-genotype 10))
  ;;(def gt2 (generate-genotype 10))

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



