(ns genjure.core
  (:require [genjure.simple-ga :refer :all])
  (:gen-class))


(defn new-gene [] (rand-int 10))
(defn fitness-function [gt] [(apply * gt) gt])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (use 'criterium.core)

  (evolve :population-size 500
          :genotype-lenght 10
          :gene-function 'new-gene
          :fitness-function 'fitness-fn
          :tournament-xcent-selected 50
          :crossover-type "half/interleaved"
          :mutation-rate 0.01)

  (evolve (generate-population 500 10) 200)

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



