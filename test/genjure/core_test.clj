(ns genjure.core-test
  (:require [clojure.test :refer :all]
            [genjure.core :refer :all]))

(deftest genotype-test
  (testing "Genotype"
    (is
      (= 10 (count (generate-genotype 10)))
      "should contain 10 genes.")
    (is
      ;; FIXME: This test will fail sometimes.
      (not= (generate-genotype 10) (generate-genotype 10))
      "two genotypes should be generated randomly.")))

(deftest population-test
  (testing "Population"
    (is
      (= 10 (count (nth (generate-population 50 10) 30)))
      "should contain genotypes with 10 genes.")
    (is
      (= 100 (count (generate-population 100 10)))
      "should have 100 genotypes.")
    (is
      (vector? (generate-population 100 10))
      "should be a vector of genotypes.")
    (is
      (vector? (nth (generate-population 50 5) (rand-int 49)))
      "should contain genotype vectors.")))

(deftest crossover-test
  (testing "Crossover"
    (let [g1 [1 2 3 4 5 6 7 8 9 0]
          g2 [0 9 8 7 6 5 4 3 2 1]]
      (is
        (vector? (crossover g1 g2 [1 1 1 1 1 0 0 0 0 0]))
        "should return a new vector genotype.")
      (is
        (= [0 9 8 7 6 6 7 8 9 0] (crossover g1 g2 [1 1 1 1 1 0 0 0 0 0]))
        "should be able to cross genotypes with one cross-point.")
      (is
        (= [0 9 3 4 6 5 4 8 2 0] (crossover g1 g2 [1 1 0 0 1 1 1 0 1 0]))
        "should be able to cross genotypes with multiple cross-points."))))





