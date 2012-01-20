(ns logiclab.test.queens
  (:refer-clojure :exclude [==])
  (:use [logiclab.queens :reload :all]
        [clojure.core.logic :only [run* fresh ==]]
        [clojure.test]))


(deftest ranko-tests
  (testing "The rank goal should match the rank"
    (is (= [2] (run* [q] (ranko q [1 2]))))
    (is (= [2] (run* [q] (ranko 2 [1 q]))))))

(deftest fileo-tests
  (testing "The file goal should match the file."
    (is (= [1] (run* [q] (fileo q [1 2]))))
    (is (= [1] (run* [q] (fileo 1 [q 2]))))))

(deftest attacks-tests
  (letfn [(run-attackso [q1 q2]
            (run* [q] (attackso q1 q2)))]
    (testing "queens in the same file should attack each other."
      (is (seq (run-attackso [1 1] [1 3]))))
    (testing "queens in the same rank should attack each other."
      (is (seq (run-attackso [1 1] [3 1]))))
    (testing "queens in the same diagonal should attack each other."
      (are [q1 q2] (seq (run-attackso q1 q2))
           [1 1] [2 2]
           [1 1] [3 3]
           [2 1] [1 2]
           [2 1] [3 2]))))


(deftest n-queens-tests
  (testing "1x1 board has a single solution."
    (is (= [1 1] (queens 1))))
  (testing "2x2 board has no solutions."
    (is (not (seq (queens 2)))))
  (testing "3x3 board has no solutions."
    (is (not (seq (queens 3))))))






