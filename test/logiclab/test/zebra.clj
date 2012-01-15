(ns logiclab.test.zebra
  (:refer-clojure :exclude [==])
  (:use [logiclab.zebra :reload :all]
        [clojure.core.logic :only [run* fresh ==]]
        [clojure.test]))

(defn- run-right-of-o
  [x y l]
  (run* [q]
        (== q [x y])
        (right-of-o x y l)))

(deftest right-of-o-tests
  (testing "There should never be any solutions in the empty list"
    (is (empty? (run* [q]
                      (fresh [x y]
                             (== q [x y])
                             (right-of-o x y []))))))
  (testing "It should recognize adjacent values"
    (is (= (list [1 2]) (run-right-of-o 1 2 [1 2 3])))
    (is (= (list [2 3]) (run-right-of-o 2 3 [1 2 3]))))
  (testing "It should reject non-adjacent values"
    (is (empty? (run* [q] (right-of-o 1 3 [1 2 3]))))))


(defn- run-next-to-o
  [x y l]
  (run* [q]
        (== q [x y])
        (next-to-o x y l)))

(deftest next-to-o-tests
  (testing "There should never by any solutions in the empty list."
    (is (empty? (run* [q]
                      (fresh [x y]
                             (== q [x y])
                             (next-to-o x y []))))))
  (testing "It should recognize values to the left and right"
    (is (= (list [1 2]) (run-next-to-o 1 2 [1 2 3])))
    (is (= (list [2 1]) (run-next-to-o 2 1 [1 2 3])))))
    

(deftest pet-o-tests
  (testing "Identify pets"
    (is [:pet] (run* [q] (pet-o [:colour :nationality :drink :smoke :pet] q)))
    (is [:zebra] (run* [q] (pet-o [:colour :nationality :drink :smoke q] :zebra)))))

(deftest nationality-o-tests
  (testing "Identify nationality"
    (is [:nationality] (run* [q] (pet-o [:colour :nationality :drink :smoke :pet] q)))
    (is [:danish] (run* [q] (pet-o [:colour q :drink :smoke :pet] :danish)))))


(deftest zebra-owner-tests
  (testing "Who owns the zebra?"
    (is (== :japanese (zebra-owner)))))

(deftest water-drinker-tests
  (testing "Who drinks water?"
    (is (== :norwegian (water-drinker)))))