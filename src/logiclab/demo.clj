(ns logiclab.demo
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))


(comment
  (run* [q]
        (fresh [x y]
               (== x 1)
               (== y 2)
               (== q [x y])
               ))
)
