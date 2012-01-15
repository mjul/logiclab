(ns logiclab.core
  (:refer-clojure :exclude [inc reify ==])
  (:require (clojure (walk :as w)))
  (:use [clojure.core.logic]))

;; Logic functions

(run* [q]
      (typedo [['f :- [Integer :> Integer]] ;; 'f is of type Integer -> Integer
               ['g :- Integer]              ;; 'g is of type Integer
               ] 
              [:apply 'f 'g] ;; Determine the resulting type of ('f 'g) ..
              Integer)       ;;  and succeed if it is Integer
      )
