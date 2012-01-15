(ns logiclab.zebra
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; Zebra Puzzle, see http://en.wikipedia.org/wiki/Zebra_Puzzle

;; The Englishman lives in the red house.
;; The Spaniard owns the dog.
;; Coffee is drunk in the green house.
;; The Ukrainian drinks tea.
;; The green house is immediately to the right of the ivory house.
;; The Old Gold smoker owns snails.
;; Kools are smoked in the yellow house.
;; Milk is drunk in the middle house.
;; The Norwegian lives in the first house.
;; The man who smokes Chesterfields lives in the house next to the man with the fox.
;; Kools are smoked in the house next to the house where the horse is kept. (should be "... a house ...", see Discussion section)
;; The Lucky Strike smoker drinks orange juice.
;; The Japanese smokes Parliaments.
;; The Norwegian lives next to the blue house.

;; Who owns the zebra?

;; First some helper goals.

;; defne does pattern matching, trying all goals (disjunction):
;; You can think of the "e" suffix in core.logic as "every" (disjunction)
(defne right-of-o
  "Goal defining that x and y are adjacent, with y to the right of x in the list l."
  [x y l]
  ([_ _ [x y . _]]) ; either the list begins with x y followed by anything
  ([_ _ [_ . r]] (right-of-o x y r))) ; or x y is in the tail of the list 

(defn next-to-o
  "Goal defining that x is next to y in the list l."
  [x y l]
  (conde
   ((right-of-o x y l))
   ((right-of-o y x l))))


;; We represent the problem as a solution to finding the configuration
;; of the five houses, each represented as a vector of their
;; [colour, nationality, drink, smoke, pet]

(defn puzzle-o
  [houses]
  (fresh [h1 h2 h3 h4 h5]
         (== houses [h1 h2 h3 h4 h5])
         (membero [:red :englishman (lvar) (lvar) (lvar)] houses)
         (membero [(lvar) :spaniard (lvar) (lvar) :dog] houses)
         (membero [:green (lvar) :coffee (lvar) (lvar)] houses)
         (membero [(lvar) :ukrainian :tea (lvar) (lvar)] houses)
         (right-of-o [:ivory (lvar) (lvar) (lvar) (lvar)] [:green (lvar) (lvar) (lvar) (lvar)] houses)
         (membero [(lvar) (lvar) (lvar) :old-gold :snails] houses)
         (membero [:yellow (lvar) (lvar) :kools (lvar)] houses)
         (== [(lvar) (lvar) :milk (lvar) (lvar)] h3)
         (== [(lvar) :norwegian (lvar)  (lvar) (lvar)] h1)
         (next-to-o [(lvar) (lvar) (lvar) :chesterfields (lvar)] [(lvar) (lvar) (lvar) (lvar) :fox] houses)
         (next-to-o [(lvar) (lvar) (lvar) :kools (lvar)] [(lvar) (lvar) (lvar) (lvar) :horse] houses)
         (membero [(lvar) (lvar) :orange-juice :lucky-strike (lvar)] houses)
         (membero [(lvar) :japanese (lvar) :parliament (lvar)] houses)
         (next-to-o [(lvar) :norwegian (lvar) (lvar) (lvar)] [:blue (lvar) (lvar) (lvar) (lvar)] houses)
         ;; For clarification, add that someone drinks water, and someone has a zebra:
         (membero [(lvar) (lvar) :water (lvar) (lvar)] houses)
         (membero [(lvar) (lvar) (lvar) (lvar) :zebra] houses)))


(defn pet-o [house pet]
  "Goal defining that the house has the given pet."
  (matche [house]
          ([[?colour ?nationality ?drink ?smoke ?pet]]
             (== ?pet pet))))

(defn nationality-o [house nationality]
  "Goal for the nationality of a house owner."
  (matche [house]
          ([[?colour ?nationality ?drink ?smoke ?pet]]
             (== ?nationality nationality))))


(defn zebra-owner
  []
  (run* [q]
        (fresh [houses
                colour nationality drink smoke pet]
               (puzzle-o houses)
               (== pet :zebra)
               (membero [colour nationality drink smoke pet] houses)
               (== q nationality))))

(defn zebra-owner-with-goals
  []
  (run* [q]
        (fresh [house houses]
               (puzzle-o houses)
               (membero house houses)
               (pet-o house :zebra)
               (nationality-o house q))))

(defn water-drinker
  []
  (run* [q]
        (fresh [houses]
               (puzzle-o houses)
               (fresh [colour nationality drink smoke pet]
                      (== drink :water)
                      (membero [colour nationality drink smoke pet] houses)
                      (== q nationality)))))