(ns logiclab.queens
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; Solve the "N queens problem"
;; How can we place N queens on a chessboard in a configuration
;; where no two queens attack each other.

;; We represent a queen by its coordinates,
;; [file rank] e.g. "A1" is [1 1], is "H8" [8 8]

(defne fileo
  "A relation where f is the file of the given queen."
  [f queen]
  ([_ [f _]]))

(defne ranko 
  "A relation where r is the rank of the given queen."
  [r queen]
  ([_ [_ r]]))


(defne attackso
  "A relation between two queens q1 and q2, where they attack each other."
  [q1 q2]
  ([_ _]
     ;; same rank
     (fresh [r]
            (ranko r q1)
            (ranko r q2)))
  ([_ _]
     ;; same file
     (fresh [f]
            (fileo f q1)
            (fileo f q2)))
  ([[f1 r1] [f2 r2]]
     ;; same diagonal
     (fresh [dist]
            (project [f1 f2] (== dist (Math/abs (- f2 f1))))
            (project [r1 r2] (== dist (Math/abs (- r2 r1)))))))


;; TODO:::

(defn queens
  "A relation where N queens do not attack each other on an NxN board."
  [n]
  (let [positions (range 1 (inc n))]
    (run* [queen]
          (fresh [q r f]
                 (== q [f r])
                 (membero r positions)
                 (membero f positions)
                 (== queen q)))))

               