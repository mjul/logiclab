;; From Ambrose Bonnaire-Sergeant (Clojure Google group)
(defn distincto [l] 
  (conde 
    [(== l ())] 
    [(fresh [a] (== l [a]))] 
    [(fresh [a ad dd] 
            (== l (llist a ad dd)) 
            (!= a ad) 
            (distincto (llist a dd)) 
            (distincto (llist ad dd)))])) 
(run* [q] 
      (fresh [a b c] 
             (membero a [1 2 3]) 
             (membero c [1 2 3]) 
             (== b 2) 
             (distincto [a b c]) 
             (== q [a b c]))) 
;=> ([1 2 3] [3 2 1]) 