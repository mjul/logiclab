(use '[clojure.core.logic minikanren prelude])

(defne moveo [before action after]
  ([[:middle :onbox :middle :hasnot]
    :grasp
    [:middle :onbox :middle :has]])
  ([[?pos :onfloor ?pos ?has]
    :climb
    [?pos :onbox ?pos ?has]])
  ([[?pos1 :onfloor ?pos1 ?has]
    :push
    [?pos2 :onfloor ?pos2 ?has]])
  ([[?pos1 :onfloor ?box ?has]
    :walk
    [?pos2 :onfloor ?box ?has]]))

(defne cangeto [state out]
  ([[_ _ _ :has] true])
  ([_ _] (exist [action next]
           (moveo state action next)
           (cangeto next out))))

(run 1 [q]
  (cangeto [:atdoor :onfloor :atwindow :hasnot] q)) ; (true)