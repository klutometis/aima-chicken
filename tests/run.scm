(use aima-vacuum
     test)

(test
 "Non-penalizing vacuum"
 1999
 (simulate-vacuum (make-world dirty clean)
                  (make-reflex-agent left)))

(test
 "Penalizing vacuum"
 1000
 (simulate-penalizing-vacuum
  (make-world dirty clean)
  (make-reflex-agent left)))
