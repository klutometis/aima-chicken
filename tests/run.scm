(use aima
     aima-vacuum
     test)

(debug? #f)

(test
 "Non-penalizing vacuum"
 1999
 (simulate-vacuum
  (make-world dirty clean)
  (make-simple-reflex-agent left)))

(test
 "Penalizing vacuum"
 1000
 (simulate-penalizing-vacuum
  (make-world dirty clean)
  (make-simple-reflex-agent left)))

(test
 "Penalizing vacuum with stateful agent"
 1995
 (simulate-penalizing-vacuum
  (make-world dirty dirty)
  (make-stateful-reflex-agent left)))
