(use aima-vacuum
     test)

(test
 1999
 (simulate-vacuum (make-world dirty clean)
                  (make-reflex-agent left)))
