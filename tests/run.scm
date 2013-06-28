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

(parameterize ((current-test-epsilon 0.1))
  (test
   "Randomized graph agent on 100 different worlds"
   17300.0
   (let* ((scores
           (list-tabulate
            100
            (lambda (i)
              (let* ((world (make-graph-world))
                     (start (random-start world))
                     (agent (make-randomized-graph-agent start)))
                (parameterize ((random-seed i)
                               (debug? #f))
                  (simulate-graph world agent))
                (agent-score agent))))))
     (/ (apply + scores) (length scores))))
  
  (test
   "Stateful graph agent on 100 different worlds"
   19176.35
   (let* ((scores
           (list-tabulate
            100
            (lambda (i)
              (let* ((world (make-graph-world))
                     (start (random-start world))
                     (agent (make-stateful-graph-agent start)))
                (parameterize ((random-seed i)
                               (debug? #f))
                  (simulate-graph world agent))
                (agent-score agent))))))
     (/ (apply + scores) (length scores)))))

(parameterize ((current-test-epsilon 0.1))
  (test
   "Randomized graph agent on a linear world 100 times"
   15000.0
   (let* ((world (make-linear-world))
          (start (random-start world)))
     (let ((scores (list-tabulate
                    100
                    (lambda (i)
                      (let* ((world (copy-world world))
                             (agent (make-randomized-graph-agent start)))
                        (parameterize ((debug? #f))
                          (simulate-graph world agent))
                        (agent-score agent))))))
       (/ (apply + scores) (length scores))))))

(test-exit)
