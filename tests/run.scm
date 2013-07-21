(use aima
     aima-csp
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

(define arc-consistent-coloring
  (make-csp
   ;; Domain can also be lambdas?
   (alist->hash-table '((a . (white black))
                        (b . (white black))))
   (alist->hash-table `(((a . b) . ,neq?)
                        ((b . a) . ,neq?)))
   (alist->hash-table '((a b)
                        (b a)))))

(define arc-inconsistent-coloring
  (make-csp
   ;; Domain can also be lambdas?
   (alist->hash-table '((a . (white))
                        (b . (white))))
   (alist->hash-table `(((a . b) . ,neq?)
                        ((b . a) . ,neq?)))
   (alist->hash-table '((a b)
                        (b a)))))

(define 3-colors '(red green blue))

;;; Could find a mechanism for automatically creating these things;
;;; indeed, will have to randomly.
(define 3-color-australia
  (make-csp
   (alist->hash-table `((wa . ,3-colors)
                        (nt . ,3-colors)
                        (sa . ,3-colors)
                        (q . ,3-colors)
                        (nsw . ,3-colors)
                        (v . ,3-colors)
                        (t . ,3-colors)))
   (alist->hash-table `(((wa . nt) . ,neq?)
                        ((nt . wa) . ,neq?)
                        ((wa . sa) . ,neq?)
                        ((sa . wa) . ,neq?)
                        ((nt . sa) . ,neq?)
                        ((sa . nt) . ,neq?)
                        ((nt . q) . ,neq?)
                        ((q . nt) . ,neq?)
                        ((sa . q) . ,neq?)
                        ((q . sa) . ,neq?)
                        ((nsw . q) . ,neq?)
                        ((q . nsw) . ,neq?)
                        ((nsw . v) . ,neq?)
                        ((v . nsw) . ,neq?)
                        ((sa . nsw) . ,neq?)
                        ((nsw . sa) . ,neq?)
                        ((sa . v) . ,neq?)
                        ((v . sa) . ,neq?)))
   (alist->hash-table '((wa nt sa)
                        (nt wa sa)
                        (sa wa nt q nsw v)
                        (q nt sa nsw)
                        (nsw q sa v)
                        (v nsw sa)
                        (t)))))

(define 2-colors '(red green))

;;; Could find a mechanism for automatically creating these things;
;;; indeed, will have to randomly.
(define 2-color-australia
  (make-csp
   (alist->hash-table `((wa . ,2-colors)
                        (nt . ,2-colors)
                        (sa . ,2-colors)
                        (q . ,2-colors)
                        (nsw . ,2-colors)
                        (v . ,2-colors)
                        (t . ,2-colors)))
   (alist->hash-table `(((wa . nt) . ,neq?)
                        ((nt . wa) . ,neq?)
                        ((wa . sa) . ,neq?)
                        ((sa . wa) . ,neq?)
                        ((nt . sa) . ,neq?)
                        ((sa . nt) . ,neq?)
                        ((nt . q) . ,neq?)
                        ((q . nt) . ,neq?)
                        ((sa . q) . ,neq?)
                        ((q . sa) . ,neq?)
                        ((nsw . q) . ,neq?)
                        ((q . nsw) . ,neq?)
                        ((nsw . v) . ,neq?)
                        ((v . nsw) . ,neq?)
                        ((sa . nsw) . ,neq?)
                        ((nsw . sa) . ,neq?)
                        ((sa . v) . ,neq?)
                        ((v . sa) . ,neq?)))
   (alist->hash-table '((wa nt sa)
                        (nt wa sa)
                        (sa wa nt q nsw v)
                        (q nt sa nsw)
                        (nsw q sa v)
                        (v nsw sa)
                        (t)))))

(test-assert "Arc-consistency"
             (ac-3 arc-consistent-coloring))

(test-assert "Arc-inconsistency"
             (not (ac-3 arc-inconsistent-coloring)))

(test "Arc-consistent coloring"
      '((b . white) (a . black))
      (hash-table->alist (backtracking-search arc-consistent-coloring)))

(test-assert "Arc-inconsistent coloring"
             (failure? (backtracking-search arc-inconsistent-coloring)))

(test-assert "3-color Australia search"
             (success? (backtracking-search 3-color-australia)))

(test-assert "3-color Australia enumeration"
             (success? (backtracking-enumeration 3-color-australia)))

(test "2-color Australia failure"
      '()
      (backtracking-enumeration 2-color-australia))

(test-exit)
