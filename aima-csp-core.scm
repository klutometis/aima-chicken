(define-record-and-printer unassigned)
(define unassigned (make-unassigned))
(define assigned? (complement unassigned?))

(define-record-and-printer failure)
(define failure
  @("The failure object: to distinguish ''bona-fide'' solutions to a
CSP that are {{#f}}.")
  (make-failure))

(define-record-and-printer csp
  @("A constraint-satisfaction-problem"
    (domains "A hash-table mapping variables to possible values")
    (constraints "A hash-table mapping pairs of variables to a dyadic
lambda which returns {{#f}} if the values don't satisfy the constraint")
    (neighbors "A hash-table adjacency-list of constraints")
    (@example-no-eval "A trivial (but inconsistent) CSP"
                      (define arc-inconsistent-coloring
                        (make-csp
                         ;; Domain can also be lambdas?
                         (alist->hash-table '((a . (white))
                                              (b . (white))))
                         (alist->hash-table `(((a . b) . ,neq?)
                                              ((b . a) . ,neq?)))
                         (alist->hash-table '((a b)
                                              (b a)))))))
  domains
  constraints
  neighbors)

(define (csp-copy csp)
  (make-csp (hash-table-copy (csp-domains csp))
            (hash-table-copy (csp-constraints csp))
            (hash-table-copy (csp-neighbors csp))))

;;; Dispense with this and do equality on the number of keys!
(define (make-assignment csp)
  (alist->hash-table
   (map (lambda (variable) (cons variable unassigned))
        (hash-table-keys (csp-domains csp)))))

(define (backtracking-search csp)
  @("Find a solution to the CSP or return {{failure}}."
    (csp "The CSP to solve")
    (@to "object or {{failure}}")
    (@example-no-eval "A trivial 2-coloring problem"
                      (define arc-consistent-coloring
                        (make-csp
                         ;; Domain can also be lambdas?
                         (alist->hash-table '((a . (white black))
                                              (b . (white black))))
                         (alist->hash-table `(((a . b) . ,neq?)
                                              ((b . a) . ,neq?)))
                         (alist->hash-table '((a b)
                                              (b a)))))
                      (test "Arc-consistent coloring"
                            '((b . white) (a . black))
                            (hash-table->alist (backtracking-search arc-consistent-coloring)))))
  (let ((enumeration (backtracking-enumeration 1 csp)))
    ;; Return #f here? No, need to distinguish between failure and the
    ;; legitimate value #f.
    (if (null? enumeration) failure (car enumeration))))

(define (complete? assignment)
  (hash-table-fold
   assignment
   (lambda (variable value complete?)
     (and (assigned? value) complete?))
   #t))

;;; Too bad this is linear across variables, right? Optimize later.
;;; 
;;; Need the CSP, at some point, to do the degree heuristic.
(define (select-unassigned-variable assignment)
  ;; We can assume there is at least one unassigned variable.
  (car (find (compose unassigned? cdr) (hash-table->alist assignment))))

;;; Need assignment, at some point, to do least-constraining-value.
(define (order-domain-values variable csp)
  (hash-table-ref (csp-domains csp) variable))

;;; Find the assigned neighbors of the variable; does the value
;;; satisfy each constraint?
;;;
;;; What if the variable is already assigned something else? Seems
;;; like a pathological case.
;;;
;;; Should we check here if we've already assigned something?
(define (consistent? variable value assignment csp)
  ;; Use the default case when there are no neighbors.
  (let* ((neighbors (hash-table-ref/default (csp-neighbors csp) variable '()))
         (assigned-neighbors (filter (lambda (neighbor) (assigned? (hash-table-ref assignment neighbor)))
                                     neighbors)))
    (every values (map (lambda (neighbor) ((hash-table-ref (csp-constraints csp) (cons variable neighbor))
                                      value
                                      (hash-table-ref assignment neighbor)))
                       assigned-neighbors))))

(define (inference csp variable value)
  (make-hash-table))

(define backtracking-enumeration
  @("Enumerate up to {{n}} solutions of the {{csp}}; enumerate all if {{n}}
is {{#f}} or unspecified."
    (n "Enumerate up to {{n}} solutions")
    (csp "The CSP to solve")
    (@to "list"))
  (case-lambda
   ((csp) (backtracking-enumeration #f csp))
   ((n csp) (let ((enumeration (make-parameter '())))
              (backtrack-enumerate n enumeration (make-assignment csp) csp)
              (enumeration)))))

(define (backtrack-enumerate n enumeration assignment csp)
  (if (complete? assignment)
      (enumeration (cons assignment (enumeration)))
      (let ((variable (select-unassigned-variable assignment)))
        (let iter ((values (order-domain-values variable csp)))
          (if (null? values)
              failure
              ;; This is too early; don't need to copy if
              ;; inconsistent.
              (let ((value (car values))
                    (csp (csp-copy csp))
                    (assignment (hash-table-copy assignment)))
                ;; Do we have to address constraints at this point? Yes.
                (if (consistent? variable value assignment csp)
                    (begin
                      ;; Copy at this point?
                      (hash-table-set! assignment variable value)
                      ;; This might actually modify the domains in the CSP;
                      ;; better copy before we get here?
                      (let ((inferences (inference csp variable value)))
                        (if (failure? inferences)
                            (iter (cdr values))
                            (begin
                              ;; When duplicate, take inferences; the only
                              ;; values we should be overriding, however, are
                              ;; unassigned.
                              (hash-table-merge! inferences assignment)
                              ;; By the time this finishes recursing,
                              ;; we have a complete assignment; don't
                              ;; we? Or should we handle the
                              ;; enumeration at the leaf?
                              (let ((result (backtrack-enumerate n enumeration assignment csp)))
                                ;; (debug (if (failure? result) result (hash-table->alist result)))
                                (if (failure? result)
                                    (iter (cdr values))
                                    (unless (and n (= (length (enumeration)) n))
                                      (iter (cdr values)))))))))
                    (iter (cdr values)))))))))

;;; Do we need to copy the thing? We're constantly destroying the CSP.
(define (ac-3 csp)
  @("Check arc-consistency of a csp; returns {{#t}} if the object is
arc-consistent."
    (csp "A constraint-satisfaction object")
    (@to "boolean"))
  (let ((queue (list->queue (hash-table-keys (csp-constraints csp)))))
    (let iter ()
      (if (queue-empty? queue)
          #t
          (match (queue-remove! queue)
            ((x . y)
             (if (revise csp x y)
                 ;; How does this work for e.g. infinite domains?
                 (if (zero? (length (hash-table-ref (csp-domains csp) x)))
                     #f
                     (begin
                       (for-each (lambda (neighbor)
                                   (queue-add! queue (cons neighbor x)))
                         (delete y (hash-table-ref (csp-neighbors csp) x)))
                       (iter)))
                 (iter))))))))

(define (revise csp x y)
  (let ((y-domain (hash-table-ref (csp-domains csp) y))
        (constraint (hash-table-ref (csp-constraints csp) (cons x y))))
    (let iter ((revised #f)
               (x-domain (hash-table-ref (csp-domains csp) x)))
      ;; (debug revised x-domain)
      ;; How does this work for infinite domains?
      (if (null? x-domain)
          revised
          (let ((x-value (car x-domain)))
            (if (any values (map (lambda (y-value)
                                   (constraint x-value y-value)) y-domain))
                (iter revised (cdr x-domain))
                (begin
                  (hash-table-update!
                   (csp-domains csp)
                   x
                   (lambda (x-domain)
                     (delete x-value x-domain)))
                  (iter #t (cdr x-domain)))))))))

(define neq?
  @("The complement to {{eq?}}"
    (x "Comparandum")
    (y "Comparator")
    (@to "boolean"))
  (complement eq?))