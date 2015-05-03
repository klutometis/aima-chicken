(define-record-and-printer unassigned)
(define unassigned (make-unassigned))
(define assigned? (complement unassigned?))

(define-record-and-printer failure)
(define failure
  @("The failure object: to distinguish ''bona-fide'' solutions to a
CSP that are {{#f}}.")
  (make-failure))

(define success?
  @("Success is defined negatively as the absence of failure."
    (result "The result to test")
    (@to "boolean"))
 (complement failure?))

(define-record-and-printer csp
  @("A constraint-satisfaction-problem"
    (domains "A hash-table mapping variables to possible values")
    (constraints "A hash-table mapping pairs of variables to a dyadic
lambda which returns {{#f}} if the values don't satisfy the constraint")
    (neighbors "A hash-table adjacency-list of constraints")
    (@example "A trivial (but inconsistent) CSP"
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
    (@example "A trivial 2-coloring problem"
              (define arc-consistent-coloring
                (make-csp
                 ;; Domain can also be lambdas?
                 (alist->hash-table '((a . (white black))
                                      (b . (white black))))
                 (alist->hash-table `(((a . b) . ,neq?)
                                      ((b . a) . ,neq?)))
                 (alist->hash-table '((a b)
                                      (b a)))))
              (hash-table->alist (backtracking-search arc-consistent-coloring))))
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
;;; For n-ary constraints, we're going to have to check every
;;; permutation; combination, if we're doing undirected graphs.
;;;
;;; Pattern matching?
(define (binary-consistent? variable value assignment csp)
  ;; Use the default case when there are no neighbors.
  (let* ((neighbors (hash-table-ref/default (csp-neighbors csp) variable '()))
         (assigned-neighbors (filter (lambda (neighbor) (assigned? (hash-table-ref assignment neighbor)))
                                     neighbors)))
    (let ((results (map (lambda (neighbor) ((hash-table-ref (csp-constraints csp) (cons variable neighbor))
                                       value
                                       (hash-table-ref assignment neighbor)))
                        assigned-neighbors)))
      ;; (debug variable value neighbors assigned-neighbors results)
      (every values results))))

(define consistent? (make-parameter binary-consistent?))

(define (arc-inference csp variable value)
  (hash-table-set! (csp-domains csp) variable (list value))
  (if (ac-3 csp)
      (begin
        ;; (debug (hash-table-map (csp-domains csp) (lambda (variable domain) (length domain))))
        (hash-table-fold (csp-domains csp)
                         (lambda (variable values inference)
                           (when (= 1 (length values))
                             (hash-table-set! inference variable (car values)))
                           inference)
                         (make-hash-table)))
      failure))

(define inference (make-parameter arc-inference))

;; (define (inference csp variable value)
;;   (make-hash-table))

(define backtracking-enumeration
  @("Enumerate up to {{n}} solutions of the {{csp}}; enumerate all if {{n}}
is {{#f}} or unspecified."
    (n "Enumerate up to {{n}} solutions")
    (csp "The CSP to solve")
    (cons "How to construct enumerations ({{cons}} by default)")
    (nil "Base enumeration ({{()}} by default)")
    (stop? "Unary function taking the current enumeration: {{#t}}
stops, {{#f}} continues; by default, compares {{n}} to the length of
the current enumeration.")
    (@to "list"))
  (case-lambda
   ((csp) (backtracking-enumeration #f csp))
   ((n csp)
    (backtracking-enumeration
     csp
     cons
     '()
     (lambda (enumeration)
       (and n (= (length enumeration) n)))))
   ((csp cons nil stop?)
    (let ((enumeration (make-parameter nil)))
      (backtrack-enumerate
       enumeration
       (make-assignment csp)
       csp
       cons
       stop?)
      (enumeration)))))

(define (delta inferences assignment)
  (let ((delta (make-hash-table))
        (assigned-variables
         (hash-table-fold
          assignment
          (lambda (variable value assigned-variables)
            (if (assigned? value)
                (cons variable assigned-variables)
                assigned-variables))
          '()))
        (inferred-variables (hash-table-keys inferences)))
    (for-each (lambda (variable)
                (hash-table-set! delta variable (hash-table-ref inferences variable)))
      (lset-difference eq?
                       inferred-variables
                       assigned-variables))
    delta))

(define (backtrack-enumerate enumeration assignment csp cons stop?)
  (if (complete? assignment)
      (enumeration (cons assignment (enumeration)))
      (let ((variable (select-unassigned-variable assignment)))
        (let iter ((values (order-domain-values variable csp)))
          (if (null? values)
              failure
              ;; This is too early; don't need to copy if
              ;; inconsistent.
              (let ((value (car values)))
                ;; (debug (consistent? variable value assignment csp)
                ;;        variable
                ;;        value
                ;;        (hash-table->alist assignment))
                (if ((consistent?) variable value assignment csp)
                    (let ((csp (csp-copy csp))
                          (assignment (hash-table-copy assignment)))
                      ;; Copy at this point?
                      (hash-table-set! assignment variable value)
                      ;; This might actually modify the domains in the CSP;
                      ;; better copy before we get here?
                      (let ((inferences ((inference) csp variable value)))
                        ;; (debug inferences)
                        ;; (debug (if (failure? inferences) failure (hash-table->alist inferences))
                        ;;        (hash-table->alist assignment))
                        ;; (debug (hash-table->alist assignment))
                        ;; (unless (failure? inferences)
                        ;;   (debug (hash-table->alist (delta inferences assignment))))
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
                              (let ((result (backtrack-enumerate enumeration assignment csp cons stop?)))
                                ;; (debug (if (failure? result) result (hash-table->alist result)))
                                (if (failure? result)
                                    (iter (cdr values))
                                    (unless (stop? (enumeration))
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

(define (neighbors-from-constraints constraints)
  (let ((neighbors (make-hash-table)))
    (for-each (match-lambda ((x . y)
                        (hash-table-update!/default
                         neighbors
                         x
                         (lambda (Y) (cons y Y))
                         '())))
      (hash-table-keys constraints))
    neighbors))

(define (set-domains! domains variables domain)
  (for-each (lambda (variable) (hash-table-set! domains variable domain))
    variables))

(define-syntax xor
  @("Logical xor: whether one or the other proposition is true (but
not both)"
    (x "A proposition")
    (y "Another proposition")
    (@to "boolean"))
  (lambda (expression rename compare)
    (match expression
      ((_ x y)
       (let ((%or (rename 'or))
             (%and (rename 'and))
             (%not (rename 'not)))
         `(,%and (,%or ,x ,y)
                 (,%not (,%and ,x ,y))))))))

(define (set-pairwise-constraints! constraints X Y relation) 
  (for-each
      (lambda (x)
        (for-each
            (lambda (y)
              ;; Or do we want to merge these with some binary
              ;; operation?
              (hash-table-set! constraints (cons x y) relation))
          (delete x Y)))
    X))

(define (set-bidirectional-constraint! constraints x y constraint-x constraint-y)
  (hash-table-update!/default constraints
                              (cons x y)
                              (lambda (constraints-x)
                                (lambda (x y) (and (constraint-x x y)
                                              (constraints-x x y))))
                              (lambda (x y) #t))
  (hash-table-update!/default constraints
                              (cons y x)
                              (lambda (constraints-y)
                                (lambda (y x) (and (constraint-y y x)
                                              (constraints-y y x))))
                              (lambda (x y) #t)))

(define (set-pairwise-bidirectional-constraints! constraints X Y constraint-x constraint-y)
  (for-each
      (lambda (x y)
        (set-bidirectional-constraint! constraints x y constraint-x constraint-y))
    X
    Y))

(define (set-alldiff-constraints! constraints variables)
  (set-pairwise-constraints! constraints variables variables neq?))

(define neq?
  @("The complement to {{eq?}}"
    (x "Comparandum")
    (y "Comparator")
    (@to "boolean"))
  (complement eq?))

(define (random-map n)
  @("Create a random k-coloring problem; returns an adjacency-list of
nodes as a hash-table."
    (n "The number of nodes in the problem")
    (@to "hash-table"))
  (let ((random-points (random-points n))
        (connections (make-hash-table)))
    (let iter-point ((points random-points)
                     (modified? #f))
      (if (null? points)
          (if modified?
              (iter-point (shuffle random-points) #f)
              connections)
          (let ((point (car points)))
            (let iter-counter-point
                ((counter-points
                  (sort-by-proximity point (delete point random-points))))
              (if (null? counter-points)
                  (iter-point (cdr points) modified?)
                  (let ((counter-point (car counter-points)))
                    (if (member point
                                (hash-table-ref/default connections counter-point '()))
                        (iter-counter-point (cdr counter-points))
                        (if (intersects-other? connections point counter-point)
                            (iter-counter-point (cdr counter-points))
                            (begin
                              (hash-table-update!/default
                               connections
                               point
                               (lambda (counter-points)
                                 (lset-adjoin eq? counter-points counter-point))
                               '())
                              (hash-table-update!/default
                               connections
                               counter-point
                               (lambda (points)
                                 (lset-adjoin eq? points point))
                               '())
                              (iter-point (cdr points) #t))))))))))))

(define (counter-clockwise? a b c)
  (> (* (- (point-y c) (point-y a))
        (- (point-x b) (point-x a)))
     (* (- (point-y b) (point-y a))
        (- (point-x c) (point-x a)))))

(define (intersect? a b c d)
  (and (neq? a b) (neq? a d) (neq? b c) (neq? b d)
       (xor (counter-clockwise? a c d) (counter-clockwise? b c d))
       (xor (counter-clockwise? a b c) (counter-clockwise? a b d))))

(define (random-points n)
  (list-tabulate n (lambda (i) (make-point (random-real) (random-real)))))

(define (sort-by-proximity point points)
  (sort points < (lambda (sortiendum) (point-distance point sortiendum))))

(define (intersects-other? connections point counter-point)
  (call/cc
   (lambda (return)
     (hash-table-walk connections
       (lambda (whence whithers)
         (for-each (lambda (whither)
                     (when (intersect? whence whither point counter-point)
                       (return #t)))
           whithers)))
     (return #f))))

(define-record-and-printer point x y)

(define (point-distance p1 p2)
  (sqrt (+ (expt (- (point-x p1) (point-x p2)) 2)
           (expt (- (point-y p1) (point-y p2)) 2))))

(define (shuffle! v)
  (do ((n (vector-length v) (- n 1)))
      ((zero? n) v)
    (let* ((r (random n)) (t (vector-ref v r)))
      (vector-set! v r (vector-ref v (- n 1)))
      (vector-set! v (- n 1) t))))

(define (shuffle list)
  @("Shuffle a list."
    (list "The list to shuffle")
    (@to "list"))
  (let ((vector (list->vector list)))
    (shuffle! vector)
    (vector->list vector)))

(define (make-labels points)
  (let ((labels (make-hash-table)))
    (for-each (lambda (point) (hash-table-set! labels point (gensym))) points)
    labels))

(define (write-map-as-dot map solution dot)
  (with-output-to-file dot
    (let ((points (hash-table-keys map))
          (edges (make-hash-table)))
      (lambda ()
        (write-graph-preamble)
        (let ((labels (make-labels points)))
          (for-each (lambda (point)
                      (write-node (hash-table-ref labels point)
                                  (point-x point)
                                  (point-y point)
                                  (hash-table-ref solution point)))
            points)
          (hash-table-walk map
            (lambda (whence whithers)
              (let ((whence-label (hash-table-ref labels whence)))
                (for-each (lambda (whither)
                            (unless (hash-table-exists? edges (cons whither whence))
                              (hash-table-set! edges (cons whence whither) #t)
                              (let ((whither-label (hash-table-ref labels whither)))
                                (write-edge whence-label whither-label))))
                  whithers)))))
        (write-graph-postamble)))))

(define (write-map-as-png map solution png)
  (let ((dot (create-temporary-file)))
    (write-map-as-dot map solution dot)
    (run (neato -n1 -Tpng -o ,png < ,dot))))

(define default-viewer (make-parameter "sxiv"))

(define display-map-as-png
  (case-lambda ((map solution)
           (display-map-as-png map solution (default-viewer)))
          ((map solution viewer)
           (let ((png (create-temporary-file ".png")))
             (write-map-as-png map solution png)
             (run (sxiv ,png))))))
