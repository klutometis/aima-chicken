(define-record node
  @("Data structure for graphs"
    (state "An indexable point")
    (predecessor "The node-predecessor")
    (action "Not used")
    (path-cost "Cost of the path up to this point"))
  state
  predecessor
  action
  path-cost)

(define (make-initial-node state)
  (make-node state #f #f 0))

(define (predecessor-path node)
  @("List the predecessors of this node."
    (node "The node to predecess")
    (@to "list"))
  (let iter ((path (list node)))
    (let ((predecessor (node-predecessor (car path))))
      (if predecessor
          (iter (cons predecessor path))
          ;; Do we want to reverse?
          (reverse path)))))

(define (search start
                successors
                step-cost
                goal?
                make-frontier
                frontier-add!
                frontier-delete!
                frontier-empty?)
  (let ((visited (make-hash-table))
        (frontier (make-frontier)))
    (frontier-add! frontier (make-initial-node start))
    (let search ()
      (if (frontier-empty? frontier)
          (error "Search failed -- SEARCH")
          (let* ((predecessor (frontier-delete! frontier))
                 (successors (successors predecessor)))
            (hash-table-set! visited (node-state predecessor) #f)
            (if (goal? predecessor)
                (reverse (predecessor-path predecessor))
                (let ((unvisited-successors
                       (filter (lambda (successor)
                                 (hash-table-ref/default
                                  visited
                                  successor
                                  #t))
                               successors)))
                  (for-each
                      (lambda (successor)
                        (let ((node (make-node successor
                                               predecessor
                                               #f
                                               (+ (node-path-cost predecessor)
                                                  ;; Should this be
                                                  ;; between two
                                                  ;; nodes?
                                                  (step-cost
                                                   successor
                                                   (node-state predecessor))))))
                          (frontier-add! frontier node)))
                    unvisited-successors)
                  (search))))))))

(define (search-depth-first start successors step-cost goal?)
  (search start
          successors
          step-cost
          goal?
          make-stack
          stack-push!
          stack-pop!
          stack-empty?))

(define (search-breadth-first start successors step-cost goal?)
  (search start
          successors
          step-cost
          goal?
          make-queue
          queue-add!
          queue-remove!
          queue-empty?))

(define (search-best-first start successors step-cost goal? f)
  (search start
          successors
          step-cost
          goal?
          make-min-heap
          (lambda (heap node)
            (let ((key (heap-key heap node))
                  (path-cost (f node)))
              (if key
                  (if (< path-cost key)
                      (heap-change-key! heap node path-cost))
                  (heap-insert! heap path-cost node))))
          heap-extract-extremum!
          heap-empty?))

(define (search-uniform-cost start successors step-cost goal?)
  (search-best-first start
                     successors
                     step-cost
                     goal?
                     node-path-cost))

(define (search-greedy-best-first start
                                  successors
                                  step-cost
                                  goal?
                                  heuristic)
  (search-best-first start
                     successors
                     step-cost
                     goal?
                     heuristic))

(define (A* start successors step-cost goal? heuristic)
  (search-best-first start
                     successors
                     step-cost
                     goal?
                     (lambda (node) (+ (node-path-cost node)
                                  (heuristic node)))))

(define (search-recursive-best-first start
                                     successors
                                     step-cost
                                     goal?
                                     heuristic)
  (define (search return predecessor f-limit)
    ;; (debug (node-state predecessor))
    (if (goal? (node-state predecessor))
        (return (predecessor-path predecessor))
        (let ((successors (successors predecessor)))
          (if (null? successors)
              (values #f +inf.0)
              (let ((heap (make-min-heap)))
                (for-each
                    (lambda (successor)
                      (let ((successor
                             (make-node successor
                                        predecessor
                                        #f
                                        (step-cost
                                         successor
                                         (node-state predecessor)))))
                        (node-path-cost-set!
                         successor
                         (max (length (predecessor-path successor))
                              (node-path-cost predecessor)))
                        (heap-insert! heap
                                      (node-path-cost successor)
                                      successor)))
                  successors)
                (let iter ()
                  (let* ((best (heap-extremum heap))
                         (best-path-cost (node-path-cost best)))
                    ;; (debug heap
                    ;;        (heap-size heap)
                    ;;        (node-state (heap-extremum heap)))
                    (if (> best-path-cost f-limit)
                        (values #f best-path-cost)
                        (let ((alternative
                               (if (= (heap-size heap) 1)
                                   +inf.0
                                   (let ((first
                                          (heap-extract-extremum! heap))
                                         (alternative
                                          (node-path-cost
                                           (heap-extremum heap))))
                                     ;; (debug heap
                                     ;;        (heap-size heap)
                                     ;;        (node-state (heap-extremum heap))
                                     ;;        (node-path-cost first)
                                     ;;        first
                                     ;;        (node-state first)
                                     ;;        'iter
                                     ;;        (heap-member? heap first))
                                     (heap-delete! heap first)
                                     ;; (debug 'iter (heap-member? heap first))
                                     (heap-insert!
                                      heap
                                      (node-path-cost first)
                                      first)
                                     alternative))))
                          (receive (predecessor best-path-cost)
                            (search return
                                    best
                                    (min f-limit alternative))
                            (if predecessor
                                (values predecessor best-path-cost)
                                (iter))))))))))))
  (call/cc (lambda (return)
             (search return (make-initial-node start) +inf.0))))

(define-record-and-printer point x y)

(define (make-random-points n)
  (do ((points '() (cons (make-point (random-real) (random-real)) points))
       (n n (- n 1)))
      ((zero? n) points)))

(define (point-distance p1 p2)
  @("Calculate the distance between two points."
    (p1 "The first point")
    (p2 "The second point")
    (@to "distance"))
  (sqrt (+ (expt (- (point-x p1) (point-x p2)) 2)
           (expt (- (point-y p1) (point-y p2)) 2))))

(define (make-straight-line-distance-heuristic end)
  (lambda (node) (point-distance (node-state node) end)))
