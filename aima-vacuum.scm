(module aima-vacuum
  (agent-score
   agent-score-set!
   agent-location
   agent-location-set!
   agent-program
   agent-program-set!
   clean
   clean?
   dirty
   dirty?
   display-world
   left
   left?
   make-performance-measure
   make-reflex-agent
   make-score-update!
   make-world
   right
   right?
   simulate-penalizing-vacuum
   simulate-vacuum
   world-location
   world-location-set!)

  (import scheme
          chicken
          data-structures
          extras
          srfi-13)

  (use aima
       srfi-1
       vector-lib)

  (define (display-world world)
    (pp
     (vector-append '#(world)
                    (vector-map
                     (lambda (i clean?)
                       (if clean? 'clean 'dirty))
                     world))))

  (define clean #t)
  (define clean? identity)

  (define dirty #f)
  (define dirty? (complement clean?))

  (define left 0)
  (define left? zero?)

  (define right 1)
  (define right? (complement zero?))

  (define make-world vector)

  (define world-location vector-ref)

  (define world-location-set! vector-set!)

  (define-record agent
    location
    score
    program)

  (define-record-printer agent
    (lambda (agent output)
      (format output
              "#(agent ~a ~a)"
              (if (left? (agent-location agent))
                  'left
                  'right)
              (agent-score agent))))

  (define (non-penalizing-response world agent location action)
    (case action
      ((left) (agent-location-set! agent left))
      ((right) (agent-location-set! agent right))
      ((suck) (world-location-set! world location clean))
      (else (error "non-penalizing-response -- Unknown action"
                   action))))

  (define (penalizing-response world agent location action)
    (case action
      ((left)
       (agent-score-set! agent (- (agent-score agent) 1))
       (agent-location-set! agent left))
      ((right)
       (agent-score-set! agent (- (agent-score agent) 1))
       (agent-location-set! agent right))
      ((suck) (world-location-set! world location clean))
      (else (error "penalizing-response -- Unknown action"
                   action))))

  (define make-environment
    (case-lambda
     ((world agent) (make-environment world
                                      agent
                                      non-penalizing-response))
     ((world agent response)
      (lambda ()
        (let* ((location (agent-location agent))
               (action ((agent-program agent)
                        location
                        (world-location world location))))
          (response world agent location action))))))

  (define (make-penalizing-environment world agent)
    (make-environment world agent penalizing-response))

  (define (reflex-agent-program location clean?)
    (if clean?
        (if (left? location)
            'right
            'left)
        'suck))

  (define make-reflex-agent
    (case-lambda
     ((location)
      (make-reflex-agent location reflex-agent-program))
     ((location program)
      (make-agent
       location
       0
       program))))

  (define (make-performance-measure world)
    (lambda ()
      (vector-count (lambda (i square) (clean? square)) world)))

  (define (make-score-update! agent)
    (lambda (score)
      (agent-score-set! agent (+ (agent-score agent)
                                 score))))

  (define simulate-vacuum
    (case-lambda
     ((world agent) (simulate-vacuum world agent 1000))
     ((world agent steps)
      (simulate
       (compose-environments
        (make-step-limited-environment steps)
        (make-performance-measuring-environment
         (make-performance-measure world)
         (make-score-update! agent))
        (make-debug-environment agent)
        (make-debug-environment world world-display)
        (make-environment world agent)))
      (agent-score agent)))))
