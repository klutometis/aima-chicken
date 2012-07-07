(module aima-vacuum
  (agent-score
   agent-score-set!
   agent-location
   agent-location-set!
   agent-program
   agent-program-set!
   clean
   clean?
   default-steps
   dirty
   dirty?
   display-world
   left
   left?
   make-agent
   make-performance-measure
   make-reflex-agent
   make-simple-reflex-agent
   make-stateful-reflex-agent
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
       debug
       srfi-1
       vector-lib)

  (define (display-world world)
    (pp
     (vector-append '#(world)
                    (vector-map
                     (lambda (i location)
                       (if (clean? location) 'clean 'dirty))
                     world))))

  (define-record clean)
  (define clean (make-clean))

  (define-record dirty)
  (define dirty (make-dirty))

  (define-record unknown)
  (define unknown (make-unknown))

  (define left 0)
  (define left? zero?)

  (define right 1)
  (define right? positive?)

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
      ((noop))
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
                        (clean? (world-location world location)))))
          (debug-print "agent-action" action)
          (response world agent location action))))))

  (define (make-penalizing-environment world agent)
    (make-environment world agent penalizing-response))

  (define (simple-agent-program location clean?)
    (if clean?
        (if (left? location)
            'right
            'left)
        'suck))

  (define (all-clean? world)
    ;; Vector bleeds a little world.
    (vector-every (lambda (location) (clean? location)) world))

  (define (make-stateful-agent-program)
    ;; We could also make an initial pessimistic hypothesis of
    ;; all-dirty.
    (let ((world (make-world unknown unknown)))
      (lambda (location clean?)
        ;; Extra work here every time; otherwise, we'd have an extra
        ;; `all-clean?' check after we set the state. `vector-set!', I'd
        ;; wager, is cheaper than `all-clean?'.
        (if clean?
            (begin
              (vector-set! world location clean)
              (if (all-clean? world)
                  ;; Symbols appropriate here, or should we have predefined
                  ;; go-left, go-right, clean, do-nothing? We're message
                  ;; passing, after all; I suppose a lambda wouldn't make any
                  ;; sense?
                  ;;
                  ;; Can't be lambdas unless we redefine e.g. `go-right'
                  ;; to penalize in the case of
                  ;; `make-penalizing-environment'; better to keep as
                  ;; symbols and dispatch, right? There should be some
                  ;; sort of data-directed model we could use, though,
                  ;; instead of the case-based dispatch.
                  'noop
                  (if (right? location)
                      'left
                      'right)))
            'suck))))

  (define default-agent-program
    (make-parameter simple-agent-program))

  (define make-reflex-agent
    (case-lambda
     ((location)
      (make-reflex-agent location (default-agent-program)))
     ((location program)
      (make-agent
       location
       0
       program))))

  (define (make-simple-reflex-agent location)
    (make-reflex-agent location simple-agent-program))

  (define (make-stateful-reflex-agent location)
    (make-reflex-agent location (make-stateful-agent-program)))

  (define (make-performance-measure world)
    (lambda ()
      (vector-count (lambda (i square) (clean? square)) world)))

  (define (make-score-update! agent)
    (lambda (score)
      (agent-score-set! agent (+ (agent-score agent) score))))

  (define default-steps (make-parameter 1000))

  (define simulate-vacuum
    (case-lambda
     ((world agent)
      (simulate-vacuum world agent (default-steps)))
     ((world agent steps)
      (simulate-vacuum world agent steps make-environment))
     ((world agent steps make-environment)
      (simulate
       (compose-environments
        (make-step-limited-environment steps)
        (make-performance-measuring-environment
         (make-performance-measure world)
         (make-score-update! agent))
        (make-debug-environment agent)
        (make-debug-environment world)
        (make-environment world agent)))
      (agent-score agent))))

  (define simulate-penalizing-vacuum
    (case-lambda
     ((world agent)
      (simulate-penalizing-vacuum world agent (default-steps)))
     ((world agent steps)
      (simulate-vacuum world agent steps make-penalizing-environment)))))
