(module aima
  (agent-location
   agent-location-set!
   agent-actuate
   agent-actuate-set!
   compose-environments
   environment-step
   environment-step-set!
   environment-score
   environment-score-set!
   make-agent
   make-environment
   make-step-limited-environment
   simulate)

  (import chicken
          data-structures
          scheme
          srfi-1)

  (use debug)

  (define-record environment
    step
    score)

  (define (simulate environment)
    (let next-step ((continue? ((environment-step environment))))
      (if continue?
          (next-step ((environment-step environment)))
          ((environment-score environment)))))

  (define (compose-environments . environments)
    (make-environment
      (lambda ()
        (every values (map (lambda (environment)
                             ((environment-step environment)))
                           environments)))
      (lambda ()
        (reduce + 0 (map (lambda (environment)
                           ((environment-score environment)))
                         environments)))))

  (define (make-step-limited-environment steps)
    (let ((current-step 0))
      (make-environment
        (lambda ()
          (set! current-step (+ current-step 1))
          (< current-step steps))
        (lambda ()
          0))))

  (define-record agent
    location
    actuate))
