(module aima
  (agent-actuate
   agent-actuate-set!
   agent-location
   agent-location-set!
   agent-score
   agent-score-set!
   compose-environments
   environment-step
   environment-step-set!
   make-agent
   make-environment
   make-step-limited-environment
   make-performance-measuring-environment
   simulate)

  (import chicken
          data-structures
          scheme
          srfi-1)

  (use debug
       foof-loop)

  (define-record environment
    step)

  (define (simulate environment)
    (loop ((while ((environment-step environment))))))

  (define (compose-environments . environments)
    (make-environment
      (lambda ()
        (every values (map (lambda (environment)
                             ((environment-step environment)))
                           environments)))))

  (define (make-performance-measuring-environment performance-measure)
    (make-environment
      performance-measure))

  (define (make-step-limited-environment steps)
    (let ((current-step 0))
      (make-environment
        (lambda ()
          (set! current-step (+ current-step 1))
          (< current-step steps)))))

  (define-record agent
    location
    actuate
    score))
