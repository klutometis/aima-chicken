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

  (define (simulate environment)
    (loop ((while (environment)))))

  (define (compose-environments . environments)
    (lambda ()
      (every identity (map (lambda (environment)
                             (environment))
                           environments))))

  (define (make-performance-measuring-environment
           measure-performance
           score-update!)
    (lambda () (score-update! (measure-performance))))

  (define (make-step-limited-environment steps)
    (let ((current-step 0))
      (lambda ()
        (set! current-step (+ current-step 1))
        (< current-step steps)))))
