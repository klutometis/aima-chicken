(use aima
     debug
     test)

(define (make-vacuum-environment world agent)
  (make-environment
    (lambda ()
      (let* ((location (agent-location agent))
             (action ((agent-actuate agent)
                      location
                      (vector-ref world location))))
        (case action
          ((left) (agent-location-set!
                   agent
                   (max (- (agent-location agent) 1) 0)))
          ((right) (agent-location-set!
                    agent
                    (min (+ (agent-location agent) 1) 1)))
          ((suck) (vector-set! world (agent-location agent) #f))
          (else (error (string-append
                        "make-vacuum-environment:environment-step -- "
                        "Unknown action")
                       action)))))
    (lambda ()
      (vector-count (lambda (i x) (not x)) world))))

(define (make-vacuum-agent location)
  (make-agent
   location
   (lambda (location dirty?)
     (if dirty?
         'suck
         (if (zero? location)
             'right
             'left)))))

(test
 2
 (simulate
  (compose-environments
   (make-step-limited-environment 1000)
   (make-vacuum-environment
    '#(#t #t)
    (make-vacuum-agent 0)))))