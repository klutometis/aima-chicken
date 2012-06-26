(use aima
     debug
     srfi-1
     test
     vector-lib)

(define clean #t)
(define clean? identity)

(define dirty #f)
(define dirty? (complement clean?))

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
          ((suck) (vector-set! world (agent-location agent) clean))
          (else (error (string-append
                        "make-vacuum-environment:environment-step -- "
                        "Unknown action")
                       action)))))))

(define (make-vacuum-agent location)
  (make-agent
   location
   (lambda (location clean?)
     (if clean?
         (if (zero? location)
             'right
             'left)
         'suck))
   0))

(define (make-vacuum-performance-measure vacuum-world vacuum-agent)
  (lambda ()
    (let ((clean-squares
           (vector-count (lambda (i x) x) vacuum-world)))
      (agent-score-set! vacuum-agent
                        (+ (agent-score vacuum-agent)
                           clean-squares)))))

(define make-vacuum-world vector)

(test
 "Vacuum agent in clean-clean world"
 2000
 (let ((vacuum-world (make-vacuum-world clean clean))
       (vacuum-agent (make-vacuum-agent 0)))
   (let ((vacuum-environment
          (make-vacuum-environment
           vacuum-world
           vacuum-agent)))
     (simulate
      (compose-environments
       (make-step-limited-environment 1000)
       (make-performance-measuring-environment
        (make-vacuum-performance-measure
         vacuum-world
         vacuum-agent))
       vacuum-environment))
     (agent-score vacuum-agent))))
