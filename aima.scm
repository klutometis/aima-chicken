(module aima
  (compose-environments
   debug?
   debug-print
   default-steps
   make-debug-environment
   make-step-limited-environment
   make-performance-measuring-environment
   simulate)

  (import chicken
          data-structures
          extras
          scheme
          srfi-1)

  (import-for-syntax matchable)

  (use debug
       foof-loop)

  (define debug? (make-parameter #t))

  ;; Another case with merely a value? We're going to have to
  ;; macrologize this thing, then.
  (define debug-print
    (case-lambda
     ((key value) (debug-print key value #t))
     ((key value out)
      (if (debug?) (format out "~a: ~a~%" key value)))))

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

  (define default-steps (make-parameter 1000))

  (define make-step-limited-environment
    (case-lambda
     (() (make-step-limited-environment (default-steps)))
     ((steps)
      (let ((current-step 0))
        (lambda ()
          (set! current-step (+ current-step 1))
          (< current-step steps))))))

  ;; Damn, we destroyed a nice abstraction that could have served for
  ;; e.g. animations by changing `print' to `make-printable-object'.
  (define-syntax make-debug-environment
    (er-macro-transformer
     (lambda (expression rename compare)
       (let ((%print (rename 'debug-print)))
         (match expression
           ((_ object)
            `(lambda () (,%print ',object ,object)))
           ((_ object make-printable-object)
            `(lambda () (,%print ',object (,make-printable-object ,object))))))))))
