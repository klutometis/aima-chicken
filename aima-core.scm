(define-syntax define-record-and-printer
  @("Define both a record type and a vector-form printer.")
  (lambda (expression rename compare)
    (match expression
      ((_ record . fields)
       (let ((%define-record (rename 'define-record))
             (%define-record-printer (rename 'define-record-printer))
             (%begin (rename 'begin))
             (%lambda (rename 'lambda))
             (%write (rename 'write))
             (%record->vector (rename 'record->vector)))
         `(,%begin
           (,%define-record ,record ,@fields)
           (,%define-record-printer
            ,record
            (,%lambda (record out)
                 (,%write (,%record->vector record) out)))))))))

(define debug?
  @("Should we print debugging information to stdout?")
  (make-parameter #t))

;; Another case with merely a value? We're going to have to
;; macrologize this thing, then.
(define debug-print
  @("Print key-value pairs if the parameter `debug?' is true."
    (key "The key to print")
    (value "The value to print")
    (out "The port to print to"))
  (case-lambda
   ((key value) (debug-print key value #t))
   ((key value out)
    (if (debug?) (format out "~a: ~a~%" key value)))))

(define random-seed
  @("`random-seed' is passed to `randomize!' during `simulate'.")
  (make-parameter #f))

(define randomize!
  @("`randomize!' is called before simulation and is seeded with
`random-seed'.")
  (make-parameter randomize))

;; Should we have first-class support for seeding the random-number
;; generator; or simply a generic initialization? Problem is that we
;; don't know which random-number library their using: we'd have to
;; pass in a thunk or at least a seed and a randomizer; in the
;; former case, however, we've gone to generic initialization.
(define simulate
  @("Run an environment to completion; an environment
is complete when it returns false."
    (environment "The environment to simulate")
    (randomize! "Function to seed the random-number generator for
reproducible results")
    (random-seed "Seed to seed the random-number generator")
    (@to "#f"))
  (case-lambda
   ((environment)
    (simulate environment (randomize!) (random-seed)))
   ((environment randomize! random-seed)
    (if random-seed (randomize! random-seed))
    (loop ((while (environment)))))))

(define (compose-environments . environments)
  @("Compose environments into a single environment suitable for
`simulate'.

`compose-environments' effectively `ands' over its constituent
environments every step."
    (environments "The environments to be composed")
    (@to "environment"))
  (lambda ()
    (every identity (map (lambda (environment)
                           (environment))
                         environments))))

(define (make-performance-measuring-environment
         measure-performance
         score-update!)
  @("Make an environment that updates a score according to a
performance measure."
    (measure-performance "A nullary procedure which measures performance")
    (score-update! "A function which receives the performance measure
and updates the score accordingly")
    (@to "environment"))
  (lambda () (score-update! (measure-performance))))

(define default-steps
  @("Default number of steps for the step-limited environment")
  (make-parameter 1000))

(define make-step-limited-environment
  @("Make an environment that stops simulation after a certain number
of steps."
    (steps "The number of steps after which to stop simulating")
    (@to "environment"))
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
  @("Make an environment that prints debugging information (according
to `debug?')."
    (object "The object to debug")
    (make-printable-object "A function which optionally transforms the
object before printing")
    (@to "environment"))
  (er-macro-transformer
   (lambda (expression rename compare)
     (let ((%print (rename 'debug-print)))
       (match expression
         ((_ object)
          `(lambda () (,%print ',object ,object)))
         ((_ object make-printable-object)
          `(lambda () (,%print ',object (,make-printable-object ,object)))))))))
