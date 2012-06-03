;;;; The basic environment simulator code

;;; This file defines the environment simulator function: RUN-ENVIRONMENT.  It
;;; is a little different from the pseudo-code in the book: to make it easier
;;; to write new environments, we used an object-oriented approach. Rather
;;; than requiring the caller to pass in a bunch of functions to
;;; RUN-ENVIRONMENT, you will define methods for the functions for each
;;; subtype of environment.  We added a DISPLAY parameter to control whether
;;; intermediate results are displayed. As an example, the following
;;; expression builds an environment and runs an agent in it, displaying the
;;; results:
;;;
;;; (run-environment (make-vacuum-world :aspec '(random-vacuum-agent)))
;;;
;;; We also define AGENT-TRIALS to compare the performance of several
;;; different agents in a series of random environments, all drawn from an
;;; environment subtype.  For example to run 2 agents in 20 environments each:
;;;
;;; (agent-trials 'vacuum-world
;;;   '(random-vacuum-agent reactive-vacuum-agent) :n 20)

(define-class <environment>
  (<standard-class>)
  ((agents
    initform: '()
    accessor: environment-agents)
   (step
    initform: 0
    accessor: environment-step)
   (max-steps
    initform: 1000
    accessor: environment-max-steps)
   (port
    initform: #t
    accessor: environment-port)
   (initialized?
    initform: #f
    accessor: environment-initialized?)
   (state
    initform: #f
    accessor: environment-state)))

;;; An agent is something that perceives and acts.  As such, each agent has a
;;; slot to hold its current percept, and its current action.  The action
;;; will be handed back to the environment simulator to perform (if legal).
;;; Each agent also has a slot for the agent program, and one for its score
;;; as determined by the performance measure.

(define-class <agent>
  (<standard-class>)
  ((program
    initform: void
    accessor: agent-program)
   (body
    initform: (make-agent-body)
    accessor: agent-body)
   (score
    initform: 0
    accessor: agent-score)
   (percept
    initform: #f
    accessor: agent-percept)
   (action
    initform: #f
    accessor: agent-action)
   (name
    initform: #f
    accessor: agent-name)))

;;;; Top level functions

(define (run-environment env)
  "Basic environment simulator.  It gives each agent its percept, gets an
  action from each agent, and updates the environment. It also keeps score
  for each agent, and optionally displays intermediate results. [p 48]"
  (initialize env)
  (display-environment env)
  (call-with-current-continuation
   (lambda (return)
     (dotimes (i (environment-max-steps env))
       (inc! (environment-step env))
       (for-each (lambda (agent)
                   (set! (agent-percept agent)
                         (get-percept env agent))
                   (set! (agent-action agent)
                         ;; TODO: `funcall' was here.
                         ((agent-program agent)
                          (agent-percept agent))))
         (environment-agents env))
       (update-fn env)
       (for-each (lambda (agent)
                   (set! (agent-score agent)
                         (performance-measure env agent)))
         (environment-agents env))
       (display-environment env)
       (when (termination? env) (return)))))
  env)

(define (agent-trials environment-fn agent-types &key (n 10))
  "Report how well a single agent does in a set of N similar environments,
  and compare that to other agents in the same set of environments.
  Environment-fn takes a :agents keyword argument, and returns an environment.
  Agent-types is a list of names of functions that each create an agent."
  (let ((env-gen-random-state (make-random-state #t)))
    (map (lambda (agent-type)
           (agent-trial environment-fn agent-type
                        (make-random-state env-gen-random-state) n)
           agent-types))))

;;;; Generic Functions that must be defined for each environment

;;; For each new type of environment you want to define, you will need a
;;; defstructure that inherits from (includes) ENVIRONMENT, and you will need
;;; to write new methods (or inherit existing methods) for each of the
;;; following eight functions.  Here are the ones that will change for each
;;; new environment:

(define-method (get-percept (environment <environment>) agent) #f)

(define-method (update-fn (env <environment>))
  "Modify the environment, based on agents actions, etc."
  (execute-agent-actions env))


(define-method (legal-actions (env <environment>))
  "A list of the action operators that an agent can do."
  #f)

(define-method (performance-measure (env <environment>) agent)
  "Return a number saying how well this agent is doing."
  (- (environment-step env)))

;;; Here are the ones that can usually be inherited:

(define-method (initialize (env <environment>))
  "Called once to do whatever is necessary to set up the environment
  for running the simulation."
  (initialize-agent-names env)
  (set! (environment-initialized? env) t)
  env)

(define-method (termination? (env <environment>))
  "Return true if the simulation should end now."
  #f)

(define-method (display-environment (env <environment>))
  "Display the current state of the environment."
  ;; You probably won't need to specialize this, unless you want to do
  ;; a fancy graphical user interface
  (let ((port (environment-port env)))
    (when port 
      (format port "~&At Time step ~D:~%" (environment-step env))
      (when (> (environment-step env) 0)
            (for-each (lambda (port)
                        (format port 
                                "~&Agent ~A perceives ~A~%~6Tand does ~A~%"
                                agent (agent-percept agent)
                                (agent-action agent)))
              (environment-agents env)))
      (display-environment-snapshot env))))

(define-method (display-environment-snapshot (env <environment>))
  "Display a 'picture' of the current state of the environment."
  (print env (environment-port env)))

;;;; Auxiliary Functions

(define (run-eval-environment env)
  "Basic environment simulator; the same as run-environment. [p 48]
  We decided it was silly to run the environment and NOT eval the agents,
  so in this code, and in future editions of the book, we will only have
  RUN-ENVIRONMENT, and it will evaluate the agents."
  (run-environment env))

(define (agent-trial environment-fn agent-type env-gen-random-state n)
  "Run n environments with an identical agent in each, and average the scores."
  ;; By binding *random-state* to env-gen-random-state, we hope to reproduce
  ;; the same set of environments each time AGENT-TRIAL is called with the
  ;; same environment-fn.
  (let ((total 0) (score 0))
    (do ((i 0 (+ i 1)))
        ((= i n))
      ;; NB: We're using (randomize ...) instead of *random-state*.
      (let* ((env
              (begin
                ;; What do you want to bet `env-gen-random-state' is
                ;; not an integer?
                (randomize env-gen-random-state)
                ;; TODO: `apply' was here.
                (environment-fn 
                 port: #f
                 aspec: (list agent-type)))))
        (run-environment env)
        (inc! total (agent-score (first (environment-agents env)))))) 
    (set! score (float (/ total n)))
    (format #t "~&~10,2F average for ~A" score agent-type)
    score))

(define (execute-agent-actions env)
  "Each agent (if the agent is alive and has specified a legal action)
  takes the action."
  (for-each (lambda (agent)
              (let ((act (agent-action agent)))
                (when (member (op act) (legal-actions env))
                      ;; TODO: `funcall' was here.
                      ((op act) env (agent-body agent) (args act)))))
    (environment-agents env)))

(define-method (print-object (env <environment>) port)
  (format port "#<~A; Step: ~D, Agents:~{ ~A~}>"
      (class-of env) (environment-step env)
      (environment-agents env)))