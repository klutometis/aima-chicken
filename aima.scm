@(heading "AIMA")

(module aima
  @("AIMA contains functions common to agents and environments.")
  (compose-environments
   debug?
   debug-print
   default-steps
   define-record-and-printer
   make-debug-environment
   make-step-limited-environment
   make-performance-measuring-environment
   random-seed
   randomize!
   simulate)

  (import chicken
          data-structures
          extras
          scheme
          srfi-1)

  (import-for-syntax matchable)

  (use lolevel foof-loop)

  (include "aima-core.scm"))
