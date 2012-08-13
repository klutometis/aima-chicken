@(title "AIMA for Chicken Scheme")
@(author "Peter Danenberg")
@(email "pcd@roxygen.org")
@(heading "AIMA")

(module aima
  @("AIMA contains functions common to agents and environments.")
  (compose-environments
   debug?
   debug-print
   default-steps
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

  (use debug
       foof-loop)

  (include "aima-core.scm"))
