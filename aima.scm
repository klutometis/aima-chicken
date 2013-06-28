@(egg "aima")
@(description "AIMA-support for Chicken Scheme")
@(author "Peter Danenberg")
@(email "pcd@roxygen.org")
@(username "klutometis")
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

  (use debug
       lolevel
       foof-loop)

  (include "aima-core.scm"))
