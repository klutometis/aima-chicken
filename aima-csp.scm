@(heading "AIMA-CSP")

(module aima-csp
  @("Solver for constraint-satisfaction-problems")
  (ac-3
   backtracking-search
   backtracking-enumeration
   consistent?
   csp-constraints
   csp-domains
   csp-neighbors
   failure
   failure?
   inference
   make-csp
   neq?
   random-map
   set-alldiff-constraints!
   set-bidirectional-constraint!
   set-pairwise-bidirectional-constraints!
   set-pairwise-constraints!
   set-domains!
   success?
   xor)

  (import scheme
          chicken)

  (use data-structures
       define-record-and-printer
       list-utils
       matchable
       random-bsd
       srfi-1
       srfi-69
       srfi-95)

  (import-for-syntax matchable)

  (include "aima-csp-core.scm"))
