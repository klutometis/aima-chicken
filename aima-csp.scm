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
   failure?
   inference
   make-csp
   neq?
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
       srfi-1
       srfi-69)

  (import-for-syntax matchable)

  (include "aima-csp-core.scm"))
