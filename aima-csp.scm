@(heading "AIMA-CSP")

(module aima-csp
  @("Solver for constraint-satisfaction-problems")
  (ac-3
   backtracking-search
   backtracking-enumeration
   consistent?
   csp-constraints
   csp-copy
   csp-domains
   csp-neighbors
   display-map-as-png
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
   shuffle
   success?
   write-map-as-dot
   write-map-as-png
   xor)

  (import scheme
          chicken)

  (use data-structures
       define-record-and-printer
       files
       graphviz
       list-utils
       matchable
       random-bsd
       shell
       srfi-1
       srfi-69
       srfi-95)

  (import-for-syntax matchable)

  (include "aima-csp-core.scm"))
