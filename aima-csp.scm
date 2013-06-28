@(heading "AIMA-CSP")

(module aima-csp
  @("Solver for constraint-satisfaction-problems")
  (ac-3
   backtracking-search
   backtracking-enumeration
   failure?
   make-csp
   neq?)

  (import scheme
          chicken
          data-structures
          srfi-1
          srfi-69)

  (use define-record-and-printer
       matchable)

  (include "aima-csp-core.scm"))
