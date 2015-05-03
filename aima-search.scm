@(heading "AIMA-search")

(module aima-search
  @("Some searching algorithms, viz.: A*, best-first, breadth-first,
depth-first, uniform-cost, recursive best-first (in progress).")
  (
   ;; Nodes
   make-node
   node-state
   node-state-set!
   node-predecessor
   node-predecessor-set!
   node-action
   node-action-set!
   node-path-cost
   node-path-cost-set!
   predecessor-path
   ;; Points
   make-point
   make-random-points
   point-x
   point-x-set!
   point-y
   point-y-set!
   point-distance
   ;; Search strategies
   search
   search-depth-first
   search-breadth-first
   search-best-first
   search-uniform-cost
   search-greedy-best-first
   search-recursive-best-first
   A*
   )

  (import scheme chicken srfi-1 srfi-69)

  (use data-structures
       define-record-and-printer
       heap
       random-bsd
       stack)

  (include "aima-search-core.scm"))
