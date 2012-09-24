@(heading "AIMA-Tessellation")

(module aima-tessellation
  @("aima-tessellation has procedures for tessellating a plane into
disjoint, convex polygons suitable for exercise 3.7; and then plotting
that tessellation with a path.")
  (make-node
   node-state
   node-state-set!
   node-parent
   node-parent-set!
   node-action
   node-action-set!
   node-path-cost
   node-path-cost-set!
   point-distance
   plot-tessellation
   plot-tessellation/animation
   point-x
   point-y
   predecessor-path
   tessellate
   tessellation-points
   tessellation-neighbors
   tessellation-start
   tessellation-end)

  (import chicken scheme)

  (use aima
       debug
       files
       format
       lolevel
       matchable
       R
       srfi-1
       srfi-69
       utils
       vector-lib)

  (include "aima-tessellation-core.scm"))
