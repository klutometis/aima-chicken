@(heading "AIMA-Tessellation")

(module aima-tessellation
  @("aima-tessellation has procedures for tessellating a plane into
disjoint, convex polygons suitable for exercise 3.7; and then plotting
that tessellation with a path.")
  (plot-tessellation
   point-x
   point-y
   tessellate
   tessellation-points
   tessellation-neighbors
   tessellation-start
   tessellation-end)

  (import chicken scheme)

  (use aima
       debug
       files
       lolevel
       matchable
       R
       srfi-1
       srfi-69
       vector-lib)

  (include "aima-tessellation-core.scm"))
