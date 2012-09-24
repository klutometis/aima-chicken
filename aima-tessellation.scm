(module aima-tessellation
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
