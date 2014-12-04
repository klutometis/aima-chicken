@(heading "AIMA-Vacuum")

(module aima-vacuum
  @("`aima-vacuum' has agents and environments for chapter 2:
Intelligent Agents.")
  (agent-score
   agent-score-set!
   agent-location
   agent-location-set!
   agent-program
   agent-program-set!
   clean
   clean?
   compare-graphs
   copy-world
   cycle
   cycle?
   connect!
   default-n-nodes
   direction->move
   dirty
   dirty?
   display-world
   display-pdf
   down
   down?
   left
   left?
   location-status
   location-status-set!
   location-neighbors
   location-neighbors-set!
   make-agent
   make-graph
   make-graph-world
   make-linear-world
   make-location
   make-node
   make-performance-measure
   make-preferential-depth-first-world
   make-randomized-graph-agent
   make-reflex-agent
   make-simple-reflex-agent
   make-stateful-reflex-agent
   make-stateful-graph-agent
   make-score-update!
   make-unknown-location
   make-world
   move->direction
   random-start
   reverse-move
   right
   right?
   simulate-graph
   simulate-graph/animation
   simulate-penalizing-vacuum
   simulate-vacuum
   unknown
   unknown?
   up
   up?
   world-location
   world-location-set!
   write-world-as-pdf
   write-world-as-dot
   write-world-as-gif)

  (import scheme
          chicken
          data-structures
          extras
          ports
          srfi-13
          srfi-69
          utils)

  (use aima
       files
       lolevel
       posix
       (prefix random-bsd bsd-)
       srfi-1
       stack
       vector-lib)

  (include "aima-vacuum-core.scm"))
