(module aima-vacuum
  (agent-score
   agent-score-set!
   agent-location
   agent-location-set!
   agent-program
   agent-program-set!
   clean
   clean?
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
       debug
       files
       lolevel
       posix
       (prefix random-bsd bsd-)
       srfi-1
       stack
       vector-lib)

  ;;;; Two-square vacuum world

  (define (display-world world)
    (pp
     (vector-append '#(world)
                    (vector-map
                     (lambda (i location)
                       (if (clean? location) 'clean 'dirty))
                     world))))

  (define-record clean)
  (define clean (make-clean))

  (define-record dirty)
  (define dirty (make-dirty))

  (define-record unknown)
  (define unknown (make-unknown))

  (define left 0)
  (define left? zero?)

  (define right 1)
  (define right? (cute = <> 1))

  (define make-world vector)

  (define world-location vector-ref)

  (define world-location-set! vector-set!)

  (define-record agent
    location
    score
    program)

  (define-record-printer agent
    (lambda (agent output)
      (display (record->vector agent) output)))

  (define (non-penalizing-response world agent location action)
    (case action
      ((left) (agent-location-set! agent left))
      ((right) (agent-location-set! agent right))
      ((suck) (world-location-set! world location clean))
      ((noop))
      (else (error "non-penalizing-response -- Unknown action"
                   action))))

  ;; Delegate to non-penalizing-response.
  (define (penalizing-response world agent location action)
    (case action
      ((left)
       (agent-score-set! agent (- (agent-score agent) 1)))
      ((right)
       (agent-score-set! agent (- (agent-score agent) 1))))
    ;; Delegates to non-penalizing-response; thanks, Darius!
    (non-penalizing-response world agent location action))

  (define make-environment
    (case-lambda
     ((world agent) (make-environment world
                                      agent
                                      non-penalizing-response))
     ((world agent response)
      (lambda ()
        (let* ((location (agent-location agent))
               (action ((agent-program agent)
                        location
                        (clean? (world-location world location)))))
          (debug-print "agent-action" action)
          (response world agent location action))))))

  (define (make-penalizing-environment world agent)
    (make-environment world agent penalizing-response))

  (define (simple-agent-program location clean?)
    (if clean?
        (if (left? location)
            'right
            'left)
        'suck))

  (define (all-clean? world)
    ;; Vector bleeds a little world.
    (vector-every (lambda (location) (clean? location)) world))

  ;; TODO: Consider changing the variable-name `world' to something
  ;; more appropriate for a model of the world.
  (define (make-stateful-agent-program)
    ;; We could also make an initial pessimistic hypothesis of
    ;; all-dirty.
    (let ((world (make-world unknown unknown)))
      (lambda (location clean?)
        ;; Extra work here every time; otherwise, we'd have an extra
        ;; `all-clean?' check after we set the state. `vector-set!', I'd
        ;; wager, is cheaper than `all-clean?'.
        (if clean?
            (begin
              (vector-set! world location clean)
              (if (all-clean? world)
                  ;; Symbols appropriate here, or should we have predefined
                  ;; go-left, go-right, clean, do-nothing? We're message
                  ;; passing, after all; I suppose a lambda wouldn't make any
                  ;; sense?
                  ;;
                  ;; Can't be lambdas unless we redefine e.g. `go-right'
                  ;; to penalize in the case of
                  ;; `make-penalizing-environment'; better to keep as
                  ;; symbols and dispatch, right? There should be some
                  ;; sort of data-directed model we could use, though,
                  ;; instead of the case-based dispatch.
                  'noop
                  (if (right? location)
                      'left
                      'right)))
            'suck))))

  (define default-agent-program
    (make-parameter simple-agent-program))

  (define make-reflex-agent
    (case-lambda
     ((location)
      (make-reflex-agent location (default-agent-program)))
     ((location program)
      (make-agent
       location
       0
       program))))

  (define (make-simple-reflex-agent location)
    (make-reflex-agent location simple-agent-program))

  (define (make-stateful-reflex-agent location)
    (make-reflex-agent location (make-stateful-agent-program)))

  (define (make-performance-measure world)
    (lambda ()
      (vector-count (lambda (i square) (clean? square)) world)))

  (define (make-score-update! agent)
    (lambda (score)
      (agent-score-set! agent (+ (agent-score agent) score))))

  (define simulate-vacuum
    (case-lambda
     ((world agent)
      (simulate-vacuum world agent (default-steps)))
     ((world agent steps)
      (simulate-vacuum world agent steps make-environment))
     ((world agent steps make-environment)
      (simulate
       (compose-environments
        (make-step-limited-environment steps)
        (make-performance-measuring-environment
         (make-performance-measure world)
         (make-score-update! agent))
        (make-debug-environment
         agent
         (lambda (agent)
           (vector
            (let ((location (agent-location agent)))
              (if (left? location)
                  'left
                  'right))
            (agent-score agent))))
        (make-debug-environment world)
        (make-environment world agent)))
      (agent-score agent))))

  (define simulate-penalizing-vacuum
    (case-lambda
     ((world agent)
      (simulate-penalizing-vacuum world agent (default-steps)))
     ((world agent steps)
      (simulate-vacuum world agent steps make-penalizing-environment))))

  ;;;; Graph world

  (define make-graph make-hash-table)

  (define-record no-passage)
  (define no-passage (make-no-passage))
  (define passage? (complement no-passage?))

  (define up 2)
  (define up? (cute = <> 2))

  (define down 3)
  (define down? (cute = <> 3))

  (define-record location
    status
    neighbors)

  (define-record-printer location
    (lambda (location output)
      (display (record->vector location) output)))

  (define (copy-location location)
    (make-location (location-status location)
                   (vector-copy (location-neighbors location))))

  (define (copy-world world)
    (let ((world (hash-table-copy world)))
      (hash-table-walk
       world
       (lambda (name location)
         (hash-table-update!
          world
          name
          copy-location)))
      world))

  (define make-node gensym)

  (define (random-direction) (bsd-random 4))

  (define (reverse-direction direction)
    (cond ((left? direction) right)
          ((right? direction) left)
          ((up? direction) down)
          ((down? direction) up)))

  (define (make-dirty-location)
    (make-location dirty
                   (vector no-passage
                           no-passage
                           no-passage
                           no-passage)))

  (define (connect! world connectend connector direction)
    (hash-table-update!/default
     world
     connectend
     (lambda (location)
       (vector-set! (location-neighbors location) direction connector)
       location)
     (make-dirty-location))
    (hash-table-update!/default
     world
     connector
     (lambda (location)
       (vector-set! (location-neighbors location)
                    (reverse-direction direction)
                    connectend)
       location)
     (make-dirty-location)))

  (define (make-seed-world)
    (let ((world (make-hash-table))
          (start (make-node))
          (neighbor (make-node)))
      (connect! world start neighbor (random-direction))
      world))

  (define (random-start world)
    (let ((nodes (hash-table-keys world)))
      (list-ref nodes (bsd-random-integer (length nodes)))))

  (define (make-randomized-graph-agent start)
    (make-reflex-agent
     start
     (lambda (location clean?)
       (if clean?
           (list-ref '(left right up down) (random-direction))
           'suck))))

  (define (count-nodes world)
    (length (hash-table-keys world)))

  (define (count-degrees world)
    (hash-table-fold
     world
     (lambda (node location n-degrees)
       (+ n-degrees (vector-count
                     (lambda (direction neighbor)
                       (passage? neighbor))
                     (location-neighbors location))))
     0))

  (define (n-neighbors location)
    (vector-fold
     (lambda (direction n-neighbors neighbor)
       (if (no-passage? neighbor)
           n-neighbors
           (+ n-neighbors 1)))
     0
     (location-neighbors location)))

  (define default-n-nodes (make-parameter 20))

  (define make-linear-world
    (case-lambda
     (() (make-linear-world (default-n-nodes)))
     ((n-nodes)
      (let ((world (make-graph))
            (nodes (list-tabulate n-nodes (lambda i (make-node)))))
        (for-each
            (lambda (node1 node2)
              (connect! world node1 node2 right))
          (drop nodes 1)
          (drop-right nodes 1))
        world))))

  ;; This, of course, won't produce any cycles.
  (define make-preferential-depth-first-world
    (case-lambda
     (() (make-preferential-depth-first-world (default-n-nodes)))
     ((n-nodes)
      (let* ((world (make-seed-world))
             (start (random-start world)))
        (let iter ((node start)
                   (n-nodes
                    (max 0 (- n-nodes (count-nodes world))))
                   (n-degrees (count-degrees world)))
          (if (zero? n-nodes)
              world
              (let ((location
                     (hash-table-ref/default
                      world
                      node
                      (make-dirty-location))))
                (let ((n-neighbors (n-neighbors location)))
                  (if (and (< n-neighbors 4)
                           (< (bsd-random-real) (/ n-neighbors n-degrees)))
                      (let* ((new-directions
                              (vector-fold
                               (lambda (direction directions neighbor)
                                 (if (no-passage? neighbor)
                                     (cons direction directions)
                                     directions))
                               '()
                               (location-neighbors location)))
                             (new-direction
                              (list-ref
                               new-directions
                               (bsd-random (length new-directions)))))
                        ;; To make this Barabási-like, we could try to
                        ;; pick a preëxisting node; and, failing that,
                        ;; produce one.
                        ;;
                        ;; Why not just produce a direction-sensitive
                        ;; Barabási? Now that we have neighbors as a
                        ;; vector, it should be less unpleasant.
                        ;;
                        ;; To connect this node to a preëxisting one,
                        ;; however; we'd have to find nodes with
                        ;; compatible, available directions.
                        ;;
                        ;; We could produce a tree, of course, and
                        ;; randomly create appropriate cycles.
                        (let ((new-node (make-node)))
                          (connect! world node new-node new-direction)
                          (iter new-node (- n-nodes 1) (+ n-degrees 2))))
                      (let* ((neighbors
                              (vector-fold
                               (lambda (direction neighbors neighbor)
                                 (if (passage? neighbor)
                                     (cons neighbor neighbors)
                                     neighbors))
                               '()
                               (location-neighbors location)))
                             (neighbor
                              (list-ref neighbors
                                        (bsd-random (length neighbors)))))
                        (iter neighbor n-nodes n-degrees)))))))))))

  (define make-graph-world make-preferential-depth-first-world)

  (define default-width (make-parameter 1600))

  (define default-height (make-parameter 900))

  (define default-font-size (make-parameter 48.0))

  (define default-title (make-parameter #f))

  ;; Height and width are in pixels.
  (define write-dot-preamble
    (case-lambda
     ((agent step)
      (write-dot-preamble agent
                          step
                          (default-width)
                          (default-height)
                          (default-font-size)
                          (default-title)))
     ((agent step width height font-size title)
      (display "digraph G {")
      (display "node [style=filled, fontname=monospace];")
      (display "edge [fontname=monospace];")
      (if (and width height)
          (begin
            (format #t "graph [fontsize=~a, ratio=fill];" font-size)
            ;; Phew: viewports are specified in points at 72 per inch;
            ;; size is specified in pixels at 96 per inch.
            (let ((width-in-inches (/ width 96))
                  (height-in-inches (/ height 96)))
              (format #t "graph [viewport=\"~a,~a\", size=\"~a,~a!\"];"
                      (* width-in-inches 72)
                      (* height-in-inches 72)
                      width-in-inches
                      height-in-inches))))
      (if step
          (format #t "graph [label=\"~aScore: ~a; step: ~a\"]"
                  (if title (format "~a\\n" title) "")
                  (agent-score agent)
                  step)))))

  (define (write-dot-nodes world agent)
    (hash-table-walk
     world
     (lambda (name location)
       (let ((color
              (cond ((eq? (agent-location agent) name) "green")
                    ((clean? (location-status location)) "white")
                    (else "gray"))))
         (format #t "~a [fillcolor=~a];" name color)))))

  (define (write-dot-edges world)
    (hash-table-walk
     world
     (lambda (name location)
       (let ((left (vector-ref (location-neighbors location) left))
             (right (vector-ref (location-neighbors location) right))
             (up (vector-ref (location-neighbors location) up))
             (down (vector-ref (location-neighbors location) down)))
         (if (passage? left)
             (format #t "~a->~a [label=l];" name left))
         (if (passage? right)
             (format #t "~a->~a [label=r];" name right))
         (if (passage? up)
             (format #t "~a->~a [label=u];" name up))
         (if (passage? down)
             (format #t "~a->~a [label=d];" name down))))))

  (define (write-dot-postscript) (display "}\n"))

  (define write-world-as-dot
    (case-lambda
     ((world agent) (write-world-as-dot world agent #f))
     ((world agent step)
      (write-world-as-dot world
                          agent
                          step
                          (default-width)
                          (default-height)
                          (default-font-size)
                          (default-title)))
     ((world agent step width height font-size title)
      (write-dot-preamble agent step width height font-size title)
      (write-dot-nodes world agent)
      (write-dot-edges world)
      (write-dot-postscript))))

  (define (write-world-as-pdf world agent pdf)
    (receive (input output id)
      (process "neato" `("-Tpdf" "-o" ,pdf))
      (with-output-to-port output
        ;; Do we really need a blank label, for some reason?
        (lambda () (write-world-as-dot world agent #f #f #f #f #f)))
      (flush-output output)
      (close-output-port output)
      (close-input-port input)))

  (define (display-pdf pdf)
    (system* "evince -s ~a" pdf))

  (define write-world-as-gif
    (case-lambda
     ((world agent frame gif)
      (write-world-as-gif world
                          agent
                          frame
                          gif
                          (default-width)
                          (default-height)
                          (default-font-size)
                          (default-title)))
     ((world agent frame gif width height font-size title)
      (receive (input output id)
        (process "neato" `("-Tgif" "-o" ,gif))
        (with-output-to-port output
          (lambda () (write-world-as-dot world
                                    agent
                                    frame
                                    width
                                    height
                                    font-size
                                    title)))
        (flush-output output)
        (close-output-port output)
        (close-input-port input)))))

  (define (make-graph-environment world agent)
    (lambda ()
      (let* ((node (agent-location agent))
             (location (hash-table-ref world node))
             (action ((agent-program agent)
                      node
                      (clean? (location-status location)))))
        (debug-print "agent-action" action)
        (case action
          ((left)
           (let ((left (vector-ref (location-neighbors location) left)))
             (if (passage? left)
                 (agent-location-set! agent left))))
          ((right)
           (let ((right (vector-ref (location-neighbors location) right)))
             (if (passage? right)
                 (agent-location-set! agent right))))
          ((up)
           (let ((up (vector-ref (location-neighbors location) up)))
             (if (passage? up)
                 (agent-location-set! agent up))))
          ((down)
           (let ((down (vector-ref (location-neighbors location) down)))
             (if (passage? down)
                 (agent-location-set! agent down))))
          ((noop))
          ((suck)
           (location-status-set! (hash-table-ref world node) clean))
          (else (error "graph-environment -- Unknown action"))))))

  (define (make-graph-performance-measure world agent)
    (lambda ()
      (let ((clean-locations
             ;; Quicker with map and apply?
             (hash-table-fold
              world
              (lambda (name location clean-locations)
                (if (clean? (location-status location))
                    (+ clean-locations 1)
                    clean-locations))
              0)))
        (agent-score-set! agent (+ (agent-score agent) clean-locations)))))

  (define make-graph-animating-environment
    (case-lambda
     ((world agent directory)
      (make-graph-animating-environment world
                                        agent
                                        directory
                                        (default-width)
                                        (default-height)
                                        (default-font-size)
                                        (default-title)))
     ((world agent directory width height font-size title)
      (let ((frame 0))
        (lambda ()
          (let ((gif (make-pathname directory (number->string frame) "gif")))
            (write-world-as-gif world
                                agent
                                frame
                                gif
                                width
                                height
                                font-size
                                title))
          (set! frame (+ frame 1)))))))

  ;; I think make-step-limited-environment is a special case of
  ;; make-finalizing-environment with noop.
  ;;
  ;; This probably belongs in aima; should we preserve the
  ;; step-limited-environment as a specialization of this?
  (define (make-finalizing-environment finalizer final-step)
    (let ((step 0))
      (lambda ()
        (set! step (+ step 1))
        (let ((continue? (< step final-step)))
          (if (not continue?) (finalizer))
          continue?))))

  (define (make-animation-finalizer directory file)
    (lambda ()
      (system* "rm -fv ~a.gif" file)
      (system* "convert $(find ~a -type f | sort -k 1.~a -n) -loop 0 ~a.gif"
               directory
               (+ (string-length directory) 2)
               file)
      (system* "identify ~a/0.gif" directory)
      (system* "mencoder ~a.gif -ovc lavc -o ~a.avi" file file)))

  (define (make-composite-animation-finalizer combinandum combinator file)
    (let ((composite-directory (create-temporary-directory)))
      (system* "cd ~a && for i in *; do echo $i; convert +append $i ~a/$i ~a/$i; done"
               combinandum
               combinator
               composite-directory)
      (make-animation-finalizer composite-directory file)))

  (define (make-unknown-location clean?)
    (make-location (if clean? clean dirty)
                   (vector unknown unknown unknown unknown)))

  (define (undiscovered-directions location)
    (vector-fold
     (lambda (direction undiscovered-directions neighbor)
       (if (unknown? neighbor)
           (cons direction undiscovered-directions)
           undiscovered-directions))
     '()
     (location-neighbors location)))

  (define (reverse-move move)
    (case move
      ((left) 'right)
      ((right) 'left)
      ((up) 'down)
      ((down) 'up)))

  (define (direction->move direction)
    (list-ref '(left right up down) direction))

  (define (move->direction move)
    (case move
      ((left) left)
      ((right) right)
      ((up) up)
      ((down) down)))

  (define-record cycle)
  (define cycle (make-cycle))

  ;; Dealing with all this move-location punning makes things complex;
  ;; we can clean this up a little bit by writing some germane
  ;; abstractions on the world.
  ;;
  ;; We're not dealing with cycles yet, by the way; does this entail
  ;; determining whether or not a new node is accounted for in the
  ;; world? I believe so.
  (define (make-stateful-graph-agent start)
    (make-reflex-agent
     start
     (let ((world (make-hash-table))
           (nodes (list->stack (list start)))
           (moves (make-stack)))
       (lambda (node clean?)
         (if (stack-empty? nodes)
             'noop
             (if (not clean?)
                 'suck
                 (let ((location
                        (hash-table-ref/default
                         world
                         node
                         (make-unknown-location clean?))))
                   ;; The following is general house-keeping on the state.
                   (if (stack-empty? moves)
                       ;; We're dealing with an uninitialized agent: set
                       ;; the world. This could also be a terminal
                       ;; agent, couldn't it? Is there a better place to
                       ;; initialize?
                       (hash-table-set! world node location)
                       ;; We need to distinguish the case, apparently,
                       ;; where we've just backtracked; this isn't quite
                       ;; the same as a fail-to-move.
                       ;;
                       ;; In 2.12, when we're dealing with a bump
                       ;; sensor, when don't have to play these games
                       ;; with an implicit bump.
                       (let ((last-move (stack-peek moves)))
                         (if (eq? last-move 'backtrack)
                             ;; Our position is the result of
                             ;; backtracking; remove the special
                             ;; backtracking move.
                             (stack-pop! moves)
                             (if (eq? (stack-peek nodes) node)
                                 ;; We tried to move but could not; mark the
                                 ;; last direction as no-passage.
                                 (let ((last-move (stack-pop! moves)))
                                   (vector-set! (location-neighbors location)
                                                (move->direction last-move)
                                                no-passage))
                                 (let* ((last-node (stack-peek nodes))
                                        ;; Need to replace hash-table-ref, &c.
                                        ;; with something more germane.
                                        (last-location
                                         (hash-table-ref world last-node)))
                                   (if (hash-table-exists? world node)
                                       ;; Cycle detected! Push the
                                       ;; cycle-sentinel.
                                       (stack-push! nodes cycle)
                                       (begin
                                         ;; This is a new node: add it
                                         ;; to the world.
                                         (hash-table-set! world node location)
                                         ;; Also, add it to the list of
                                         ;; interesting nodes.
                                         (stack-push! nodes node)))
                                   ;; This location's reverse-move points to
                                   ;; the last node.
                                   (vector-set! (location-neighbors location)
                                                (move->direction
                                                 (reverse-move last-move))
                                                last-node)
                                   ;; The last location's move points to
                                   ;; this node.
                                   (vector-set! (location-neighbors
                                                 last-location)
                                                (move->direction last-move)
                                                node))))))
                   ;; Are there any other undiscovered passages?
                   (let ((new-moves (map direction->move
                                         (undiscovered-directions location))))
                     (if (or (cycle? (stack-peek nodes))
                             (null? new-moves))
                         (begin
                           ;; Remove this node from the interesting
                           ;; nodes: it's been thoroughly explored.
                           (stack-pop! nodes)
                           (if (stack-empty? moves)
                               ;; No moves lest; let's rest. This may change
                               'noop
                               (let ((move (stack-pop! moves)))
                                 ;; Push the special backtrack move onto the
                                 ;; stack; this helps us distinguish the
                                 ;; backtracking case from the case where
                                 ;; we've hit a wall.
                                 ;;
                                 ;; The bump-sensor should obviate the
                                 ;; need for this, I think; or not.
                                 (stack-push! moves 'backtrack)
                                 ;; Go back the way we came.
                                 (reverse-move move)))) 
                         (let ((move (list-ref new-moves
                                               (bsd-random (length new-moves)))))
                           (stack-push! moves move)
                           move))))))))))

  (define default-file (make-parameter "graph"))

  (define simulate-graph
    (case-lambda
     ((world agent)
      (simulate-graph world agent (default-steps)))
     ((world agent steps)
      (parameterize ((randomize! bsd-randomize))
        (simulate
         ;; Order of composition matters, apparently; be thoughtful.
         (compose-environments
          (make-step-limited-environment steps)
          (make-debug-environment agent)
          (make-graph-environment world agent)
          (make-graph-performance-measure world agent)))))))

  (define simulate-graph/animation
    (case-lambda
     ((world agent file)
      (simulate-graph/animation world agent file (default-steps)))
     ((world agent file steps)
      (simulate-graph/animation world
                                agent
                                file
                                steps
                                (default-width)
                                (default-height)
                                (default-font-size)
                                (default-title)))
     ((world agent file steps width height font-size title)
      (let ((directory (create-temporary-directory)))
        (parameterize ((randomize! bsd-randomize))
          (simulate
           ;; Order of composition matters, apparently; be thoughtful.
           (compose-environments
            (make-step-limited-environment steps)
            ;; Can't this contain its finalizer? Maybe even give it the
            ;; terminal frame?
            (make-graph-animating-environment world
                                              agent
                                              directory
                                              width
                                              height 
                                              font-size
                                              title)
            (make-finalizing-environment
             (make-animation-finalizer directory file)
             steps)
            (make-debug-environment agent)
            (make-graph-environment world agent)
            (make-graph-performance-measure world agent))))
        directory))))

  (define (simulate-comparatively world agent steps width height font-size title)
    (let ((directory (create-temporary-directory)))
      (parameterize ((randomize! bsd-randomize))
        (simulate
         ;; Order of composition matters, apparently; be thoughtful.
         (compose-environments
          (make-step-limited-environment steps)
          ;; Can't this contain its finalizer? Maybe even give it the
          ;; terminal frame?
          (make-graph-animating-environment world
                                            agent
                                            directory
                                            width
                                            height 
                                            font-size
                                            title)
          (make-debug-environment agent)
          (make-graph-environment world agent)
          (make-graph-performance-measure world agent))))
      directory)

    ;; We should generalize this.
    (define compare-graphs
      (case-lambda
       ((world agent-one title-one agent-two title-two composite-file)
        (compare-graphs world
                        agent-one
                        title-one
                        agent-two
                        title-two
                        composite-file
                        (default-steps)
                        (/ (default-width) 2)
                        (default-height)
                        (/ (default-font-size) 2)))
       ((world
         agent-one
         title-one
         agent-two
         title-two
         composite-file
         steps
         width
         height
         font-size)
        (let ((directory-one
               (simulate-comparatively (copy-world world)
                                       agent-one
                                       steps
                                       width
                                       height
                                       font-size
                                       title-one))
              (directory-two
               (simulate-comparatively world
                                       agent-two
                                       steps
                                       width
                                       height
                                       font-size
                                       title-two)))
          (let ((composite-directory (create-temporary-directory)))
            (system* "cd ~a && for i in *; do echo $i; convert +append $i ~a/$i ~a/$i; done"
                     directory-one
                     directory-two
                     composite-directory)
            ((make-animation-finalizer composite-directory composite-file)))))))))
