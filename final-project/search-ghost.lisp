;;; ----------------------------------------
;;;   CMPU-365, Spring 2019
;;;   Final Project,
;;; ----------------------------------------
;;;     FILE:  search-ghost.lisp
;;;   AUTHORS:  Amy O'Connell, Mattew Imiolek
;;;   *some content taken from asmt2 solutions
;;; ----------------------------------------
;;;  This file contains functions needed to perform
;;;  shortest path search in pacman GAME

;;; =============================================================
;;; FROM ASMT2 BASIC-DEFNS.LISP (print functions omitted)
;;; =============================================================

;;; --------------------------------------
;;;  The PROBLEM struct
;;; --------------------------------------

(defstruct search-problem
  init-state		; the initial state
  actions		; the list of actions
  goal-test-func	; the goal test function
  state-eq-func		; the state-equality function
  )

;;; --------------------------------------
;;;  The NODE struct
;;; --------------------------------------
;;;  Note the use of the :PRINT-FUNCTION keyword argument that enables
;;;  us to specify which function should be used to display a NODE
;;;  struct.  (Kind of like providing a "toString" method in Java.)

(defstruct node
  state			; the state associated with this node
  (parent nil)		; the parent node for this node
  (action nil)		; the most recent action performed
  (depth 0)		; the depth of this node
  )

;;;  BUILD-PATH
;;; -----------------------------------------------------------
;;;  INPUT:   GOAL-NODE
;;;  OUTPUT:  A list of nodes obtained by tracing the parent
;;;           links starting from the goal-node all the way
;;;           back to the root node

(defun build-path (goal-node)

  ;; BUILD-PATH-ACC is a recursive helper function
  ;; Note the use of LABELS (similar to LETREC in Scheme)

  (labels ((build-path-acc (node acc)
	     ;; Base Case:  NODE is NIL
	     ;;   This happens when previous function call involved
	     ;;   the root node.  The root node's parent is NIL.
	     ;;   In this case, we are done!
	     (if (not node)
		 ;; So return the accumulator
		 acc
	       ;; Recursive Case:
	       ;;   Accumulate the current node as we move
	       ;;   to the PARENT...
	       (build-path-acc (node-parent node) (cons node acc)))))

    ;; Body of LABELS:  call the recursive helper with ACC=NIL
    (build-path-acc goal-node nil)))

;;; ------------------------------------------------------------
;;;  RESULTS struct -- used to keep track of info during search
;;; ------------------------------------------------------------
;;;  The start time, end time, and elapsed time are all measured by
;;;  "internal-run-time" which is typically measured in milliseconds.
;;;  See the global constant INTERNAL-TIME-UNITS-PER-SECOND, which
;;;  equals 1000 on our system.
;;; -----------------------------------------------------------
;;;  Note the PRINT-RESULTS print function

(defstruct results
  (goal-node nil)       ; a goal node, if found; otherwise, NIL
  (num-nodes 0)         ; the number of nodes generated during the search
  (start-time NIL)      ; when the search started (msecs)
  (end-time NIL)        ; when the search stopped (msecs)
  (elapsed-time NIL)    ; how long the search took (msecs)
  )

;;;  GEN-SEARCH -- wrapper function for GEN-SEARCH-GUTS
;;; --------------------------------------------------------
;;;   INPUTS:  PROBLEM, a search problem
;;;            QUEUE-FN, a queuing function
;;;   OUTPUT:  A RESULTS data structure, whose contents report
;;;            the results of the search.
;;; --------------------------------------------------------
;;;   NOTE: This is just a wrapper for GEN-SEARCH-GUTS (which you will
;;;   define in "gen-search-starter.lisp").  GEN-SEARCH-GUTS does most
;;;   of the work.  The wrapper function creates a new RESULTS data
;;;   structure, initialized with NUM-NODES = 0 and START-TIME fetched
;;;   from the operating system.

(defconstant *print-stats* nil)

(defun gen-search (problem queue-fn)

  (let (;; Initial RESULTS struct
	      (rezzy (make-results)))

    ;; When *print-stats* is set to true, start timing search now
    (when *print-stats*
      (setf (results-start-time rezzy) (get-internal-run-time)))

    ;; GEN-SEARCH-GUTS will update the contents of REZZY:
    ;;   GOAL-NODE will either be NIL (failure) or a goal NODE,
    (gen-search-guts problem queue-fn rezzy)

    ;; When *print-stats* is set to true, calculate and print
    ;; statistics from the search
    (when *print-stats*
      ;; Update END-TIME and ELAPSED-TIME
      (setf (results-end-time rezzy) (get-internal-run-time))
      (setf (results-elapsed-time rezzy) (- (results-end-time rezzy)
  					  (results-start-time rezzy)))
      (cond
       ;; Case 1:  The search returned a node... presumably a GOAL node!
       ((node-p (results-goal-node rezzy))
        (format t "Hey!  We found a goal node!!~%"))
       ;; Case 2:  The search returned something else... indicating failure
       (t
        (format t "Uh oh... queue empty!~%")))
      ;; Report statistics
      (let ((num-nodes (results-num-nodes rezzy))
        	  (elapsed-time (/ (results-elapsed-time rezzy)
        			   internal-time-units-per-second
        			   1.0)))
        (format t "~%Generated ~A nodes " num-nodes)
        (if (> num-nodes 0)
      	  (format t "in ~A seconds (~A sec/node)~%"
      		  elapsed-time (/ elapsed-time num-nodes))
  	      (format t "~%"))))

    ;; Return the search-result
    rezzy
    ))

;;;  (destructive!) Helper functions for BREADTH and DEPTH-FIRST search:

;;;  FRONT-ENQUEUE!
;;; -------------------------------------------------
;;;  INPUT:  OLD-NODES, a list of old nodes (i.e., old search queue)
;;;          NEW-NODES, a list of new nodes (i.e., new child nodes)
;;;  OUTPUT:  A list containing new-nodes (at the front) and
;;;           old nodes (at the rear)

(defun front-enqueue! (old-nodes new-nodes)
  ;; NCONC is a destructive version of APPEND (see Paul Graham's book)
  ;; We use it here for efficiency reasons
  (nconc new-nodes old-nodes))

;;;  END-ENQUEUE!
;;; ---------------------------------------------------
;;;  Just like FRONT-ENQUEUE! except that the old-nodes go at the front.

(defun end-enqueue! (old-nodes new-nodes)
  ;; NCONC is a destructive version of APPEND
  ;; We use it here for efficiency reasons
  (nconc old-nodes new-nodes))

;;;  BREADTH-FIRST-SEARCH
;;; ---------------------------------------------------
;;;  INPUT:  PROB, a search problem
;;;  OUTPUT:  The result of doing breadth-first search on the given
;;;             problem:  either NIL or a goal NODE.

(defun breadth-first-search (prob)
  ;; Note that since END-ENQUEUE! is not the first element of the
  ;; list, we need to use #' to ensure that its "function value"
  ;; is passed as input to the GEN-SEARCH function.
  (gen-search prob #'end-enqueue!))

;;;  DEPTH-FIRST-SEARCH
;;; ---------------------------------------------------
;;;  Just like breadth-first-search, except that it uses a different
;;;  queuing function.

(defun depth-first-search (prob)
  (gen-search prob #'front-enqueue!))

;;; =============================================================
;;; FROM ASMT2 GEN-SEARCH-SOLNS.LISP
;;; =============================================================

;;;   CYCLE?
;;; -------------------------------------------------------------
;;;  INPUTS:  STATE, a problem state
;;;           NODE, a search node
;;;           STATE-EQ-FUNC, a function that determines whether
;;;             two states are equal
;;;  OUTPUT:  T if the given STATE is the same as the state of
;;;    the given NODE or the state of any of NODE's ancestors.
;;;    Uses STATE-EQ-FUNC to determine whether two states are equal.
;;;    Otherwise returns NIL.
;;;    NOTE:  If NODE is NIL it returns NIL.

(defun cycle? (state node state-eq-func)
  ;; Returns T if NODE is an instance of a NODE structure...
  (and node
       ;; ... and either STATE is equal to NODE's state
       (or (funcall state-eq-func state (node-state node))
	   ;; ... or STATE is equal to a state of one of NODE's ancestors
	   (cycle? state (node-parent node) state-eq-func))))

;;;  MAKE-ROOT-NODE
;;; ---------------------------------------
;;;  INPUT:  PROB, a search problem
;;;  OUTPUT:  A search NODE that will be the ROOT NODE of a
;;;           brand new search tree.

(defun make-root-node (prob)
 ;; The default values are okay for every field except STATE,
 ;; which needs to be the initial state specified by PROB.
 (make-node :state (search-problem-init-state prob)))

;;;  EXPAND
;;; ---------------------------------
;;;  INPUTS:  NODE, a search node
;;;           ACTS, a list of actions
;;;           ST-EQ-FUNC, a function for testing whether two
;;;              states are equal
;;;           REZZY, a RESULTS struct
;;;  OUTPUT:  A list of child nodes obtained by applying the
;;;           given list of actions to NODE's state.  However, it
;;;           does *NOT* include child nodes whose states already
;;;           appear on the path from the root node to NODE.
;;;           (Uses CYCLE? to determine this.)

(defun expand (node acts st-eq-func rezzy)
 (labels
     ;; ACCUMULATOR-BASED HELPER FUNCTION:  EXPAND-ACC
     ;;   INPUTS:  ACTIONS, the actions that have not yet been applied
     ;;            CHILDREN, the accumulator of child nodes
     ;;   OUTPUT:  The accumulated list of child nodes
     ;; ------------------------------------------------------
     ;;  Note that the syntax of a LABELS function does not
     ;;  involve the use of LAMBDA; so it is just like DEFUN in
     ;;  that respect...

     ((expand-acc (actions children)

	 (cond

	  ;; BASE CASE:  No more ACTIONS to apply; so we're done...
	  ((null actions)
	   ;; increment global counter by the number of accumulated child nodes
	   (incf (results-num-nodes rezzy) (length children))
	   ;; return the accumulated list of child nodes
	   children)

	  ;; RECURSIVE CASE:  More actions left to apply
	  (t
	   (let* (;; NEW-STATE is the state that results from applying
		  ;; the first action to NODE's state
		  (new-state (funcall (first actions) (node-state node)))
		  ;; UPDATED-CHILDREN will be the updated accumulator
		  ;; for the next recursive function call.
		  (updated-children
		   (cond
		    ;; Case 1:  NEW-STATE is non-NIL and it is not a repeated
		    ;;          state (as determined by CYCLE?)
		    ((and new-state
			  (not (cycle? new-state node st-eq-func)))
		     ;; so create a new node struct and CONS it onto
		     ;; the front of the accumulator
		     (cons (make-node
			    :state new-state
			    :parent node
			    :depth (1+ (node-depth node))
			    :action (first actions))
			   children))
		    ;; Case 2:
		    (t
		     ;; otherwise, just keep CHILDREN the same
		     children))))

	     ;; make recursive function call on rest of ACTIONS
	     ;; and updated accumulator of child nodes
	     (expand-acc (rest actions)
			 updated-children))))))

   ;; Call recursive helper with accumulator set to NIL
   (expand-acc acts nil)))

;;; --------------------------------------------------------
;;;  GEN-SEARCH-GUTS
;;; ---------------------------------------------------------------
;;;  INPUTS: PROBLEM, a search problem
;;;          QUEUE-FN, a queuing function (used to insert newly
;;;            created nodes into the search queue).  The queuing
;;;            function determines which kind of search we're doing.
;;;          REZZY, a RESULTS struct
;;;  OUTPUT: The RESULTS struct, with its fields updated to include
;;;          info about the search (e.g., num nodes explored; the goal
;;;          node, if found; and timing information).
;;; ---------------------------------------------------------------
;;;  This function performs the indicated search problem using
;;;  the search strategy determined by QUEUE-FN.

(defun gen-search-guts (problem queue-fn rezzy)

 (let ((gt-func (search-problem-goal-test-func problem))
	(st-eq-func (search-problem-state-eq-func problem))
	(acts (search-problem-actions problem)))

   (labels
	;; RECURSIVE HELPER FUNCTION
	((gen-rec (queue)

	   (cond

	    ;; Base Case 1:  The search-queue is empty
	    ((null queue)
	     ;; Return REZZY, with GOAL-NODE=NIL, indicating failure
	     rezzy)

	    ;; Base Case 2:  The first node in the queue is a GOAL node
	    ((funcall gt-func (node-state (first queue)))
	     ;; Set GOAL-NODE in the RESULTS struct to that goal node, success!
	     (setf (results-goal-node rezzy) (first queue))
	     ;; Return the results
	     rezzy)

	    ;; Recursive Case:  More work to do
	    (t
	     ;; Expand first node in the queue (i.e., generate new
	     ;; child nodes).  Then use the QUEUE-FN to insert those
	     ;; new nodes into the REST of the queue.  The QUEUE-FN
	     ;; determines whether you're doing breadth-first or
	     ;; depth-first search.
	     (gen-rec (funcall queue-fn
			       (rest queue)
			       (expand (first queue)
				       acts
				       st-eq-func
				       rezzy)))))))

     ;; BODY of LABELS (i.e., LETREC)
     ;; Call the recursive helper function with initial search queue
     (gen-rec (list (make-root-node problem))))))

;;; =============================================================
;;; USEFUL FUNCTIONS FOR SEARCH IN PACMAN
;;; =============================================================

(defun copy-array (old-array)
  (let* ((new-array (make-array '(15 15))))
    (dotimes (y 15)
      (dotimes (x 15)
        (setf (aref new-array y x)
              (aref old-array y x))))
    new-array))

;;; =============================================================
;;; SIMPLIFIED PACMAN FOR SEARCH - BASED ON ASMT2 VW.LISP
;;; =============================================================

;;;  PM-STATE data structure
;;; ---------------------------
;;;   BOARD: array representing the game board
;;;   PAC-LOC: ordered pair representing PAC's X and Y coordinates
;;;   PAC-LOC: ordered pair representing GHOST's X and Y coordinates

(defstruct pm-state
  board
  pac-loc
  ghost-loc)

;;;  MAKE-PM-STATE-FROM-GAME
;;; -----------------------------------------------------
;;;   INPUT: GAMEY - a pacman GAME struct
;;;          INDEX - the index of the GHOST in GAME'S list
;;;                  of GHOSTS we want to use for search
;;;   OUTPUT: a PM-STATE struct representing the current
;;;           state of GAMEY

(defun make-pm-state-from-game (gamey index)
  (let ((pac (game-pac gamey))                     ; retrieve GHOSTY at INDEX
        (ghosty (nth index (game-ghosts gamey))))  ; from GHOSTS
    (make-pm-state
      :board (game-board gamey)
      :pac-loc (list (pacman-x pac) (pacman-y pac)) ; store X and Y coordinates
      :ghost-loc (list (ghost-x ghosty)             ; for PAC and GHOST
                       (ghost-y ghosty)))))         ; as ordered pairs

;;;  PM-STATE-EQUAL?
;;; ---------------------------------------------------
;;;   INPUT:  STATE1, STATE2 -- Two PM-STATE structs
;;;   OUTPUT:  T if the given PM-STATES have the same contents, NIL otherwise.

(defun pm-state-equal? (state1 state2)
  ;; are the 2 search ghosts in the same location?
  ;; (note: we only care about GHOST-LOC because
  ;; PAC-LOC should not change and BOARD reflects GHOST-LOC)
  (equal
    (pm-state-ghost-loc state1) (pm-state-ghost-loc state2)))

;;;  PM-GOAL?
;;; ----------------------------------------------------
;;;   INPUT:  STATE, a PM-STATE struct
;;;   OUTPUT:  T if STATE is a goal-state. (search GHOST and PAC are in
;;;            the same location)

(defun pm-goal? (state)
;; the goal for search GHOST is to be in the same location as PAC
;; check that PAC and search GHOST have the same X and Y coordinates
    (equal
      (pm-state-ghost-loc state) (pm-state-pac-loc state)))

;;;  VALID-XY
;;; --------------------------
;;;  A helper function for the DO-MOVE function.
;;;  INPUTS:  X, Y -- numerical coordinates
;;;  OUTPUT:  T if (x,y) is a valid location on the board.
;;;           (i.e. not a wall or off of the board)

(defun valid-xy (x y board)
  (and
    ;; target location is on the board
       (> x 0)
       (> y 0)
       (< x 15)
       (< y 15)
       ;; target location is not a wall
       (not (eq (aref board x y) 1))))


;;;  DO-MOVE-FOR-SEARCH
;;; -------------------------------------------------
;;;   Nondestructive generic move operator. Values of DELTA-X and
;;;   DELTA-Y determine which way search GHOST moves.
;;;     INPUTS:  ORIG-STATE, original PM-STATE of game
;;;              DELTA-X -- amount to move GHOST in Y direction
;;;              DELTA-Y -- amount to move GHOST in Y direction
;;;     OUTPUT:  resulting PM-STATE (if the move was legal)
;;;              NIL if the move was not legal.


(defun do-move-for-search (orig-state delta-x delta-y)
  (let* ((orig-loc (pm-state-ghost-loc orig-state))
         (orig-x (first orig-loc))  ; extract ORIG-X and Y coordinates
         (orig-y (second orig-loc)) ; from orighinal location
	       (new-x (+ orig-x delta-x))  ; calculate resulting coordinates from
	       (new-y (+ orig-y delta-y))  ; ORIG-X, Y and delta-X, Y
         (orig-board (pm-state-board orig-state)) ; use COPY-ARRAY to generate
         (new-board (copy-array orig-board)))     ; NEW-BOARD from ORIG-BOARD
    ;; if the move is valid, return a new (updated) PM-STATE struct
    (if (valid-xy new-x new-y orig-board)
      (progn
        ;; update NEW-BOARD to reflect new location of search GHOST
        (decf (aref new-board orig-x orig-y) 4)
        (incf (aref new-board new-x new-y) 4)
        ;; generate updated PM-STATE struct
        (make-pm-state
          :board new-board
          :pac-loc (pm-state-pac-loc orig-state) ; PAC does not move
          :ghost-loc (list new-x new-y)))
      ;; if move is not valid - return nil
      nil)))

;;;  The UP, DOWN, LEFT, RIGHT move operators (they use DO-MOVE)
;;; --------------------------------------------------------------------
(defun up (orig-state) (do-move-for-search orig-state 0 -1))
(defun down (orig-state) (do-move-for-search orig-state 0 1))
(defun left (orig-state) (do-move-for-search orig-state -1 0))
(defun right (orig-state) (do-move-for-search orig-state 1 0))

;;; =============================================================
;;; FUNCTIONS FOR EXECUTING SEARCH - BASED ON ASMT2 VW-TEST.LISP
;;; =============================================================

;;;  MAKE-PM-PROBLEM
;;; ----------------------------------------------------
;;;   INPUTS: GAMEY, a GAME struct
;;;           INDEX, an int
;;;   OUTPUT: generates an instance of a search problem for the GHOST
;;;           at INDEX in GAMEY's list of GHOSTS

(defun make-pm-problem (gamey index)
  (make-search-problem
   :init-state (make-pm-state-from-game gamey index)
   :actions (list #'up #'down #'left #'right) ; move operators for search GHOST
   :goal-test-func #'pm-goal?
   :state-eq-func #'pm-state-equal?))

;;  DO-PM-DEPTH
;; ------------------------------------------
;;  INPUTS:  None.
;;  OUTPUT:  A RESULTS struct that contains info about what
;;           happened from running Depth-First Search on the
;;           pacman problem.

(defun do-pm-depth (game index)
 (depth-first-search (make-pm-problem game index)))

;;  DO-PM-BREADTH
;; ------------------------------------------
;;  INPUTS:  None.
;;  OUTPUT:  A RESULTS struct that contains info about what
;;           happened from running Breadth-First Search on the
;;           pacman problem.

(defun do-pm-breadth(game index)
 (breadth-first-search (make-pm-problem game index)))

;;  DO-BREADTH-MOVE
;; ------------------------------------------
;;  INPUTS:  GAME - a game struct
;;           INDEX - index of GHOST in GHOSTS
;;  OUTPUT:  performs breadth-first search to find the
;;           shortest path to PACMAN and executes the
;;           first move in the resulting path

 (defun do-breadth-move (game index)
  (let* ((ghosty (nth index (game-ghosts game)))
   (first-action (node-action (second (build-path (results-goal-node (do-pm-breadth game index))))))
   (first-move-int
     (cond
       ((equal first-action #'up) 0)
       ((equal first-action #'down) 1)
       ((equal first-action #'left) 2)
       ((equal first-action #'right) 3))))

   (do-move! game nil (ghost-x ghosty) (ghost-y ghosty) first-move-int)))

;;;  GHOST-RACE
;;; ----------------------------------------------------
;;;   INPUTS: GAMEY, a GAME struct
;;;   OUTPUT: list of ints representing the shortest path to pacman
;;;           for the GHOST at the same index in GAMEY's GHOST-list

(defun ghost-race (gamey)
(let ((acc '())) ; ACC: accumulator list for pathe lengths
 (dotimes (n (length (game-ghosts gamey)))
   ;; use breadth-first-search to find the shortest path to PACMAN
   ;; for the GHOST at index N in GHOSTS
   (setf acc
     ;; use BUILD-PATH to obtain the shortest path from the root node
     ;; (INIT-STATE) to GOAL-NODE (pacman reached)
     (cons
       (- (length (build-path (results-goal-node
            (breadth-first-search (make-pm-problem gamey n)))))
           1) ; subtract one to get the number of moves needed to reach GOAL
      acc))) ; add to accumulator list of shortest path lengths
 ;; because we CONSED results onto ACC, return REVERSED ACC to make results
 ;; match respective GHOST indices in GAMEY's list of GHOSTS.
 (reverse acc)))
