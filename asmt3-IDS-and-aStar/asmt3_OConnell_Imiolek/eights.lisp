;; ===================================
;;  CMPU-365, Spring 2019
;;  Asmt. 3
;;  EIGHTS.LISP
;; ===================================
;;  An implementation of the eights tile puzzle

;; The underscore is used when printing out a blank;
;; The number 0 is used as a blank within the eights struct.

(defparameter +blank-symbol+ '_)
(defparameter +blank-num+ 0)

;; The EIGHTS data structure

(defstruct (eights (:print-function show-eights))
  (locations (make-array '(3 3) :initial-contents 
			 '((0 1 2) (3 4 5) (6 7 8))))
  ;; The position of the blank is given by:
  (blank-row 0)
  (blank-col 0))

;; GOAL-GAME is used by the goal-testing function, EIGHTS-GOAL?.
;; GOAL-ARRAY is the array that appears in the GOAL-GAME struct.

(defparameter goal-array (make-array '(3 3) 
				     :initial-contents
				     '((1 2 3) (8 0 4) (7 6 5))))

(defparameter goal-game (make-eights :locations goal-array
				     :blank-row 1
				     :blank-col 1))

;;  INIT-EIGHTS
;; ----------------------------------------
;;  INPUTS:   None
;;  OUTPUT:   An eight-tile puzzle in the goal configuration

(defun init-eights
    ()
  (make-eights :locations (make-array '(3 3)
				      :initial-contents
				      '((1 2 3) (8 0 4) (7 6 5)))
	       :blank-row 1
	       :blank-col 1))

;;  EIGHTS-EQUAL?
;; ---------------------------------------------------
;;  INPUTS:  GAME1, GAME2, two instances of an EIGHTS struct
;;  OUTPUT:  T if the two games have all of their tiles in the
;;              same positions.

(defun eights-equal? (game1 game2)
  (let* ((locs1 (eights-locations game1))
	 (locs2 (eights-locations game2))
	 (size1 (array-dimensions locs1))
	 (size2 (array-dimensions locs2)))
    (cond
     ;; Case 1:  The blanks are in the same positions
     ;;          and the puzzles have the same sizes
     ((and (= (eights-blank-row game1)
	      (eights-blank-row game2))
	   (= (eights-blank-col game1)
	      (eights-blank-col game2))
	   (equal size1 size2))
      ;; Walk through the different tile locations
      (dotimes (i (first size1))
	(dotimes (j (second size1))
	  ;; If we find a location having different tiles
	  (when (not (= (aref locs1 i j) 
			(aref locs2 i j)))
	    ;; then immediately return NIL
	    (return-from eights-equal? nil))))
      ;; If we make it to here, then all locations must've had
      ;; the same tiles...
      t)
     
     ;; Case 2:  The blanks are in different locations or the
     ;;          puzzle dimensions are different
     (t
      ;; So, they can't be equal!
      nil))))


;;  EIGHTS-GOAL?
;; -------------------------------------------------
;;  INPUT:  GAME, an eights tile puzzle
;;  OUTPUT:  T if the game is equal to the goal state

(defun eights-goal? (game)
  (eights-equal? game goal-game))


;;  SHOW-EIGHTS
;; ----------------------------------------------------
;;  INPUTS:  GAME, an eights tile puzzle
;;           STR, an output stream (probably just T)
;;           DEPTH, ignored
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the given game in the interactions window.

(defun show-eights (game str depth)
  (declare (ignore depth))
  (let ((locs (eights-locations game)))
    (format str "~%")
    ;; Walk through the rows and columns of the puzzle
    (dotimes (row 3)
      (dotimes (col 3)
	;; display the tile at the current location
	(let ((tile (aref locs row col)))
	  (format str "~A " (if (= tile +blank-num+) +blank-symbol+ tile))))
      (format str "~%"))))

;;  ON-BOARD
;; -------------------------------------------------------------
;;  INPUTS:  ROW, COL, integers specifying a location on a tile puzzle
;;  OUTPUT:  T if (ROW,COL) is a legal position (i.e., on the board)

(defun on-board (row col)
  (and (>= row 0)
       (< row 3)
       (>= col 0)
       (< col 3)))

;;  COPY-ARRAY
;; -------------------------------------------------------------
;;  INPUTS:  ARRIE, a two-dimensional array
;;  OUTPUT:  A copy of ARRIE

(defun copy-array (arrie)
  (let (;; NEW-ARRAY:  this will be the copy
	(new-array (make-array (array-dimensions arrie))))
    ;; Walk through the rows and columns of the arrays
    (dotimes (row (array-dimension arrie 0))
      (dotimes (col (array-dimension arrie 1))
	;; Copy corresponding elements...
	(setf (aref new-array row col)
	  (aref arrie row col))))
    ;; Return the NEW-ARRAY
    new-array))

;;  DO-BLANK-MOVE
;; -------------------------------------------------------
;;  INPUTS:  GAME, an eights tile puzzle
;;           ROW-DELTA, COL-DELTA, the direction in which the
;;               blank should move
;;  OUTPUT:  The resulting GAME (i.e., the resulting state)
;;           or NIL (if the move would be illegal)

(defun do-blank-move (game row-delta col-delta)
  (let* ((old-row (eights-blank-row game))
	 (old-col (eights-blank-col game))
	 (new-row (+ old-row row-delta))
	 (new-col (+ old-col col-delta)))
    (cond
     ;; Case 1:  It would be a legal move
     ((on-board new-row new-col)
      (let* ((old-locs (eights-locations game))
	     ;; A copy of the locations array
	     (new-locs (copy-array old-locs))
	     ;; Create a new game struct
	     (new-game (make-eights 
		       :locations new-locs
		       :blank-row new-row
		       :blank-col new-col))
	     (tile-being-moved (aref old-locs new-row new-col)))
	;; move blank to new location (and move tile that was
	;; there to blank's old location)
	(setf (aref new-locs old-row old-col) tile-being-moved)
	(setf (aref new-locs new-row new-col) +blank-num+)
	;; return the new GAME struct
	new-game))
     
     ;; Case 2:  Illegal Move
     (t
      ;; Return NIL
      nil))))

;;  BLANK-NORTH, BLANK-SOUTH, BLANK-EAST, BLANK-WEST
;; -------------------------------------------------------------
;;  INPUT:  GAME, an eights tile puzzle
;;  OUTPUT:  The resulting puzzle (EIGHTS struct) if the move
;;            was legal; otherwise NIL.

(defun blank-north (game) (do-blank-move game -1 0))
(defun blank-south (game) (do-blank-move game 1 0))
(defun blank-west (game) (do-blank-move game 0 -1))
(defun blank-east (game) (do-blank-move game 0 1))


;;  NUM-TILES-OUT
;; ----------------------------------------------
;;  INPUT:  GAME, an eights tile puzzle
;;  OUTPUT:  The number of tiles in GAME that are not
;;           in their goal positions.
;;  NOTE:  The blank is not counted.

(defun num-tiles-out (game)
  (let* ((locs (eights-locations game))
	 (counter 0))
    ;; For each location (row,col)...
    (dotimes (row 3)
      (dotimes (col 3)
	;; Only deal with actual tiles, not the blank
	(when (and (not (= (aref locs row col) +blank-num+))
		   ;; tile's location is not in its goal location
		   (not (eq (aref locs row col) 
			    (aref goal-array row col))))
	  (incf counter))))
    ;; Return the counter
    counter))

;;  NUM-TILES-OUT-HEURISTIC
;; --------------------------------------------------------
;;  Same as NUM-TILES-OUT, except that it applies to NODES, not STATES.

(defun num-tiles-heuristic (node)
  (num-tiles-out (node-state node)))
  
;;  MAKE-EIGHTS-PROBLEM
;; ---------------------------------------------------
;;  INPUTS:  LOCS, a list-of-list representation of an eights puzzle,
;;                 where a blank appears as a 0
;;           BX, the blank's row in LOCS
;;           BY, the blank's column in LOCS
;;  OUTPUT:  A search-problem struct for the given eights tile puzzle.

(defun make-eights-problem (locs bx by)
  (make-search-problem 
   :init-state (make-eights :locations (make-array '(3 3) 
						   :initial-contents
						   locs)
			    :blank-row bx
			    :blank-col by)
   :actions (list #'blank-north #'blank-south
		  #'blank-east #'blank-west)
   :goal-test-func #'eights-goal?
   :state-eq-func #'eights-equal?))

(defun make-eights-prob-v2 (init-state)
  (make-search-problem
   :init-state init-state
   :actions (list #'blank-north #'blank-south #'blank-east #'blank-west)
   :goal-test-func #'eights-goal?
   :state-eq-func #'eights-equal?))
