;; ========================================
;;     FILE:  rubik.lisp
;;   AUTHOR:  luke hunsberger
;;     DATE:  spring 2019
;; ========================================

;;  GLOBAL CONSTANTS

;;  The faces of a cube point in the X, Y and Z directions (plus or minus)

(defconstant *x* 0)
(defconstant *y* 1)
(defconstant *z* 2)

;;  The faces of each cubie are labeled A, B, C, X, Y and Z.
;;  The A and X faces originally point in the X directions;
;;  The B and Y faces originally point in the Y directions (plus or minus);
;;  etc.

;;  *FACES* is a fixed vector of vectors.
;;  See description of RUBIK struct for more info on orientation of
;;  the original cube.

(defconstant *faces* #(#(a b z) #(a y c) #(a y z) #(x b c) #(x b z) 
		       #(x y c) #(x y z)))

;;  RUBIK struct
;; ---------------------------------------
;;  Cubes are numbered 0 .. 6, so are positions for cubes.  Positions
;;  of cubes is a 7-vector.  Orientation of cubes is also a 7-vector.
;;  Each cube has faces in X, Y and Z dimensions.  Orientation of each
;;  cube is determined by the orientation its face that is supposed to
;;  be in the X direction.  Note: Represent x, y and z directions by
;;  0, 1, and 2.

;;  The locations specified by the indices 0 .. 6 are:
;;     --------/|         Z
;;    / 0 / 2 / |         ^
;;   /-------/|2/         |
;;  / 4 / 6 / |/|         |
;;  --------|6/ |         |
;;  | 4 | 6 |/|1/         /-------> Y
;;  |-------| |/         /
;;  | 3 | 5 |5/         /
;;  --------|/         X

(defstruct (rubik (:print-function alt-show-rubik))
  ;; by default, each cube is in its proper location, and each
  ;; cube's X face is oriented in the X direction.
  ;; ---------------------------------------------------------
  ;; NOTE:  Use (list 0 .. 6) rather than '(0 .. 6) because
  ;;        we want each new instance to have a brand new list.
  ;;        Don't want modifications to one rubik's cube to 
  ;;        affect another.
  (positions (make-array 7 :initial-contents (list 0 1 2 3 4 5 6)))
  (orientations (make-array 7 :initial-contents (list 0 0 0 0 0 0 0))))

;;  COPY-RUBE
;; ---------------------------------------
;;  INPUT:  RUBE, a rubik struct
;;  OUTPUT:  A new RUBIK struct having the same positions and orientations
;;           as RUBE.  (The data are COPIED!!)

(defun copy-rube (rube)
  ;; first, create a new rubik's cube instance.
  (let ((r (make-rubik)))
    ;; DOLIST:  INDY takes on values 0 .. 6
    ;;  Copy the contents of the POSITIONS and ORIENTATIONS vectors
    (dolist (indy '(0 1 2 3 4 5 6))
      ;; copy position
      (setf (svref (rubik-positions r) indy) 
	(svref (rubik-positions rube) indy))
      ;; copy orientation
      (setf (svref (rubik-orientations r) indy) 
	(svref (rubik-orientations rube) indy)))
    ;; return the copy
    r))

;;  GET-FACE
;; ---------------------------
;;  INPUTS:  RUBE, a rubik struct
;;           POSN, an integer from 0 to 6 inclusive
;;           DIRN, one of *X*, *Y* or *Z* (i.e., 0, 1 or 2)
;;  OUTPUT:  The "color" of the specified face of the specified cube
;;           as a symbol: A, X, B, Y, C or Z.
;;    X is the color that's supposed to be in the +x direction;
;;    A is the color that's supposed to be in the -x direction;
;;    Y is the color that's supposed to be in the +y direction;
;;    B is the color that's supposed to be in the -y direction; etc.
;; --------------------------------
;;   Cubes 0, 1, 3 and 6 have their X/Y/Z faces oriented counter-clockwise 
;;   Cubes 2, 4 and 5 have their X/Y/Z faces oriented clockwise
;;    (The nailed-down cube also has its faces oriented clockwise.)

(defun get-face (rube posn dirn)
  (let* (;; index = cube # that currently resides at position POSN
	 (index (svref (rubik-positions rube) posn))
	 ;; oreo = orientation of that cube 
	 ;;        (i.e., where's its x face is pointing)
	 (oreo (svref (rubik-orientations rube) posn))
	 ;; match:  posn is same type (C or CC) as cube at that posn
	 (match (or (and (find posn #(0 1 3 6))
			 (find index #(0 1 3 6)))
		    (and (find posn #(2 4 5))
			 (find index #(2 4 5)))))
	 ;; indy: offset to determine which face to show
	 (indy (mod (- dirn oreo) 3))
	 ;; which-face:  index (0, 1 or 2) saying which face to show
	 (which-face (svref (if match #(0 1 2) #(0 2 1)) indy)))
    (svref (svref *faces* index) which-face))) 
    

;;  ALT-SHOW-RUBIK
;; -----------------------------------------
;;  INPUT:  RUBE, a rubik struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the rubik's cube, but only showing visible cubes.
;;  Note:  The unscrambled cube looks like this:
;;
;;     --------/|
;;    / Z / Z / |
;;   /-------/|Y/
;;  / Z / Z / |/|
;;  --------|Y/ |
;;  | X | X |/|Y/
;;  |-------| |/
;;  | X | X |Y/
;;  --------|/
;;
;;  The faces labeled A, B and C are hidden in this example.

(defun alt-show-rubik (rube str depth)
  (declare (ignore str depth))
  (format t "     --------/|~%")
  (format t "    / ~A / ~A / |~%" 
	  (get-face rube 0 *z*) 
	  (get-face rube 2 *z*))
  (format t "   /-------/|~A/~%" 
	  (get-face rube 2 *y*))
  (format t "  / ~A / ~A / |/|~%" 
	  (get-face rube 4 *z*) 
	  (get-face rube 6 *z*))
  (format t "  --------|~A/ |~%" 
	  (get-face rube 6 *y*))
  (format t "  | ~A | ~A |/|~A/~%" 
	  (get-face rube 4 *x*) 
	  (get-face rube 6 *x*)
	  (get-face rube 1 *y*))
  (format t "  |-------| |/~%")
  (format t "  | ~A | ~A |~A/~%" 
	  (get-face rube 3 *x*) 
	  (get-face rube 5 *x*)
	  (get-face rube 5 *y*))
  (format t "  --------|/~%")
  )

;;  SHOW-CUBIT -- Helper function for SHOW-RUBIK, below
;; ------------------------------------------------------
;;  INPUT:  SPACES, a string of spaces
;;          RUBE, a rubik struct
;;          INDY, an integer from 0 to 6 inclusive
;;  OUTPUT:  None
;;  SIDE EFFECT:  Prints out the spaces, followed by information
;;    about the cube currently occupying position INDY in RUBE---
;;    namely, the cube number and its orientation.

(defun show-cubit (spaces rube indy)
  (format t "~A~A_~A" spaces 
	  (svref (rubik-positions rube) indy)
	  (svref (rubik-orientations rube) indy)))


;;  SHOW-RUBIK
;; -----------------------------------------
;;  INPUT:  RUBE, a rubik struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the information about the given rubik's cube. 
;;     Uses SHOW-CUBIT for each cubit.

(defun show-rubik (rube)
  (show-cubit "    " rube 0) 
  (show-cubit "  " rube 2) (newline) (newline)
  (show-cubit "" rube 4)
  (show-cubit "  " rube 6) (newline)
  (show-cubit "         " rube 1) (newline)
  (show-cubit "" rube 3)
  (show-cubit "  " rube 5) (newline) (newline))


;;  VECTOR-EQUAL
;; ---------------------------------------
;;  INPUTS:  VEC1, VEC2:  two vectors of length 7
;;  OUTPUT:  T if the given vectors have equal contents;
;;           NIL otherwise.

(defun vector-equal (vec1 vec2)
  (and (= (svref vec1 0) (svref vec2 0))
       (= (svref vec1 1) (svref vec2 1))
       (= (svref vec1 2) (svref vec2 2))
       (= (svref vec1 3) (svref vec2 3))
       (= (svref vec1 4) (svref vec2 4))
       (= (svref vec1 5) (svref vec2 5))
       (= (svref vec1 6) (svref vec2 6))))


;;  RUBE-EQUAL
;; ---------------------------------------
;;  INPUTS:  RUBE1, RUBE2:  two RUBIK structs
;;  OUTPUT:  T if the given rubik's cubes have identical states.

(defun rube-equal (rube1 rube2)
  (and 
   (vector-equal (rubik-positions rube1) (rubik-positions rube2))
   (vector-equal (rubik-orientations rube1) (rubik-orientations rube2))))
  

;; -------------------------------------------
;;  Operators for modifying Rubik's Cube
;; -------------------------------------------

;;  PERMUTE-LEFT! --  DESTRUCTIVE!!
;; ------------------------------------
;;  INPUTS:  VEC, a vector
;;           LISTY, a list of indices representing a permutation
;;  OUTPUT:  Destructively permutes the contents of VEC as specified
;;           by LIST

;;  Example:  (permute-left! vec '(1 3 5)) causes the items of
;;   VEC at indices 1, 3 and 5 to be permuted as follows:
;;        v[1] <-- v[3] <-- v[5] <---
;;         |                        |
;;         --------------------------
;;   Thus, if VEC's original contents were #(a b c d e f g)
;;   they would later be:  #(a d c f e b g)
;;   Notice that items at indices other than 1, 3 and 5 were 
;;   not affected.

(defun permute-left!(vec listy)
  (when (rest listy)
    (let* ((i (first listy))
	   (old-firsty (svref vec i)))
      (dolist (j (rest listy))
	(setf (svref vec i) (svref vec j))
	(setf i j))
      (setf (svref vec i) old-firsty))))

;;  PERMUTE-LEFT-RUBE   --  non-destructive
;; --------------------------------------------------------------------
;;  INPUTS:  ORIG-RUBE, a rubik struct
;;           LISTY, a list specifying a permutation of the cubies
;;             in the rubik's cube.
;;           FACE-CHANGES, a list of length 3 specifying 
;;             how the orientation of a cube changes due
;;             to the rotation (e.g., (0 2 1) would specify
;;             that the X face had not changed, but that the
;;             Y and Z faces had moved:  a face formerly pointing
;;             in the +/- Y direction would now point in the
;;             +/- Z direction, and vice versa).
;;  OUTPUT:  A NEW rubik's cube that results from applying the
;;           specified permutation to the original rubik's cube's cubes,
;;           and adjusting the orientation of the cubies that moved.

(defun permute-left-rube (orig-rube listy face-changes)
  (let* ((rube (copy-rube orig-rube))
	 (ornz (rubik-orientations rube)))
    ;; First, permute the POSITIONS of the rubik's cube
    (permute-left!(rubik-positions rube) listy)
    ;; Then, permute the ORIENTATIONS
    (permute-left!(rubik-orientations rube) listy)
    ;; Finally, adjust the orientations of the cubes that moved
    (dolist (indy listy)
      ;; Set the orientation of the cubie at index INDY
      (setf (svref ornz indy)
	;; from its old orientation to its new orientation
	(nth (svref ornz indy) face-changes)))
    rube))

;;  Individual move operators --  non-destructive!!
;;;
;;   ROTATE-X-CLOCKWISE, ROTATE-X-COUNTER, ROTATE-Y-CLOCKWISE, etc.

(defun rotate-x-clockwise (rube)
  (permute-left-rube rube '(4 3 5 6) '(0 2 1)))

(defun rotate-x-counter (rube)
  (permute-left-rube rube '(6 5 3 4) '(0 2 1)))

(defun rotate-y-clockwise (rube)
  (permute-left-rube rube '(6 5 1 2) '(2 1 0)))

(defun rotate-y-counter (rube)
  (permute-left-rube rube '(2 1 5 6) '(2 1 0)))

(defun rotate-z-clockwise (rube)
  (permute-left-rube rube '(0 4 6 2) '(1 0 2)))

(defun rotate-z-counter (rube)
  (permute-left-rube rube '(2 6 4 0) '(1 0 2)))


;; *RUBE-OPS*  -- The three clockwise operators

(defparameter *rube-ops* (list #'rotate-x-clockwise 
			       #'rotate-y-clockwise 
			       #'rotate-z-clockwise))

;; *RUBE-COUNTER-OPS*  -- The three counterclockwise operators

(defparameter *rube-counter-ops* (list #'rotate-x-counter
				       #'rotate-y-counter
				       #'rotate-z-counter))

;;  RANDOM-RUBE-COUNTER-OP
;; -----------------------------------------
;;  INPUT:  none
;;  OUTPUT:  One of the counterclockwise operators, selected 
;;           at random

(defun random-rube-counter-op 
    () 
  (nth (random 3) *rube-counter-ops*))

;;  MAKE-RUBE-PROBLEM
;; ------------------------------------
;;  INPUT:  Only one optional input:  STATE, the initial state
;;            By default, it is the unscrambled cube
;;  OUTPUT:  A PROBLEM struct using the CLOCKWISE operators
;;            and the specified initial state

(defun make-rube-problem (&optional (state (make-rubik)))
  (make-search-problem :init-state state
		:actions *rube-ops*
		:goal-test-func 
		#'(lambda (rube)
		    (and
		     (vector-equal #(0 1 2 3 4 5 6) (rubik-positions rube))
		     (vector-equal #(0 0 0 0 0 0 0) (rubik-orientations rube))
		     ))
		:state-eq-func #'rube-equal
		))

;;  MAKE-RANDOM-RUBE-PROBLEM
;; ---------------------------------------
;;  Same as above except that it random scrambles the cube NUM times

(defun make-random-rube-problem (&optional (num 10))
  (make-rube-problem (scramble-rube num)))

;;  SCRAMBLE-CUBE
;; --------------------------------------------
;;  INPUT:  NUM, a non-negative integer
;;  OUTPUT:  A rubik's cube obtained by randomly applying NUM 
;;            *counterclockwise* moves

(defun scramble-rube (num)
  (format t "Scrambling Cube!~%")
  (let ((rube (scramble-rube-hlpr (make-rubik) num)))
    (newline)
    ;; return the cube!
    rube))

;;  SCRAMBLE-RUBE-HLPR
;; ------------------------------------------
;;  INPUTS:  RUBE, a rubik's struct
;;           NUM, a non-negative integer
;;  OUTPUT:  The rubik's cube that results from applying NUM 
;;           randomly selected counterclockwise operations

(defun scramble-rube-hlpr (rube num)
  (if (<= num 0) 
      rube
    (let ((rnd-op (random-rube-counter-op)))
      (format t "     Random OP: ~A~%" rnd-op)
    (scramble-rube-hlpr (funcall rnd-op rube) (1- num)))))


;;  DO-RUBE-IDS
;; -------------------------------------------
;;  Performs Iterative Deepening Search on a randomly
;;  scrambled rubik's cube.
;;  OPTIONAL INPUTS:
;;    NUM-RND:  The number of randomly generated counterclockwise
;;                ops to apply to generate a scrambled cube
;;    BIG-LIMIT:  A cutoff for the IDS search (useful for debugging)
;;      not currently used
  
(defun do-rube-ids (&optional (num-rnd 3) (big-limit 9))
  (let* ((prob (make-random-rube-problem num-rnd))
	 (result (ids prob)))
    (if (node-p result) 
	result 
      (format t "Whoops... No solution found... NUM-RND = ~A, BIG-LIMIT = ~A~%"
	      num-rnd big-limit))))


;;  TWO-CORNERS-SWAPPED --  a GOAL-TEST function
;; -----------------------------------------------
;;  INPUT:  RUBY, a rubik's cube
;;  OUTPUT:  T if RUBY is the same as an unscrambled cube
;;     except that cubes at positions 4 and 6 have been swapped.
;;     All cubes must be in their original orientation, except
;;     we don't care about the orientations of cubes 4 and 6.

(defun two-corners-swapped
    (ruby)
  (let ((locs (rubik-positions ruby))
	(oreos (rubik-orientations ruby)))
    ;; swap cubes at indices 4 and 6
    (and (vector-equal locs #(0 1 2 3 6 5 4))
	 ;; orientations at indices 4 and 6 are don't care
	 (= 0 
	    (aref oreos 0)
	    (aref oreos 1)
	    (aref oreos 2)
	    (aref oreos 3)
	    (aref oreos 5)))))


;;  THREE-CORNERS-ROTATED  --  a GOAL-TEST function
;; ---------------------------------------------------------
;;  INPUT:  RUBY, a rubik's cube
;;  OUTPUT:  T, if all the cubes of RUBY are in their unscrambled
;;    locations with their unscrambled orientations, except that
;;    cubes 4, 5 and 6 have been rotated in the clockwise direction
;;    (while still in their original locations).

(defun three-corners-rotated
    (ruby)
  (let ((locs (rubik-positions ruby))
	(oreos (rubik-orientations ruby)))
    (and 
     ;; original locations
     (vector-equal locs #(0 1 2 3 4 5 6))
     ;; original orientations for cubes 0 thru 3
     (= 0 
	(aref oreos 0)
	(aref oreos 1)
	(aref oreos 2)
	(aref oreos 3))
     ;; cubes 4, 5 and 6 rotated clockwise
     (= (aref oreos 4) *y*)
     (= (aref oreos 5) *y*)
     (= (aref oreos 6) *z*))))


;;  MAKE-FANCY-RUBE-PROBLEM
;; --------------------------------------------------
;;  INPUT:  GOAL-TEST-FUNC, a goal-test function for 
;;                    a Rubik's cube
;;          ACTIONS, a list of allowed actions on Rubik's
;;                    cubes
;;  OUTPUT:  A SEARCH-PROBLEM instance where the initial
;;    state is the unscrambled Rubik's cube, the goal
;;    state function is given by GOAL-TEST-FUNC, and the
;;    allowed actions are given by ACTIONS.

(defun make-fancy-rube-problem
    (goal-test-func actions)
  (let ((prob (make-rube-problem)))
    (setf (search-problem-goal-test-func prob) goal-test-func)
    (setf (search-problem-actions prob) actions)
    prob))

  
