;; ====================================
;;  CMPU-365, Spring 2019
;;  Asmt. 4
;;  alpha-beta-template.lisp
;;  Feb. 28, 2019
;; ====================================

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)
  ;; Sets up the stats struct that is used in COMPUTE-MAX and COMPUTE-MIN, and
  ;; also does initial call to COMPUTE-MAX
  (let* ((statty (make-stats)))
    (setq return-val 
      (compute-max g 0 most-negative-fixnum most-positive-fixnum statty cutoff-depth))
    ;; Format output, and calculate the number of moves pruned
    (format t "   ROOT NODE ALPHA: ~A~%" (second return-val))
    (format t "   NUM-MOVES-DONE: ~A, NUM-MOVES-PRUNED: ~A~%" 
	    (stats-num-moves-done statty)
	    (- (stats-num-potential-moves statty) (stats-num-moves-done statty)))
    (format t "   My move: ~A~%" (first return-val))
    ;; Return the best move found
    (first return-val)))


;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta statty cutoff-depth)
  (cond
   ;; In the case CUTOFF-DEPTH is given as 0, return an ALPHA and
   ;; BEST-MOVE of 0, as the initial game state, G, is the only
   ;; one looked at and nothing can be assumed.
   ((= cutoff-depth 0)
    (list 0 0))
   ;; In the case CUR-DEPTH is 0, return a list containing MY-MOVE,
   ;; the best-move-so-far, and its ALPHA value
   ((= curr-depth 0)
    ;; Initializes MOVES, a list of all legal moves for G, MY-MOVE,
    ;; and CUR-SCORE, the value of the last move done
    (let* ((moves (legal-moves g))
	   (my-move 0)
	   (cur-score 0))
      ;; Update STATTY's NUM-POTENTIAL-MOVES
      (setf (stats-num-potential-moves statty)
	(+ (list-length moves) (stats-num-potential-moves statty)))
      ;; Loop through every MOVE in MOVES, doing each move, updating the
      ;; number of moves done, setting CUR-SCORE to the best value of the
      ;; next depth using COMPUTE-MIN, undoing the move, and updating 
      ;; ALPHA and MY-MOVE as appropriate
      (loop for move in moves
	  do
	    ;; Do move
	    (apply #'do-move! g nil move)
	    ;; Update NUM-MOVES-DONE
	    (setf (stats-num-moves-done statty)
	      (+ (stats-num-moves-done statty) 1))
	    ;; Set CUR-SCORE to value of COMPUTE-MIN at one depth greater
	    (setq cur-score 
          (compute-min g (+ curr-depth 1) alpha beta statty cutoff-depth))
	    ;; Undo move
	    (undo-move! g)
	    ;; Update ALPHA and MY-MOVE if CUR-SCORE is a value greater than
	    ;; the current ALPHA
	    (if (< alpha cur-score)
		(progn
		  (setq alpha cur-score)
		  (setq my-move move))))
     ;; Return the list with MY-MOVE and ALPHA for use in COMPUTE-MOVE
      (list my-move alpha)))
   ;; In the case the game is over, return the value of the win/loss
   ((game-over? g) (+ *loss-value* curr-depth))
   ;; In the case CUTOFF-DEPTH is reached, use EVAL-FUNC to calculate its value
   ;; for use in higher levels of recurssion
   ((= curr-depth cutoff-depth) 
    (eval-func g))
   ;; Otherwise, do each possible move for G, finding its optimal value using
   ;; COMPUTE-MIN at the next depth, undo the move, update ALPHA as needed, and
   ;; if possible prune excess moves
   (t
    ;; Initalizes MOVES, a list of all legal moves for G, and CUR-SCORE, the
    ;; current score of a move done in G
    (let* ((moves (legal-moves g))
	   (cur-score 0))
      ;; Update NUM-POTENTIAL-MOVES in STATTY
      (setf (stats-num-potential-moves statty) 
	(+ (list-length moves) (stats-num-potential-moves statty)))
      ;; For every MOVE in MOVES...
      (dolist (move moves)
	;; ...do MOVE
	(apply #'do-move! g nil move)
	;; ...update CUR-SCORE to the value found one depth down in COMPUTE-MIN
	(setq cur-score 
      (compute-min g (+ curr-depth 1) alpha beta statty cutoff-depth))
	;; ...undo the move
	(undo-move! g)
;	(format t "!!!!! ~a ~a ~a ~%" alpha cur-score move)
	;; ...update NUM-MOVES-DONE in STATTY
	(setf (stats-num-moves-done statty)
	  (+ (stats-num-moves-done statty) 1))
	;; ...update ALPHA to the MAX of ALPHA and CUR-SCORE
	(setq alpha (max alpha cur-score))
	;; ...in the case BETA is less than of equal to ALPHA, prune the remaining
	;; possible moves
	(if (<= beta alpha)
	    (return-from compute-max alpha)))
      ;; Return ALPHA
      alpha))))
		 
 

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the depth of this MIN node
;;           ALPHA, BETA, values received from parent MAX node
;;           STATTY, a stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth)
  (cond
    ;; In the case the game is over, return the value of the win/loss
   ((game-over? g) (- *win-value* curr-depth))
     ;; If if is whites turn, that indicates a win, as black lost, and a win
    ;; value is returned
;    (if (= (chess-whose-turn? g) *white*)
;	(- *win-value* curr-depth)
      ;; Otherwise a loss is indicated, and a loss value is returned
;      (+ *loss-value* curr-depth)))
   ;; In the case CUTOFF-DEPTH is reached, use EVAL-FUNC to calculate its value
   ;; for use in higher levels of recurssion
   ((= curr-depth cutoff-depth) (eval-func g))
   ;; Otherwise, do each possible move for G, finding its optimal value using
   ;; COMPUTE-MAX at the next depth, undo the move, update BETA as needed, and
   ;; if possible prune excess moves
   (t
    ;; Initalizes MOVES, a list of all legal moves for G, and CUR-SCORE, the
    ;; current score of a move done in G
    (let* ((moves (legal-moves g))
	   (cur-score 0))
      ;; Update NUM-POTENTIAL-MOVES in STATTY
      (setf (stats-num-potential-moves statty) 
	(+ (list-length moves) (stats-num-potential-moves statty)))
      ;; For every MOVE in MOVES...
      (dolist (move moves)
	;; ...do MOVE
	(apply #'do-move! g nil move)
	;; ...update CUR-SCORE to the value found one depth down in COMPUTE-MAX
	(setq cur-score 
      (compute-max g (+ curr-depth 1) alpha beta statty cutoff-depth))
	;; ...undo the move
	(undo-move! g)
	;; ...update NUM-MOVES-DONE in STATTY
	(setf (stats-num-moves-done statty)
	  (+ (stats-num-moves-done statty) 1))
	;; ...update BETA to the MIN of BETA and CUR-SCORE
	(setq beta (min beta cur-score))
	;; ...in the case ALPHA is greater than of equal to BETA, prune the
	;; remaining possible moves
	(if (>= alpha beta)
	    (return-from compute-min beta)))
      ;; Return BETA
      beta))))


;;  MY-TEST
;; -------------------------------
;;  INPUTS: none
;;  OUTPUT: none
;;  SIDE EFFECTS: White should have checkmate in two moves,
;;     first the white knight should move to (2 1), black
;;     will then move the rook, and then white rook to (4 2)
;;     for checkmate. For the actual full capture, the black
;;     king will then move to (5 2), where the white king
;;     will capture it.

(defun my-test ()
   (problem "MY-MATE-IN-TWO")
   (let ((g (init-game (list
			(list *pawn* 4 1)
			(list *rook* 5 2)
			(list *knight* 3 3)
			(list *king* 6 3)
			(list *pawn* 2 4)
			(list *pawn* 5 4)
			(list *knight* 0 5))
		       (list
			(list *king* 4 3)
			(list *pawn* 3 4)
			(list *rook* 4 4)
			(list *bishop* 3 5)))))
    (compute-do-and-show-n-moves g 5 6)
    (format t "White should have taken black's king by now!~%")
    (format t "Game over? ~A~%~%~%" (game-over? g))))
