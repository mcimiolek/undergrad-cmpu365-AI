;; =======================================================================
;;   CMPU-365, Spring 2019
;;   Final Project
;;   Matthew Imiolek, Amy O'Connell
;;   alpha-beta.lisp
;;   Alpha-Beta search with minimax for Pacman
;; =======================================================================

;; Defines the most negative number possible
(defconstant *neg-inf* most-negative-fixnum)
;; Defines the most positive number possible
(defconstant *pos-inf* most-positive-fixnum)
;; Defines the loss value
(defconstant *loss-value* -1000000)
;; Defines the win value
(defconstant *win-value* 1000000)

;;  COMPUTE-MOVE
;; ----------------------------------------------------------------------
;;  Selects the best move using Alpha-beta search with MINIMAX
;;
;;  INPUTS: G, a GAME struct
;;          CUTOFF-DEPTH, the limited depth of minimax search
;;  OUTPUT: The best move according to MINIMAX with ALPHA-BETA pruning,
;;          using the static eval func, EVAL-FUNC. Searches to a depth of
;;          CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth)
  (format t "Compute Move (cutoff: ~A)~%" cutoff-depth)
  (cond
   ;; Case 1:  Game already over
   ((game-over? g)
    (format t "Game is over!~%")
    nil)
   ;; Case 2:  Game still going
   (t
    ;; Call COMPUTE-MAX with init alpha/beta values
    (let* ((best-move (compute-max g 0 *neg-inf* *pos-inf* cutoff-depth)))
      ;; Report number of moves considered...
      (format t "   BEST MOVE: ~A~%" best-move)
      best-move))))

;;  COMPUTE-MAX
;; ----------------------------------------------------------------------
;;  Computes the best move for the given person
;;
;;  INPUTS:  G, a GAME struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move, otherwise returns value
;;           of this node according to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta cutoff-depth)
  (let
      ((best-move-so-far -1)
       (turn (game-whose-turn? g)))
    (cond
      ;; Base Case 1:  Game over
      ((game-over? g)
       ;; return the loss-value and the current depth to prefer later losses
      (game-over-calc g curr-depth))
      ;; Base Case 2:  We're at the cutoff depth
      ((>= curr-depth cutoff-depth)
       ;; Use the static evaluation func
       (eval-func g))
      ;; Recursive Case 1: Need to do minimax with alpha-beta pruning for Pacman
      ((= turn *pac*)
       (let* ((moves (legal-moves g))
              (start-x (pacman-x (game-pac g)))
              (start-y (pacman-y (game-pac g))))
         (dolist (mv moves)
                 (do-move! g nil start-x start-y mv)
                 (toggle-turn! g)
                 (let ((child-val (compute-min g (1+ curr-depth) alpha beta cutoff-depth)))
                   (toggle-turn! g)
                   (undo-move! g)
                   ;; Check for updating ALPHA value...
                   (when (> child-val alpha)
                     (setf alpha child-val)
                     (setf best-move-so-far mv)
                     ;; Check for pruning ...
                     (when (<= beta alpha)
                       ;; Hey! PRUNE!  Forget about any remaining moves in
                       ;;  this DOLIST... We're outta here!!
                       (return-from compute-max
                                    ;; Need to return BEST-MOVE if we're at depth 0
                                    ;; Otherwise return ALPHA value
                                    (cond ((zerop curr-depth)
                                           best-move-so-far)
                                          (t
                                           alpha)))))))
         ;; return alpha or best-move-so-far, depending on whether
         ;; we're at depth 0 or not
         (cond
           ((zerop curr-depth)
            best-move-so-far)
           (t
            alpha))))
      (t
       ;;(ADD THING THAT ACTUALLY MOVES HERE)
       (let*
           ((list-of-move-lists (legal-moves g))
            (move-combos (gen-move-combos (rest list-of-move-lists)))
            (ghosts (rest (game-ghosts g))))
         (dolist (moves move-combos)
                 (dotimes (z (length moves))

                          (do-move! g nil (ghost-x (nth z ghosts)) (ghost-y (nth z ghosts)) (nth z moves)))
                 (toggle-turn! g)
                 (let* ((child-val (compute-min g (1+ curr-depth) alpha beta cutoff-depth)))
                   (toggle-turn! g)
                   (dotimes (i (length moves))
                            (undo-move! g))
                   (when (> child-val alpha)
                            (setf alpha child-val)
                            (setf best-move-so-far moves)
                   ;; Check for pruning ...
                            (when (<= beta alpha)
                     ;; Hey! PRUNE!  Forget about any remaining moves in
                     ;;  this DOLIST... We're outta here!!
                                  (return-from compute-max
                                  ;; Need to return BEST-MOVE if we're at depth 0
                                  ;; Otherwise return ALPHA value
                                          (cond ((zerop curr-depth)
                                                       best-move-so-far)
                                                (t
                                         alpha)))))))
         (cond
           ((zerop curr-depth)
            best-move-so-far)
           (t
            alpha)))))))

;;  COMPUTE-MIN
;; ----------------------------------------------------------------------
;;  Helps compute the best move for the given person
;;
;;  INPUTS:  G, a GAME struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move, otherwise returns value
;;           of this node according to MINIMAX with ALPHA-BETA pruning.

(defun compute-min (g curr-depth alpha beta cutoff-depth)
(let* ((turn (game-whose-turn? g)))
  (cond
   ;; Base Case 1:  Game over
   ((game-over? g)
    ;; return the loss-value and the current depth to prefer later losses
    (game-over-calc g curr-depth))
   ;; Base Case 2:  We're at the cutoff depth
   ((>= curr-depth cutoff-depth)
    ;; Use the static evaluation func
    (eval-func g))
   ;; Recursive Case 1: Need to do minimax with alpha-beta pruning for Pacman
   ((= turn *pac*)
   (let* ((moves (legal-moves g))
    (start-x (pacman-x (game-pac g)))
    (start-y (pacman-y (game-pac g))))
     (dolist (mv moves)

       (do-move! g nil start-x start-y mv)
       (toggle-turn! g)
       (let* ((child-val (compute-max g (1+ curr-depth) alpha beta cutoff-depth)))
       (toggle-turn! g)
   (undo-move! g)
   ;; Check for updating ALPHA value...
   (when (< child-val beta)
     (setf beta child-val)
     ;; Check for pruning ...
     (when (<= beta alpha)
       ;; Hey! PRUNE!  Forget about any remaining moves in
       ;;  this DOLIST... We're outta here!!
       (return-from compute-min beta)))))
       beta))
  (t
    ;;(ADD THING THAT ACTUALLY MOVES HERE)
   (let*
 ((list-of-move-lists (legal-moves g))
  (move-combos (gen-move-combos (rest list-of-move-lists)))
  (ghosts (rest (game-ghosts g))))
     (dolist (moves move-combos)
       (dotimes (n (length moves))

         (do-move! g nil (ghost-x (nth n ghosts)) (ghost-y (nth n ghosts)) (nth n moves)))
       (toggle-turn! g)
       (let* ((child-val (compute-max g (1+ curr-depth) alpha beta cutoff-depth)))
       (toggle-turn! g)
         (dotimes (z (length moves))
           (undo-move! g))
         (when (< child-val beta)
           (setf beta child-val)
     ;; Check for pruning ...
           (when (<= beta alpha)
       ;; Hey! PRUNE!  Forget about any remaining moves in
       ;;  this DOLIST... We're outta here!!
       (return-from compute-min beta)))))
       beta)))))

;;  COMPUTE-DO-AND-SHOW-N-MOVES
;; ------------------------------------------
;;  INPUTS:  G, a GAME struct
;;           N, a positive integer
;;           CUTOFF-DEPTH, the cutoff depth for minimax
;;  OUTPUT:  don't care
;;  SIDE EFFECT:  Computes, does, and shows the results of N
;;                moves generated using COMPUTE-MOVE.

(defun comp-do-n-show
    (g n cutoff-depth)
  (let ((mv nil)
        (whose-turn? (game-whose-turn? g)) ; integer for pac or ghosts
        (item nil)  ; character doing move, their x and y coordinates
        (x nil)
        (y nil))
    ;; WHOSE-TURN? is represented by integer constants for PAR and GHOSTS
    ;; we need to assign values to ITEM, X, and Y based on this value
    (if (equal whose-turn? *pac*)
    (progn (setf item (game-pac g))
           (setf x (pacman-x item))
           (setf y (pacman-y item)))
    (progn (setf item (first (game-ghosts g)))
           (setf x (ghost-x item))
           (setf y (ghost-y item))))
    (dotimes (i n)
      (format t "~%~A~%" g)
      (setf mv (compute-move g cutoff-depth))
      (do-move! g nil x y mv))
    (format t "~%~A~%" g)))
