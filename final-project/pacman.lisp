;; =======================================================================
;;   CMPU-365, Spring 2019
;;   Final Project
;;   Matthew Imiolek, Amy O'Connell
;;   pacman.lisp
;;   Set up for the actual game of Pacman
;; =======================================================================

;(load "search-ghost" :verbose nil)

;(setq compiler:tail-call-self-merge-switch t)
;(setq compiler:tail-call-non-self-merge-switch t)

;;  PACMAN struct
;; -----------------------------------------------------------------------
;;  represents Pacman (the person) in the game
;;
;;  x        - x coordinate of location
;;  y        - y coordinate of location
;;  alive?   - is pacman dead yet?
;;  eat-hist - a list containing integers of when pacman eats pellets
;; -----------------------------------------------------------------------

(defstruct pacman
  x
  y
  (alive? t)
  (eat-hist nil))

;;  GHOST struct
;; -----------------------------------------------------------------------
;;  represents a ghost in the game
;;
;;  x         - x coordinate of location
;;  y         - y coordinate of location
;;  type      - species of ghost, determines movement pattern
;;  on-pellet - tells if the ghost is on a pellet or not
;;  ghost-id  - a unique ID so we move the right ghost
;; -----------------------------------------------------------------------

(defstruct ghost
  x
  y
  (type 0)
  (on-pellet nil)
  ghost-id)

;; CONSTANTS
;; ------------------------------------------------------------------------

;; *BLANK* - integer to represent blank space in the board array
(defconstant *blank* 0)
;; *WALL* - integer to represent wall in board array
(defconstant *wall* 1)
;; *PELLET* - integer to represent pellet in board array
(defconstant *pellet* 2)
;; *PAC* - integer to represent pacman in board array
(defconstant *pac* 3)
;; *GHOST* - integer to represent ghost in board array
(defconstant *ghost* 4)
;; *PEL-SCORE* - integer to represent the score for getting a pellets
(defconstant *pel-score* 10)
;; *MOVE-SCORE* - integer to represent the negative score for taking a move
(defconstant *move-score* -1)
;; *DIST-SCORE* - integer to represent the negative score for the distance from
;;                a ghost
(defconstant *dist-score* -100)
;; DEF-PAC - a default pacman structs
(defconstant def-pac
    (make-pacman
     :x 7
     :y 9))
;; DEF-GHOST-ONE - the default ghost one
(defconstant def-ghost-one
    (make-ghost
     :x 7
     :y 7
     :ghost-id 101))
;; DEF-GHOST-TWO - the default ghost two
(defconstant def-ghost-two
    (make-ghost
     :x 8
     :y 7
     :ghost-id 102))
;; DEF-GHOSTS - the default list of ghosts
(defconstant def-ghosts
    (list def-ghost-one def-ghost-two))
;; DEF-BLANKS* - the default blank spaces in the board array
(defconstant *def-blanks*
    '((7 5)
      (7 6)
      (6 7)))
;; *DEF-PELLETS* - the default pellet locations in the board array
(defconstant *def-pellets*
    '((1 1) (2 1) (3 1) (4 1) (5 1) (6 1) (7 1) (8 1) (9 1) (10 1) (11 1) (12 1) (13 1)
      (1 2) (5 2) (9 2) (13 2)
      (1 3) (5 3) (9 3) (13 3)
      (1 4) (5 4) (6 4) (7 4) (8 4) (9 4) (13 4)
      (1 5) (2 5) (3 5) (4 5) (5 5) (9 5) (10 5) (11 5) (12 5) (13 5)
      (1 6) (4 6) (10 6) (13 6)
      (1 7) (4 7) (10 7) (13 7)
      (1 8) (4 8) (10 8) (13 8)
      (1 9) (4 9) (5 9) (6 9) (7 9) (8 9) (9 9) (10 9) (13 9)
      (1 10) (2 10) (3 10) (4 10) (7 10) (10 10) (11 10) (12 10) (13 10)
      (1 11) (7 11) (13 11)
      (1 12) (7 12) (13 12)
      (1 13) (2 13) (3 13) (4 13) (5 13) (6 13) (7 13) (8 13) (9 13) (10 13) (11 13) (12 13) (13 13)))

;;  GAME struct
;; -----------------------------------------------------------------------
;;  The representation of a game of Pacman.
;;
;;  Fields:
;;   BOARD         -- An 15-by-15 array containing an int representing a
;;                     PELLET, WALL, PACMAN, GHOST, or EMPTY
;;   PAC           -- a PACMAN struct
;;   WHOSE-TURN?   -- Either *pac* or *ghosts*
;;   GHOSTS        -- a list of GHOST structs
;;   PELLETS-EATEN -- how many pellets Pacman has eaten
;;   TOT-PELLETS   -- the total number of pellets at the start of the game
;;   SCORE         -- an integer representing the current score of the game
;;   GAME-HIST     -- a list of the moves that got us from the initial state to
;;                     the current state
;;   NUM-PAC-MOVES -- the number of moves pacman has done
;; -----------------------------------------------------------------------

(defstruct (game (:print-function print-game))
  (board (make-array '(15 15) :initial-element 1))
  pac
  (whose-turn? *pac*)
  ghosts
  (pellets-eaten 0)
  (tot-pellets 0)
  (score 0)
  (game-hist nil)
  (num-pac-moves 0))

;;  INIT-GAME
;; ------------------------------------------------------------------------
;;  Sets up an initial GAME struct
;;
;;  INPUTS: PELLET-LOCS, a list of pellet locations on the board (optional)
;;          BLANK-LOCS, a list of blank locations on the board (optional)
;;          PAC, a Pacman struct (optional)
;;          GHOSTS, a vector of ghost structs (optional)
;;  OUTPUT: A game of Pacman updated with the users preferences

(defun init-game (&key  (pellet-locs *def-pellets*)
                        (blank-locs *def-blanks*)
                        (pac (copy-structure def-pac))
                        (ghosts (copy-alist def-ghosts)))
  ;; Create the initial Pacman game
  (let* ((game (make-game))
	 (new-ghosts nil))
    (dolist (ghosty ghosts)
      (setf new-ghosts (cons (deep-copy-ghost ghosty) new-ghosts)))
    ;; Update the PACMAN struct in GAME
    (setf (game-pac game) pac)
    ;; Update the GHOSTS vector in the GAME
    (setf (game-ghosts game) new-ghosts)
    ;; Update the BOARD array in the GAME
    (setf (game-board game) (set-board pellet-locs blank-locs game))
    ;; Return the game
    game))

;;  SET-BOARD
;; ------------------------------------------------------------------------
;;  Sets up the initial https://sourceforge.net/p/sbcl/mailman/message/10369977/board for the Pacman game, and updates pellet count
;;
;;  INPUTS: PELLET-LOCS, a list of pellet locations on the board
;;          BLANK-LOCS, a list of blank locations on the board
;;          GAMEY, a GAME struct
;;  OUTPUT: The updated game board
;;  SIDE EFFECTS: Updates the number of pellets on the initial board

(defun set-board (pellet-locs blank-locs gamey)
  (let*
      ;; The wall-only array of the game
      ((bored (game-board gamey))
       ;; The x position of Pacman
       (pac-x (pacman-x (game-pac gamey)))
       ;; The y position of Pacman
       (pac-y (pacman-y (game-pac gamey)))
       ;; A vector of GHOST structs
       (ghosts (game-ghosts gamey)))
    ;; For each location in blank locations, set that location to *BLANK* in the
    ;; board array
    (dolist (n blank-locs)
      (setf (aref bored (first n) (second n)) *blank*))
    ;; For each location in pellet locations, set that location to *PELLET* in
    ;; the board array
    (dolist (n pellet-locs)
      (setf (aref bored (first n) (second n)) *pellet*))
    ;; For each ghost set the array to the value of being a ghost
    (dolist (n ghosts)
      (setf (aref bored (ghost-x n) (ghost-y n)) *ghost*))
    ;; Update the number of pellets on the initial board in the given GAME
    (setf (game-tot-pellets gamey) (length pellet-locs))
    ;; For the Pacman location, set that location to *PAC* in the board array
    (setf (aref bored pac-x pac-y) *pac*)
    bored))

;;  COPY-GHOST
;; -----------------------------------------------------------------------
;;  Creates a copy of a ghost that doesn't use refrences, but actual new values
;;
;;  INPUT: GHOSTY, a GHOST struct
;;  OUTPUT: a new ghost struct
;;  SIDE EFFECT: copies the values in the old ghost struct into the new one

(defun deep-copy-ghost (ghosty)
  (let*
      ;; Create a new ghost to return
      ((new-ghost (make-ghost :x 1 :y 1 :ghost-id 1)))
    ;; Set x
    (setf (ghost-x new-ghost) (ghost-x ghosty))
    ;; Set y
    (setf (ghost-y new-ghost) (ghost-y ghosty))
    ;; Set type
    (setf (ghost-type new-ghost) (ghost-type ghosty))
    ;; Set if it is on a pellet
    (setf (ghost-on-pellet new-ghost) (ghost-on-pellet ghosty))
    ;; Set ID
    (setf (ghost-ghost-id new-ghost) (ghost-ghost-id ghosty))
    ;; Return the copied ghost
    new-ghost))

;;  PRINT-GAME
;; -----------------------------------------------------------------------
;;  Print function for GAME struct
;;
;;  INPUTS: GAME, a Pacman game
;;          STR, an output stream (usually t)
;;          DEPTH, printing depth parameter (ignored)
;;  OUTPUT: nil
;;  SIDE EFFECT: Prints out the current Pacman Game

(defun print-game (game str depth)
  (declare (ignore depth))
  (let*
      ((board (game-board game))
       (tot-pells (game-tot-pellets game))
       (eaten-pells (game-pellets-eaten game))
       (score (game-score game)))
    (format str " ~%~%  Pacman Game:~%")
    (format str " ===============================~%~%")
    (format str "   0 1 2 3 4 5 6 7 8 9 A B C D E~%")
    (format str "   -----------------------------~%")
    (dotimes (row 15)
      (format str " ~x|" row)
      (dotimes (col 15)
	(let* ((element (aref board col row)))
	  (cond
	   ((= element 1)
	    (format str "W "))
	   ((= element 2)
	    (format str "* "))
	   ((= element 3)
	    (format str "P "))
	   ((> element 3)
	    (format str "G "))
	   (t
	    (format str "_ ")))))
      (format str "~%"))
    (format str "~%")
    (format str " Total Pellets: ~a~%" tot-pells)
    (format str " Pellets Eaten: ~a~%" eaten-pells)
    (cond
      ((game-over? game)
       (format str " Current Score: ~a~%" score))
      (t
        (format str " Current Score: ~a~%" (eval-func game))))))

;;  DESTN
;; ------------------------------------------------------------------------
;;  Calculates the end position of a move
;;
;;  INPUTS: X, the current X of the moving thing
;;          Y, the current Y of the moving thing
;;          MOVE, an integer representing the direction the player wants to move
;;  OUTPUT: A list with the new X and Y coordinates

(defun destn (x y move)
  (let*
      ;; The updated x value for a specific move
      ((up-x x)
       ;; The updated y value for a specific move
       (up-y y))
    (cond
     ;; Attempt to move up
     ((= move 0) (setf up-y (- y 1)))
     ;; Attempt to move down
     ((= move 1) (setf up-y (+ y 1)))
     ;; Attempt to move left
     ((= move 2) (setf up-x (- x 1)))
     ;; Attempt to move right
     ((= move 3) (setf up-x (+ x 1))))
    ;; Return updated location
    (list up-x up-y)))

;;  LEGAL-MOVE?
;; ------------------------------------------------------------------------
;;  Checks if a player input move is legal
;;
;;  INPUTS: GAME, a GAME struct
;;          X, the current X of the moving thing
;;          Y, the current Y of the moving thing
;;          MOVE, an integer representing the direction the player wants to move
;;  OUTPUT: T if the move would be legal, otherwise nil

(defun legal-move? (game x y move)
  (let*
      ;; The board for the Pacman game
      ((board (game-board game))
       ;; full updated move
       (up-move (destn x y move))
       ;; The updated x value for a specific move
       (up-x (first up-move))
       ;; The updated y value for a specific move
       (up-y (second up-move))
       ;; Get the value of the pieces
       (val (aref board x y))
       ;; Get whose turn it is
       (turn (game-whose-turn? game)))
;    (format t "~A, up-x: ~A~%" x up-x)
;    (format t "~A, up-y: ~A~%" y up-y)
    (cond
     ;; If attempt to move off of board, return nil
     ((or (> 0 up-y) (> 0 up-x) (< 14 up-y) (< 14 up-x))
      (return-from legal-move? nil))
     ;; If attempt to move into a wall, return nil
     ((= (aref board up-x up-y) *wall*)
      (return-from legal-move? nil))
     ;; Error if it is not pacman being moved on Pacman's turn
     ((and (not (= val 3)) (= turn 3))
      (return-from legal-move? nil))
     ;; Error if it is not a ghost being moved on Ghosts' turn
     ((and (not (> val 3)) (= turn 4))
      (return-from legal-move? nil))
    (t t))))
    ;; Otherwise return true
;    t))

;;  LEGAL-MOVES
;; ------------------------------------------------------------------------
;;  If it is pacman's turn, returns a list of moves that pac can make
;;  represented as integers
;;  If it is the ghosts' turn, returns a list of lists. Each inner list
;;  contains moves one of the ghosts can make represented as integers
;;
;;  INPUTS: GAMEY, a GAME struct
;;  OUTPUT: LIST or LIST of LISTS of legal moves

(defun legal-moves (gamey)
  (let ((whose-turn (game-whose-turn? gamey)) ;stores whose turn it is
        (legal-moves '()))		;container for output - initialize to empty
    (cond
     ;; Case 1: Pacman's turn: store pac's location
     ((eq whose-turn *pac*)
      (let ((pac-x (pacman-x (game-pac gamey)))
	    (pac-y (pacman-y (game-pac gamey))))
	;; try each move (up, down, right, left) and store the moves
	;; allowed by LEGAL-MOVE? function in the LEGAL-MOVES list

;  (format t "pac-x: ~A~%" pac-x)
;  (format t "pac-y: ~A~%" pac-y)

	(dotimes (move 4)
	  (when (legal-move? gamey pac-x pac-y move)
	    (setf legal-moves (cons move legal-moves))))))
     ;; Case 2: Ghosts' turn: iterate through GHOSTS in GAMEY
     (t
      (let ((ghost-list (game-ghosts gamey)))
	(dolist (ghosty ghost-list)
	  (let ((ghosty-x (ghost-x ghosty))
		(ghosty-y (ghost-y ghosty))
		(inner-list '()))
	    ;; try each move (up, down, right, left) and store the moves
	    ;; allowed by LEGAL-MOVE? function in the INNER-LIST list
	    (dotimes (move 4)
	      (when (legal-move? gamey ghosty-x ghosty-y move)
		(setf inner-list (cons move inner-list))))
	    ;; add INNER-LIST to the LEGAL-MOVES list
	    (setf legal-moves (cons inner-list legal-moves)))))))
    ;; return list of (lists of) legal moves for GAMEY
    (reverse legal-moves)))

;;  DO-MOVE!
;; -----------------------------------------------------------------------
;;  Does a move destructively (we do not change whose turn here due to having
;;  to do multiple ghosts moves bofore switching back to pacman's turn)
;;
;;  INPUTS: GAME, GAME struct
;;          CHECK-LEGAL?, a boolean flag
;;          X, the current X position of the object to move
;;          Y, the current Y position of the object to move
;;          MOVE, an integer representing the wanted move (0- up, 1- down, 2-
;;            left, 3- right)
;;  OUTPUT: Resulting move if legal and made, nil otherwise

(defun do-move! (game check-legal? x y move)
  (cond
    ;; Check if a player input move can be done
    ((and check-legal? (not (legal-move? game x y move)))
     (format t "Umm... Can't do illegal move.")
     nil)
    ;; Otherwise...
    (t
     (let*
         ;; the current game board
         ((bored (game-board game))
          ;; the full updated move
          (up-move (destn x y move))
          ;; the updated x position
          (up-x (first up-move))
          ;; the updated y position
          (up-y (second up-move))
          ;; if it is the ghosts or pacmans turn
          (turn (game-whose-turn? game)))
       (cond
         ;; In the case it is pacmans turn...
         ((= turn *pac*)
          (cond
            ;; If the next position has a pellet
            ((= (aref bored up-x up-y) *pellet*)
             ;; Add a 1 to the when pacman eats pellets
             (setf (pacman-eat-hist (game-pac game))
                   (cons 1 (pacman-eat-hist (game-pac game))))
             ;; Add 1 to the num,ber of pellets eaten
             (incf (game-pellets-eaten game))
             ;; Set that place to *pac*
             (setf (aref bored up-x up-y) *pac*))
            ;; If the next space is a ghost
            ((> (aref bored up-x up-y) *pac*)
             ;; Add a 0 to the when pacman eats pellets
             (setf (pacman-eat-hist (game-pac game))
                   (cons 0 (pacman-eat-hist (game-pac game))))
             ;; Set that place to *pac* plus the current val
             (setf (aref bored up-x up-y) (+ *pac* (aref bored up-x up-y))))
            ;; Otherwise is is a blank
            (t
             ;; Add a 0 to the when pacman eats pellets
             (setf (pacman-eat-hist (game-pac game))
                   (cons 0 (pacman-eat-hist (game-pac game))))
             ;; Set that place to *pac*
             (setf (aref bored up-x up-y) *pac*)))
          ;; change the place he came from to *BLANK*
          (setf (aref bored x y) *blank*)
          ;; update Pacman's X field
          (setf (pacman-x (game-pac game)) up-x)
          ;; update Pacman's Y field
          (setf (pacman-y (game-pac game)) up-y)
          ;; update the number of moves done by pacman
          (incf (game-num-pac-moves game)))
         ;; Otherwise move a ghost
         (t
          (let*
              ;; the list of ghosts
              ((ghosts (game-ghosts game))
               ;; the ghost that is actually being moved
               (right-ghost nil))
            ;; go through the list of ghosts checking their position to get
            ;; the right ghost so we can store all of its other data correctly
            (dolist (n ghosts)
                    (if (and (= x (ghost-x n)) (= y (ghost-y n)))
                        (setf right-ghost n)))
            (let*
                ;; get the old value of the location where the ghost was
                ((old-val (aref bored x y))
                 ;; get the current value of the location where the ghost is
                 ;; moving
                 (cur-val (aref bored up-x up-y)))
              ;; move the ghost to the new space
              (setf (aref bored up-x up-y) (+ cur-val *ghost*))
              ;; move the ghost from the old space
              (setf (aref bored x y) (- old-val *ghost*))
              ;; Update the x positon in the GHOST struct
              (setf (ghost-x right-ghost) up-x)
              ;; Update the y position in the GHOST struct
              (setf (ghost-y right-ghost) up-y)))))
       ;; Add the move that was done to the GAME-HIST for use in UNDO-MOVE!
       (push (list x y move) (game-game-hist game))
       ;; Return game
       game))))

;;  UNDO-MOVE!
;; -----------------------------------------------------------------------
;;
;;  Undoes the previous move
;;  INPUT: GAME, a GAME struct
;;  OUTPUT: the modified GAME struct
;;  SIDE EFFECT: Destructively undoes the most recent move on the move history

(defun undo-move! (game)
  (cond
    ((null (game-game-hist game))
     (format t "Umm... Can't undo move... empty history!~%")
     nil)
    (t
     (let*
         ;; Get the info for the last move done
         ((move-inf (first (game-game-hist game)))
          ;; Get the current game board
          (bored (game-board game))
          ;; Get the initial x position of the last move
          (x (first move-inf))
          ;; Get the initial y position of the last move
          (y (second move-inf))
          ;; Get the actual move done for the last move
          (move (third move-inf))
          ;; Get the full move for the last move done
          (up-move (destn x y move))
          ;; Get the current x of the piece last moved
          (up-x (first up-move))
          ;; Get the current y of the piece last moved
          (up-y (second up-move))
          ;; Get whose turn it is
          (turn (game-whose-turn? game))
          ;; Get the history of when pacman ate pellets
          (eat-hist (pacman-eat-hist (game-pac game))))
       (cond
         ((= turn *pac*)
          ;; In the case it is pacmans turn...
          (cond
            ;; If Pacman ate a pellet on its last move...
            ((= (first eat-hist) 1)
             ;; ... set the updated board back to being a pellet
             (setf (aref bored up-x up-y) *pellet*)
             ;; ... decrement the number of pellets eaten
             (decf (game-pellets-eaten game)))
            ;; Otherwise ...
            ((> (aref bored up-x up-y) *pac*)
             (setf (aref bored up-x up-y) (- (aref bored up-x up-y) *pac*)))
            (t
             ;; ... set the updated board back to being blank
             (setf (aref bored up-x up-y) *blank*)))
          ;; ... and set the eat history to be the REST of eat history
          (setf (pacman-eat-hist (game-pac game)) (rest (pacman-eat-hist (game-pac game))))
          ;; change the place Pacman moved to in array to *PAC*
          (setf (aref bored x y) *pac*)
          ;; change the place he came from to *BLANK*
          (setf (pacman-x (game-pac game)) x)
          ;; update Pacman's Y field
          (setf (pacman-y (game-pac game)) y)
          ;; update the number of moves done by pacman
          (decf (game-num-pac-moves game)))
         ;; Otherwise move a ghost
         (t
          (let*
              ;; the list of ghosts
              ((ghosts (game-ghosts game))
               ;; the ghost that is actually being moved
               (right-ghost nil))
            ;; go through the list of ghosts checking their position to get
            ;; the right ghost so we can store all of its other data correctly
            (dolist (n ghosts)
                    (if (and (= up-x (ghost-x n)) (= up-y (ghost-y n)))
                        (setf right-ghost n)))
            (let*
                ;; get the current value of the location where the ghost is
                ((old-val (aref bored x y))
                 ;; get the old value of the location where the ghost was
                 (cur-val (aref bored up-x up-y)))
              ;; move the ghost to the old place
              (setf (aref bored up-x up-y) (- cur-val *ghost*))
              ;; move the ghost from the new space
              (setf (aref bored x y) (+ old-val *ghost*))
              ;; Update the x positon in the GHOST struct
              (setf (ghost-x right-ghost) x)
              ;; Update the y position in the GHOST struct
              (setf (ghost-y right-ghost) y)))))
       ;; Add the move that was done to the GAME-HIST for use in UNDO-MOVE!
       (setf (game-game-hist game) (rest (game-game-hist game)))
       ;; Return game
       game))))

;;  TOGGLE-TURN!
;; -----------------------------------------------------------------------
;;  Changes whose turn it is (form ghost to pacman or vica-versa)
;;
;;  INPUT: GAME, a GAME struct
;;  OUTPUT: None
;;  SIDE EFFECT: Changes whose turn it is

(defun toggle-turn! (game)
  (let
      ;; The current turn in the game
      ((current-turn (game-whose-turn? game)))
    ;; Update the game turn
    (setf (game-whose-turn? game) (other-plr current-turn))))

;;  OTHER-PLR
;; -----------------------------------------------------------------------
;;
;;  INPUT: PLR, either *pac* or *ghost*- the current groups turn
;;  OUTPUT: The other player (either *pac* or *ghost*)
(defun other-plr (plr)
  (- 7 plr))

;;  GAME_OVER?
;; ------------------------------------------------------------------------
;;  Checks if the game is over by checking if a ghost has the same position
;;  as Pacman or all of the pellets have been eaten
;;
;;  INPUT: G, a GAME structs
;;  OUTPUT: T if the game is over

(defun game-over? (g)
  ;; The x position of pacman
  (let* ((pac-x (pacman-x (game-pac g)))
	 ;; the y positon of pacman
	 (pac-y (pacman-y (game-pac g)))
	 ;; the list of ghosts in the game
	 (ghosts (game-ghosts g))
	 ;; total number of pellets
	 (tot-pells (game-tot-pellets g))
	 ;; number of pellets eaten
	 (pells-eaten (game-pellets-eaten g)))
    ;; for each ghost...
    (dolist (n ghosts)
      ;; ... check if it is at the same position as pacman
      (if (and (= pac-x (ghost-x n)) (= pac-y (ghost-y n)))
	  ;; ... and if it is return t for game over
	  (return-from game-over? t)))
    ;; if the number of pellets eaten is the same as the number of pellets
    ;; in the original game, the game is over
    (if (= pells-eaten tot-pells)
	(return-from game-over? t))
    ;; otherwise return nil
    nil))

;;  GAME_OVER-CALC
;; ------------------------------------------------------------------------
;;  Calculates the score for a game over condition
;;
;;  INPUT: G, a GAME structs
;;         DEPTH, the current depth in the search
;;  OUTPUT: T if the game is over

(defun game-over-calc (g depth)
  ;; The x position of pacman
  (let* ((pac-x (pacman-x (game-pac g)))
    ;; the y positon of pacman
    (pac-y (pacman-y (game-pac g)))
    ;; the list of ghosts in the game
    (ghosts (game-ghosts g))
    ;; total number of pellets
    (tot-pells (game-tot-pellets g))
    ;; number of pellets eaten
    (pells-eaten (game-pellets-eaten g))
    ;; Score to return
    (score 0)
    ;; Whose turn it is
    (turn (game-whose-turn? g)))
    ;; for each ghost...
    (dolist (n ghosts)
      ;; ... check if it is at the same position as pacman
      (if (and (= pac-x (ghost-x n)) (= pac-y (ghost-y n)))
        ;; ... and if it is return a loss score
        (setf score (+ -100000 depth))))
    ;; checks if the number of pellets is the same as those eaten
    (if (= pells-eaten tot-pells)
      ;; sets score to a win value if it is
      (setf score (- 100000 depth)))
    ;; Inverts scores if it is a ghosts turn (opposite win/loss conditions to pacman)
    (if (= turn *ghost*)
      (* -1 score))
    ;; Set game score to score
    (setf (game-score g) score)
    ;; Return score
    score))

;;  EVAL-FUNC
;; -------------------------------------------------------------------------
;;  evaluates the current score of the game
;;
;;  INPUT: G, a GAME struct
;;  OUTPUT: The static evaluation of the current state of the game based on
;;          the number of moves made by pacman, number of pellets collected
;;          by Pacman, and how close the Ghosts are to Pacman

(defun eval-func (g)
  ;; Get the number of pellets eaten so far
  (let* ((eaten (game-pellets-eaten g))
	       ;; Get the number of moves Pacman has done so far
         (moves (game-num-pac-moves g))
	        ;; The distances of each ghost from Pacman
         (distances (ghost-race g))
	        ;; Score to returns
         (scorey 0)
         ;; Gets whose turn it is
         (turn (game-whose-turn? g)))
    (cond
      ;; Calculate score for pacman turn
      ((= turn *pac*)
        ;;Calculate the score for each ghosts distance
        (dolist (dist distances)
          (setf scorey (+ scorey (floor *dist-score* dist))))
        ;; Return the calculated score
        (setf scorey (+ scorey (* eaten *pel-score*) (* moves *move-score*))))
        ;; Calculate score for pacman turn
       (t
         ;;Calculate the score for each ghosts distance
         (dolist (dist distances)
           (setf scorey (- scorey (floor *dist-score* dist))))
       ;; Return the calculated score
       (setf scorey (- scorey (* eaten *pel-score*) (* moves *move-score*)))))
      scorey))

;;  GEN-MOVE-COMBOS
;; -------------------------------------------------------------------------
;;  evaluates the current score of the game
;;
;;  INPUT: GHOST-MOVE-LIST - a list of lists of legal move integers --
;;                           represents the legal moves for each alpha-beta
;;                           GHOST in a pacman game
;;  OUTPUT: a list of lists: each inner list is a sequence of move integers
;;          the move at each index corresponds to a legal move for the
;;          corresponding GHOST. Outer list represents all unique sequences
;;          of legal moves by a group of alpha-beta GHOSTs

(defun gen-move-combos (ghost-move-lists)
  (let ((acc-outer '(nil)) ; stores move sequences for processed GHOSTS
        (acc-inner nil))   ; temp storage for new sequences
    ;; while ghost-move-list is not empty
    ;; there are still ghosts left to move
    (while ghost-move-lists
           (setf acc-inner nil)
           (dolist (move-list acc-outer)
              ;; iterate through the next list of moves in GHOST-MOVE-LISTS
              (dolist (move (first ghost-move-lists))
                (setf acc-inner
                  ;; generate new sequence by adding MOVE to each existing
                  ;; sequence in ACC-OUTER -- collect using ACC-INNER
                  (cons (cons move move-list) acc-inner))))
           ;; replace ACC-OUTER with new sequences, reversed to preserve order
           (setf acc-outer (reverse acc-inner))
           ;; move on to next moves list in GHOST-MOVE-LIST
           (setf ghost-move-lists (rest ghost-move-lists)))
    ;; when there are no move lists left in GHOST-MOVE-LIST, reverse each
    ;; sequence in ACC-OUTER to match move indices to respective GHOSTS
    ;; return completed list of move sequences
    (mapcar #'reverse acc-outer)))
