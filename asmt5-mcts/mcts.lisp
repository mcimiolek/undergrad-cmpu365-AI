;; ========================================
;;  CMPU-365, Spring 2019
;;  Monte Carlo Tree Search -- Matthew Imiolek, Yina Wang
;; ========================================

;;  Contracts for the following functions used by MCTS algorithm
;; ----------------------------------------------------------
;;     GET-ROOT-NODE
;;     NEW-MC-TREE
;;     INSERT-NEW-NODE
;;     SIM-TREE
;;     SIM-DEFAULT (defined for you) 
;;     BACKUP
;;     UCT-SEARCH
;;     SELECT-MOVE

;;  In addition, for testing, the COMPETE function is defined for you.


;;  Your MCTS functions may call the following DOMAIN-DEPENDENT
;;  functions that are defined in "othello-starter.lisp":
;; ------------------------------------------------------------------
;;     COPY-GAME               -- creates a copy of the given othello game board
;;     MAKE-HASH-KEY-FROM-GAME -- returns list of the form (WHITE-PCS BLACK-PCS WHOSE-TURN)
;;     WHOSE-TURN              -- returns *BLACK* or *WHITE*

;;  Your MCTS functions may call the following DOMAIN-DEPENDENT
;;  functions that are defined in "othello-the-rest.lisp":
;; ------------------------------------------------------------------ 
;;     DO-MOVE!        --  does a move (destructively modifies game struct)
;;     LEGAL-MOVES     --  returns VECTOR of legal moves
;;     GAME-OVER?      --  returns T or NIL
;;     DEFAULT-POLICY  --  returns random legal move

;;  Your MCTS functions should not need to call any of the MACROs defined
;;  in "othello-macros.lisp".


;;  Note:  If a player has no legal moves, but the game isn't over, then that
;;         player *must* pass...


;;  MC-NODE struct -- a node in the MCTS tree
;; ----------------------------------------------------------------------------
;;  KEY:          a hash-table key (compact rep'n of current state of game)
;;  WHOSE-TURN:   *BLACK* or *WHITE*
;;  NUM-VISITS:   the number of times this state has been visited
;;  VECK-MOVES:   a VECTOR of the legal moves from this state
;;  VECK-VISITS:  a VECTOR recording the number of times each legal move
;;                   has been visited during MCTS
;;  VECK-SCORES:  a VECTOR recording the average scores for the legal
;;                   moves visited during MCTS

(defstruct mc-node
  key             
  whose-turn      
  (num-visits 0)  
  veck-moves      
  veck-visits     
  veck-scores    
  )



;;  MC-TREE struct -- the MCTS tree
;; -------------------------------------------------------------
;;  HASHY:     a hash-table whose entries are (key,value), where
;;               key = compact repn of state, value = mc-node
;;  ROOT-KEY:  the hash-table key for the root node of the mcts tree

(defstruct mc-tree
  (hashy (make-hash-table :test #'equal))      
  root-key)



;;  GET-ROOT-NODE
;; ------------------------------------------------------
;;  INPUT:   TREE, a MCTS struct
;;  OUTPUT:  The MC-NODE corresponding to the root of the TREE

(defun get-root-node
    (tree)
  ;; Search hash-table of the tree for the node corresponding
  ;; with the key for the root node
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))



;; -------------------------------------------------
;;  Easiest to define the following functions
;;  in the following order (to facilitate testing)
;; -------------------------------------------------

;;  NEW-MC-TREE
;; ---------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived
;;           from GAME.

(defun new-mc-tree
    (game)
  (let
      ;; Create a key for the root node of a new tree
      ;; from a given game
      ((root-key (make-hash-key-from-game game)))
    ;; Set the new trees ROOT-KEY to the created key
    (make-mc-tree :root-key root-key)))



;;  INSERT-NEW-NODE
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.

(defun insert-new-node
    (game tree key)
  (let
      ;; Get the hash table for the tree being updated
      ((hashy (mc-tree-hashy tree))
       ;; Set the values for the node to be added to the tree
       (nodey (make-mc-node
	       :key key
	       :whose-turn (othello-whose-turn game)
	       :veck-moves (legal-moves game)
	       :veck-visits (make-array (length (legal-moves game)) :initial-element 0)
	       :veck-scores (make-array (length (legal-moves game)) :initial-element 0))))
    ;; Update the hash-table of the tree for the given key with the new node
    (setf (gethash key hashy) nodey)
    ;; Return the new node
    nodey))
       
	       

;;  SELECT-MOVE
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector

(defun select-move
    (nodey c)
  (let*
      ;; Create a vector of legal moves
      ((legal (mc-node-veck-moves nodey))
       ;; create a vector of scores for moves
       (scores (mc-node-veck-scores nodey))
       ;; create a vector of number of visits for moves
       (visits (mc-node-veck-visits nodey))
       ;; create an integer for the number of total visits
       (num-visits (mc-node-num-visits nodey))
       ;; track whose turn it is
       (turn (mc-node-whose-turn nodey))
       ;; initialize the index of the selected move to 0
       (index 0)
       ;; initialze the best value so far to 0
       (best-val 0))
    (cond 
     ;; When it is black's turn...
     ((= turn *BLACK*)
      ;; ... update BEST-VAL to negative infinity
      (setf best-val *neg-inf*)
      ;; ... for each legal move ...
      (dotimes (i (length legal))
	(let* 
	    ;; get the average score of the given move
	    ((score (svref scores i))
	     ;; get the numebr of moves for the given move
	     (visit (svref visits i))
	     ;; initialize the current score for the given move to 0
	     (cur-score 0))
	  ;; if the move has never been done before ...
	  (if (= visit 0)
	      ;; ... set CUR-SCORE to positive infinity to make sure it goes
	      (setf cur-score *pos-inf*)
	  ;; ... otherwise update CUR-SCORE using the VISITS and SCORE
	  (setf cur-score (+ score
			     (* c 
				(sqrt 
				 (/ (log num-visits) visit))))))
	  (cond
	   ;; If CUR-SCORE is greater than BEST-VAL ...
	   ((> cur-score best-val)
	    ;; update BEST-VAL to be CUR-SCORE
	    (setf best-val cur-score)
	    ;; update INDEX to be the index of the current move
	    (setf index i)))))) 
     (t
       ;; When it is whites's turn...
      (setf best-val *pos-inf*)
      ;; ... update BEST-VAL to infinity
      (dotimes (i (length legal))
	;; ... for each legal move ...
	(let* 
	    ;; get the average score of the given move
	    ((score (svref scores i))
	     ;; get the average score of the given move
	     (visit (svref visits i))
	     ;; initialize the current score for the given move to 0
	     (cur-score 0))
	  ;; if the move has never been done before ...
	  (if (= visit 0)
	      ;; ... set CUR-SCORE to negative infinity to make sure it goes
	      (setf cur-score *neg-inf*)
	    ;; ... otherwise update CUR-SCORE using the VISITS and SCORE
	    (setf cur-score (- score
			     (* c
				(sqrt
				 (/ (log num-visits) visit))))))
	  (cond 
	   ;; If CUR-SCORE is less than BEST-VAL ...
	   ((< cur-score best-val)
	    ;; update BEST-VAL to be CUR-SCORE
	    (setf best-val cur-score)
	    ;; update INDEX to be the index of the current move
	    (setf index i)))))))
    ;; Return INDEX
    index))
	      
	
	
;;  SIM-TREE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is a key into the hashtable, and each move_i
;;    is an index into the MOVES vector of the node assoc with state_i.

(defun sim-tree
    (game tree c)
  (let*
      ;; Initialize state-move list to NIL
      ((listy nil))
    ;; While the game is not over...
    (while (not (game-over? game))
      (let*
	  ;; ... create a KEY from GAME
	  ((key (make-hash-key-from-game game))
	   ;; ... get the node asociated with that KEY
	   (node (gethash key (mc-tree-hashy tree)))
	   ;; ... initialize for the case of a new node
	   (new-node nil)
	   ;; ... initialize for the index of node in the MOVES vector
	   (index nil))
	(cond
	 ;; ... if NODE is not in TREE yet ...
	 ((null node)
	  ;; update NEW-NODE to the value of a new node created using GAME and inserted in TREE
	  (setf new-node (insert-new-node game tree key))
	  ;; Update INDEX to the correct value using SELECT-MOVE
	  (setf index (select-move new-node c))
	  ;; Do the move on the game
	  (apply #'do-move! game nil (svref (mc-node-veck-moves new-node) index))
	  ;; Update LISTY by adding INDEX and KEY to it
	  (setf listy (cons index (cons key listy)))
	  ;; RETURN-FROM SIM-TREE the reverse of LISTY, which is updated backwards
	  (return-from sim-tree (reverse listy))))
	;; ... otherwise ...
	;; ... Update INDEX to the correct value using SELECT-MOVE
	(setf index (select-move node c))
	;; ... do the move on the game
	(apply #'do-move! game nil (svref (mc-node-veck-moves node) index))
	;; Update LISTY by adding INDEX and KEY to it
	(setf listy (cons index (cons key listy)))))
    ;; return the correct ordered LISTY
    (reverse listy)))

  

;;  SIM-DEFAULT -- defined for you!
;; ----------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default
    (game)
  (default-policy game))



;;  BACKUP
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the 
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY

(defun backup
    (hashy key-move-acc result)
  (cond
   ;; If the list of keys and moves is empty, return nil
   ((null key-move-acc) nil)
   ;; Otherwise...
   (t
    ;; Update the totla number of NUM-VISITS done
    (setf (mc-node-num-visits (gethash (first key-move-acc) hashy)) 
      (+  (mc-node-num-visits (gethash (first key-move-acc) hashy)) 1))
    ;; Update the visits in VECK-VISITS for the move
    (setf (svref (mc-node-veck-visits (gethash (first key-move-acc) hashy)) 
		 (second key-move-acc)) 
      (+ 1 (svref (mc-node-veck-visits (gethash (first key-move-acc) hashy)) 
		  (second key-move-acc))))
    ;; Update the score in VECK-SCORES for the move
    (setf (svref (mc-node-veck-scores (gethash (first key-move-acc) hashy))
		 (second key-move-acc)) 
      (+ (/ (- result 
	       (svref (mc-node-veck-scores (gethash (first key-move-acc) hashy))
		      (second key-move-acc)))
	    (svref (mc-node-veck-visits (gethash (first key-move-acc) hashy)) 
		   (second key-move-acc)))
	 (svref (mc-node-veck-scores (gethash (first key-move-acc) hashy))
		(second key-move-acc))))
    ;; and recursively do BACKUP again
    (backup hashy (rest (rest key-move-acc)) result))))    
      
		    

;;  UCT-SEARCH
;; ---------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of MCTS.

;;  The following global parameter can be used to decide whether
;;  UCT-SEARCH should print out stats about the current round
;;  of MCTS.  The COMPETE function sets *verbose* to T; the
;;  COMPETE-NO-PRINTING function sets it to NIL.  

(defparameter *verbose* t) 

(defun uct-search
    (orig-game num-sims c)
  (let* 
      ;; Create TREE, a MC-TREE using ORIG-GAME
      ((tree (new-mc-tree orig-game))
       ;; Set the initial index to nil
       (index nil))
    ;; For the number of requested simulations...
    (dotimes (i num-sims)
      (let*
	  ;; Create COPY, a copy of ORIG-GAME
	  ((copy (copy-game orig-game))
	   ;; Use COPY to run a simulation of TREE from the ORIG-GAME, and save in STATES
	   (states (sim-tree copy tree c))
	   ;; Use COPY to finish up any undone moves using the default polict, and save in RESULT
	   (result (sim-default copy)))
	;; Update TREE using the found STATES and RESULT
	(backup (mc-tree-hashy tree) states result)))
    ;; Update INDEX to the best move found
    (setf index (select-move (get-root-node tree) 0))
    (cond
     ;; In the case detailed stats are wanted, print out detailed stats
     (*verbose*
      (format t "Best Score: ~a~%" (svref (mc-node-veck-scores (get-root-node tree)) index))
      (format t "Score Vector: ~a~%" (mc-node-veck-scores (get-root-node tree)))
      (format t "Visits Vector: ~a~%" (mc-node-veck-visits (get-root-node tree)))))
    ;; Return the best move found
    (svref (mc-node-veck-moves (get-root-node tree)) index)))

;;  COMPETE -- defined for you!
;; ------------------------------------------------------------------------------
;;  INPUTS:  BLACK-NUM-SIMS, the number of simulations for each of black's moves
;;           BLACK-C, the exploration/exploitation constant used by black
;;           WHITE-NUM-SIMS, the number of simulations for each of white's moves
;;           WHITE-C, the exploration/exploitation constant used by white
;;  OUTPUT:  Don't care
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute best moves
;;    for both players according to the specified parameters.

(defun compete
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-othello)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%" 
		(apply #'do-move! g nil (uct-search g black-num-sims black-c))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g nil (uct-search g white-num-sims white-c))))))))


;;  COMPETE-NO-PRINTING
;; --------------------------------------------------
;;  Same as COMPETE, but only shows the end result

(defun compete-no-printing
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-othello)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "B ")
	(apply #'do-move! g nil (uct-search g black-num-sims black-c)))
       (t
	(format t "W ")
	(apply #'do-move! g nil (uct-search g white-num-sims white-c)))))
    (format t "~%~A~%" g)))

