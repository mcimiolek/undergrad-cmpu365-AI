;;; ========================================
;;;   CMPU-365, Spring 2019
;;;   FILE: MatthewImiolek-asmt1-turing.lisp
;;;   February 2019
;;; ========================================
;;;   Turing Machine simulator for Asmt 1 of CS365

(load "2019-asmt-helper")

(header "Matthew Imiolek" "Asmt. 1 (Turing Machine)")

;; --------------------------------------------------
(problem "1: Create an instance of a turing machine")
;; --------------------------------------------------

;; Define the constant for the symbol indicating the 
;; left side of the tape as $

(defconstant LEFT_END_MARKER '$)



;; Define the constant for the symbol indicating a
;; blank tape space as _

(defconstant BLANK '_)



;; Define the constant indicating the Turing Machine
;; should move left on the tape as <

(defconstant MOVE_LEFT '<)



;; Define the constant indicating the Turing Machine
;; should move right on the tape as >

(defconstant MOVE_RIGHT '>)



;; Define a data structure TM representing a Turing
;; Machine which has an initial state INIT-STATE, a 
;; transition table TRANS-TABLE, and a set a halting
;; states HALT-STATES

(defstruct TM
  init-state
  trans-table
  halt-states)



;; Define a data structure CONFIG representing the 
;; current configuration of the Turing Maching
;; which has the current position on the tape
;; CUR-POS, the current state of the Turing Machine,
;; CUR-STATE, the character at the current position,
;; CUR-CHAR, and the current tape, CUR-TAPE

(defstruct config
  
  cur-state
  cur-pos
  cur-char
  cur-tape)



;; INIT-HASH -- used to initialize a Turing Machine's
;;              transition table
;; ----------------------------------------------------------
;; INPUT:  LIST-OF-LISTS, each element is a 4-element list
;; OUTPUT: A hash table whose key/value pairs are determined
;;         by the LIST-Of-LISTS. In particular, for each
;;         (A B C D) in LIST-OF-LISTS, there is an entry in
;;         the hash table with key = (A B) and value = (C D).

(defun init-hash 
    (list-of-lists)
  (let* 
      ((transit-table (make-hash-table :test 'equal))
       (helper
      (mapcar 
       #'(lambda (x) 
	   (setf (gethash (cons (first x) (cons (second x) nil)) transit-table)
	     (cons (third x) (cons (fourth x) nil))))
      list-of-lists)))
  transit-table))



;; MAKE-MY-TM
;; ---------------------------------------------------------
;; INPUT:  INIT-ST, initial state for a turing machine
;;         LIST-OF-LISTS, a list-of-lists representation of
;;              the transition table for a Turing Maching
;;         HALTING-STATES, a set of halting states
;; OUTPUT: A Turing Machine struct (TM) based on the above
;;         inputs where the tranition table is represented
;;         by a hash table based on LIST-OF-LISTS.

(defun make-my-tm
    (init-st list-of-lists halting-states)
  (setq TM (make-TM
	    :init-state init-st
	    :trans-table (init-hash list-of-lists)
	    :halt-states halting-states)))




;; ----------------------------------------------------------
(problem "2: Display the current configuration nicely")
;; ----------------------------------------------------------

;; Define a function for nicely displaying the current config

;; SHOW-CURR-CONFIG
;; ----------------------------------------------------------
;; INPUT:  KONFIG, a CONFIG struct
;; OUTPUT: None
;; SIDE EFFECT: Displays info about the current status of the
;;   given CONFIG struct.

(defun show-curr-config
    (konfig)
  (format nil "State = ~D, Posn = ~D, Char = ~D, Tape = ~D" 
	  (config-cur-state konfig) 
	  (- (config-cur-pos konfig) 1)
	  (config-cur-char konfig) 
	  (config-cur-tape konfig)))





;; ---------------------------------------------------------
(problem "3: Run the Turing Machine")
;; ---------------------------------------------------------

;; GET-HASH
;; --------------------------------------------------------
;; INPUTS: M, a Turing Machine struct
;;         KONFIG, a CONFIG struct
;; OUTPUT: the value associated with a given hashcode, made
;;   specifically to work well a Turing Macine struct and
;;   CONFIG struct

(defun get-hash
    (m konfig)
  (gethash
   (cons (config-cur-state konfig) (cons (config-cur-char konfig) nil))
   (tm-trans-table m)))



;; CORRECT-STRING
;; ---------------------------------------------------------
;; INPUTS: KONFIG, a CONFIG struct
;;         TRANS-TABLE, the transition table of a Turing
;;           Machine
;; OUTPUT: A string describing the action the Turing Machine
;;         is currently doing.

(defun correct-string
    (konfig m)
  (cond
   
   ;; Describes moving left
   ((equal (second (get-hash m konfig)) MOVE_LEFT)
    (format nil "Moving Left."))
   
   ;; Describes moving right
   ((equal (second (get-hash m konfig)) MOVE_RIGHT)
    (format nil "Moving Right."))
   
   ;; In all other cases says what character is being written
   ;; at what position
   (t (format nil "Writing character ~D at position ~D." 
	      (config-cur-char konfig) 
	      (config-cur-pos konfig)))))



;; UP-CONFIG
;; --------------------------------------------------------
;; INPUTS: M, a Turing Machine struct
;;         KONFIG, a CONFIG struct
;; OUTPUT: None
;; SIDE EFFECT: Modifies the contents of KONFIG according
;;   to the current settings in KONFIG and the transition
;;   rules for the transition table in M

(defun up-config
    (m konfig)
  (cond
   
   ;; Updates the config if the TM should be moving left on
   ;; the tape
   ((equal (second (get-hash m konfig)) MOVE_LEFT)
    (let ((tempc (nth (- (config-cur-pos konfig) 2) (config-cur-tape konfig)))
	  (temps (first (get-hash m konfig))))
      (setf (config-cur-char konfig) tempc
	    (config-cur-state konfig) temps
	    (config-cur-pos konfig) (- (config-cur-pos konfig) 1))))
   
   ;; Updates the config if the TM should be moving right on
   ;; the tape
   ((equal (second (get-hash m konfig)) MOVE_RIGHT)
    (let ((tempc (nth (config-cur-pos konfig) (config-cur-tape konfig)))
	  (temps (first (get-hash m konfig))))
      (setf (config-cur-char konfig) tempc
	    (config-cur-state konfig) temps
	    (config-cur-pos konfig) (+ (config-cur-pos konfig) 1))))
   
   ;; Otherwise updates the config assuming it is staying in
   ;; the same position and updating the tape
   (t
    (let ((tempc (second (get-hash m konfig)))
	  (temps (first (get-hash m konfig))))
      (setf (config-cur-char konfig) tempc
	    (config-cur-state konfig) temps
	    (nth (- (config-cur-pos konfig) 1) (config-cur-tape konfig))
	    tempc)))))



;; TM-RUN-HELPER
;; ---------------------------------------------------------
;; INPUTS: M, a Turing Machine struct
;;         KONFIG, a CONFIG struct
;; OUTPUT: None
;; SIDE EFFECT: May destructively modify the contents of
;;   KONFIG. Should display the current configuration each
;;   step of the way as the Turing Machine processes the
;;   tape.

(defun tm-run-helper
    (m konfig)
  (cond 
   
   ;; Checks if a halt state has been reached
   ((member (config-cur-state konfig) (tm-halt-states m))
    (format nil "~D~% TM has HALTED!" (show-curr-config konfig)))
   
   ;; Checks if the the user has moved past the left endmarker
   ((and 
     (equal (config-cur-char konfig) LEFT_END_MARKER)
     (equal (second (get-hash m konfig)) MOVE_LEFT))
    (format nil "~D~% ERROR: attempt to move left at left end of tape." 
	    (show-curr-config konfig)))
   
   ;; Checks that there is a valid transition table rule before
   ;; attempting to apply that rule; if not present makes an error
   ((not (get-hash m konfig))
    (format nil "~D~% ERROR: no known rule for given state and character." 
	    (show-curr-config konfig)))
   ((get-hash m konfig)
    (let*
	()
      (write-line (format nil "~D~%~D" (show-curr-config konfig) (correct-string konfig m)))
      (up-config m konfig)
      (tm-run-helper m konfig)))))



;; TM-RUN
;; ------------------------------------------------------------
;; INPUTS: M, a Turing Machine struct
;;         INIT-TAPE, a tape for the turing machine to process
;;           (for safety, uses a copy of the tape)
;; OUTPUT: None
;; SIDE EFFECT: Runs the given turing machine on the given tape
;;   starting from the turing machine's initial state.

(defun tm-run
    (m init-tape)
  (tm-run-helper m
		 (setq konfig
		   (make-config
		    :cur-state (tm-init-state m)
		    :cur-pos 1
		    :cur-char (first init-tape)
		    :cur-tape init-tape))))





;; ---------------------------------------------------------
(problem "Tests")
;; ---------------------------------------------------------

;; Turing Machine from class

(setq 4-1-1 (make-my-tm '0 
		    '((0 a 1 _) (0 _ -1 _) (0 $ 0 >) (1 a 0 a) (1 _ 0 >) (1 $ 1 >)) 
		    '(-1)))


;; My Turing Machine
;; ---------------------------------------------------------
;; My turing machine will turn a leading string of a's into
;; b's, however once it reaches either _ or b it will convert
;; all remaining a's and b's into _'s. Once it reaches a c it
;; will go left till it reaches the first b, or the left end
;; marker.
(setq mine (make-my-tm '0
		       '((0 $ 0 >) (0 a 0 b) (0 b 0 >) (0 _ 1 >) (0 c 2 <) (1 _ 1 >) (1 a 1 _)
			 (1 c 2 <) (1 a 1 _) (2 b -1 b) (2 $ -1 $) (2 _ 2 <) (1 b 1 _))
		       '(-1)))

;; Left error Turing Machine
(setq left-error (make-my-tm '0
			'((0 _ 0 <) (0 $ 0 <))
			'(-1)))

;; Tapes
(setq a '($ a a a _ _ _))
(setq b '($ _ _ _ _ _ _))
(setq c '($ _ a a a a a))
(setq d '($ a a _ _ a a))
(setq e '($ d _ _ _ _ _))		; Used to test for no valid transition rule
(setq f '($ a a b b c b))
(setq g '($ b b _ _ _ c))
(setq h '($ a a _ a a c))
(setq i '($ a a a a a c))

;; Run Tests for 4-1-1
(fancy-tester '(tm-run 4-1-1 a))
(fancy-tester '(tm-run 4-1-1 b))
(fancy-tester '(tm-run 4-1-1 c))
(fancy-tester '(tm-run 4-1-1 d))
(fancy-tester '(tm-run 4-1-1 e))

;; Run tests for mine
(fancy-tester '(tm-run mine e))
(fancy-tester '(tm-run mine f))
(fancy-tester '(tm-run mine g))
(fancy-tester '(tm-run mine h))
(fancy-tester '(tm-run mine i))

;; Run tests for left-error
(fancy-tester '(tm-run left-error b))