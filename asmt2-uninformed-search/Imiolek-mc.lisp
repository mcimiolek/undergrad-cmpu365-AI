;;; ===================================================================
;;; CMPU-365, Spring 2019
;;; Asmt. 2
;;; FILE: mc.lisp
;;; ===================================================================
;;; Implementation of MISSIONARIES AND CANNIBALS
;;; ===================================================================


;;; Specify the size of missionaries and cannibals in a global variable

(defconstant +mc-size+ 4)



;;; MC-STATE (data structure)
;;; --------------------------------------------------
;;; CAN-NEAR, number of cannibals on the near bank
;;; CAN-FAR, number of cannibals on the far bank
;;; MISS-NEAR, number of missionaries on the near bank
;;; MISS-FAR, number of missionaries on the far bank
;;; BOAT-LOC, number describing location of the boat,
;;;           0 for at near bank, 1 for at far bank)

(defstruct (mc-state (:print-function print-mc-state))
  can-near
  can-far
  miss-near
  miss-far
  boat-loc)



;;; PRINT-MC-STATE
;;; --------------------------------------------------
;;; INPUTS: STATE, a MC-STATE struct
;;;         STR, an output stream to print to
;;;         DEPTH, printing depth
;;; OUTPUT: None
;;; SIDE-EFFECT: Displays the given MC-STATE struct.

(defun print-mc-state
    (state str depth)
  ;; Tell the compiler not to warn us about the fact that we
  ;; don't make use of depth
  (declare (ignore depth))
  (let ((can-near (mc-state-can-near state))
	(can-far (mc-state-can-far state))
	(miss-near (mc-state-miss-near state))
	(miss-far (mc-state-miss-far state))
	(boat-loc (mc-state-boat-loc state)))
    (cond
     ;; Check if the boat is at the near bank to give simpler
     ;; output
     ((equal boat-loc 0)
      (format str "Cannibals Near: ~A, Cannibals Far: ~A, Missionaries Near: ~A, Missionaries Far: ~A, Boat on near bank." can-near can-far miss-near miss-far))
     ;; Otherwise assume the boat is at the far bank and
     ;; change output
     (t
      (format str "Cannibals Near: ~A, Cannibals Far: ~A, Missionaries Near: ~A, Missionaries Far: ~A, Boat on far bank." can-near can-far miss-near miss-far)))))



;;; MC-STATE-EQUAL?
;;; --------------------------------------------------
;;; INPUTS: STATE1, a MC-STATE struct
;;;         STATE2, a MC-STATE struct
;;; OUTPUT: T if the given MC-STATES have the same contents, NIL otherwise

(defun mc-state-equal?
    (state1 state2)
  (and
   (equal (mc-state-can-near state1) (mc-state-can-near state2))
   (equal (mc-state-can-far state1) (mc-state-can-far state2))
   (equal (mc-state-miss-near state1) (mc-state-miss-near state2))
   (equal (mc-state-miss-far state1) (mc-state-miss-far state2))
   (equal (mc-state-boat-loc state1) (mc-state-boat-loc state2))))



;;; MC-GOAL?
;;; --------------------------------------------------
;;; INPUTS: STATE, a MC-STATE struct
;;; OUTPUT: T if STATE is a goal-state.

(defun mc-goal?
    (state)
  (and
   (equal (mc-state-can-near state) 0)
   (equal (mc-state-can-far state) 3)
   (equal (mc-state-miss-near state) 0)
   (equal (mc-state-miss-far state) 3)))




;;; MAKE-MC-PROBLEM
;;; --------------------------------------------------
;;; Create an instance of a search problem for missionaries
;;; and cannibals

(defun make-mc-problem
    ()
  (make-search-problem
   :init-state (make-mc-state
		:can-near 3
		:can-far 0
		:miss-near 3
		:miss-far 0
		:boat-loc 0)
   :actions (list #'move-far-2-0 #'move-far-1-1 #'move-far-0-2 #'move-far-1-0 #'move-far-0-1
		  #'move-near-2-0 #'move-near-1-1 #'move-near-0-2 #'move-near-1-0 #'move-near-0-1)
   :goal-test-func #'mc-goal?
   :state-eq-func #'mc-state-equal?))





;;; DO-MC-DEPTH
;;; --------------------------------------------------
;;; INPUTS: None
;;; OUTPUT: A RESULTS struct that contains info about
;;;         what happened from running Depth-First Search
;;;         on the Missionaries and Cannibals problem.

(defun do-mc-depth
    ()
  (depth-first-search (make-mc-problem)))





;;; DO-MC-BREADTH
;;; --------------------------------------------------
;;; INPUTS: None
;;; OUTPUT: A RESULTS struct that contains info about
;;;         what happened from running Breadth-First
;;;         search on the Missionaries and Cannibals
;;;         problem.

(defun do-mc-breadth
    ()
  (breadth-first-search (make-mc-problem)))





;;; CAN-EAT?
;;; --------------------------------------------------
;;; INPUTS: CAN-NEAR, number of cannibals on the near bank
;;;         CAN-FAR, number of cannibals on the far bank
;;;         MISS-NEAR, number of missionaries on the near bank
;;;         MISS-FAR, number of missiobaries on the far bank
;;; OUTPUT: Returns T if the cannibals would eat the
;;;         missionaries

(defun can-eat?
    (can-near can-far miss-near miss-far)
   ;; Return T if there are more cannibals than
   ;; missionaries on either bank, assuming there is at
   ;; least one missionary on that bank
   (or
     (> can-near miss-near 0)
     (> can-far miss-far 0)))





;;; INVALID-NUMS?
;;; --------------------------------------------------
;;; INPUTS: CAN-NEAR, number of cannibals on the near bank
;;;         CAN-FAR, number of cannibals on the far bank
;;;         MISS-NEAR, number of missionaries on the near bank
;;;         MISS-FAR, number of missiobaries on the far bank
;;; OUTPUT: Returns T if there is a case of negative
;;;         amounts of a group on either bank

(defun invalid-nums?
    (can-near can-far miss-near miss-far)
  (or
   (> 0 can-near)
   (> 0 can-far)
   (> 0 miss-near)
   (> 0 miss-far)))




;;; MC-MOVE-NEAR
;;; --------------------------------------------------
;;; Attempts a move across the river, with various amounts'
;;; of cannibals and missionaries. Returns the new state
;;; or nil. In this case is moving to the near bank
;;; INPUTS: ORIG-STATE, the original MC-STATE struct
;;;         NUM-CAN, the amount of cannibals to move
;;;         NUM-MISS, the amount of missionaries to move
;;; OUTPUT: The resulting state, if all aspects are valid,
;;;         otherwise nil

(defun mc-move-near
    (orig-state num-can num-miss)
  ;; Create new values
  (let* ((n-can-near (+ (mc-state-can-near orig-state) num-can))
	 (n-can-far (- (mc-state-can-far orig-state) num-can))
	 (n-miss-near (+ (mc-state-miss-near orig-state) num-miss))
	 (n-miss-far (- (mc-state-miss-far orig-state) num-miss)))
    ;; Checks validity of output values, if a value would be invalid
    ;; returns nil
    (if
	(or
	 (can-eat? n-can-near n-can-far n-miss-near n-miss-far)
	 (invalid-nums? n-can-near n-can-far n-miss-near n-miss-far)
	 (equal (mc-state-boat-loc orig-state) 0))
	nil
      ;; Otherwise returns a struct with the new values
      (make-mc-state
       :can-near n-can-near
       :can-far n-can-far
       :miss-near n-miss-near
       :miss-far n-miss-far
       :boat-loc 0))))




;;; MC-MOVE-FAR
;;; --------------------------------------------------
;;; Attempts a move across the river, with various amounts'
;;; of cannibals and missionaries. Returns the new state
;;; or nil. In this case is moving to the far bank
;;; INPUTS: ORIG-STATE, the original MC-STATE struct
;;;         NUM-CAN, the amount of cannibals to move
;;;         NUM-MISS, the amount of missionaries to move
;;; OUTPUT: The resulting state, if all aspects are valid,
;;;         otherwise nil

(defun mc-move-far
    (orig-state num-can num-miss)
  ;; Creates new values
  (let* ((n-can-near (- (mc-state-can-near orig-state) num-can))
	 (n-can-far (+ (mc-state-can-far orig-state) num-can))
	 (n-miss-near (- (mc-state-miss-near orig-state) num-miss))
	 (n-miss-far (+ (mc-state-miss-far orig-state) num-miss)))
    ;; Checks validity of output values, if any are not valid returns
    ;; nil
    (if
	(or
	 (can-eat? n-can-near n-can-far n-miss-near n-miss-far)
	 (invalid-nums? n-can-near n-can-far n-miss-near n-miss-far)
	 (equal (mc-state-boat-loc orig-state) 1))
	nil
      ;; Otherwise returns a struct with the new values
      (make-mc-state
       :can-near n-can-near
       :can-far n-can-far
       :miss-near n-miss-near
       :miss-far n-miss-far
       :boat-loc 1))))





;;; ACTIONS
;;; ---------------------------------------------------------------
;;; Creates the individual possible actions using either
;;; MC-MOVE-NEAR or MC-MOVE-FAR. The names will use the format
;;; MOVE-(BANK MOVING TO)-(NUM CANNIBALS)-(NUM-MISSIONARIES)
;;; INPUTS: ORIG-STATE, the original MC-STATE struct
;;; OUTPUT: Either nil or an updated MC-STATE struct

(defun move-far-2-0
    (orig-state)
  (mc-move-far orig-state 2 0))

(defun move-far-1-1
    (orig-state)
  (mc-move-far orig-state 1 1))

(defun move-far-0-2
    (orig-state)
  (mc-move-far orig-state 0 2))

(defun move-far-1-0
    (orig-state)
  (mc-move-far orig-state 1 0))

(defun move-far-0-1
    (orig-state)
  (mc-move-far orig-state 0 1))

(defun move-near-2-0
    (orig-state)
  (mc-move-near orig-state 2 0))

(defun move-near-1-1
    (orig-state)
  (mc-move-near orig-state 1 1))

(defun move-near-0-2
    (orig-state)
  (mc-move-near orig-state 0 2))

(defun move-near-1-0
    (orig-state)
  (mc-move-near orig-state 1 0))

(defun move-near-0-1
    (orig-state)
  (mc-move-near orig-state 0 1))