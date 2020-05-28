;;; ========================================
;;;   CMPU-365, Spring 2019
;;;   Asmt. 2
;;;   Matthew Imiolek
;;; ========================================
;;;   FILE:  gen-search-starter.lisp
;;; ========================================
;;;   General tree-search algorithm.  Special cases
;;;   include breadth-first and depth-first search.

;;;  The "basic-defns" file definethe PROBLEM and NODE
;;;  data structures and some related low-level functions.

(load "basic-defns" :verbose nil)





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

(defun cycle? 
    (state node state-eq-func)
  (cond
   ;; Checks if the parent node is nil, which indicates there is
   ;; no parent, and that thus none of the ancestors were of the
   ;; given state
   ((equal node nil) nil)
   ;; Returns true if the states are equal
   ((funcall state-eq-func (node-state node) state) t)
   ;; If there is a parent node, recursively checks if the parent
   ;; node is the same as the given state for a cycle
   (t (cycle? state (node-parent node) state-eq-func))))





;;;  MAKE-ROOT-NODE 
;;; ---------------------------------------
;;;  INPUT:  PROB, a search problem
;;;  OUTPUT:  A search NODE that will be the ROOT NODE of a
;;;           brand new search tree.

(defun make-root-node 
    (prob)
  (make-node
   :state (search-problem-init-state prob)
   :parent nil
   :action nil
   :depth 0))





;;;  CLEAR-EXTRA
;;; ---------------------------------
;;;  INPUTS: ST-EQ-FUNC, a function for testing whether two
;;;             states are equal
;;;          LIST-NODES, a list of nodes
;;;          NODE, a node
;;;  OUTPUT: A list of nodes without repeats of the give NODE
;;;          or the nil values caused by attempting invalid
;;;          moves

(defun clear-extra 
    (st-eq-func list-nodes node)
  (remove-if #'(lambda (x) 
		 (or
		  ;; Removes the extra nils present in the list
		  (equal (node-state x) nil)
		  ;; Removes any nodes the are found as cycles
		  ;; in CYCLE?
		  (cycle? (node-state x) node st-eq-func)))
	     list-nodes))
  




;;;  EXPAND 
;;; ---------------------------------
;;;  INPUTS:  NODE, a search node
;;;           ACTS, a list of actions
;;;           ST-EQ-FUNC, a function for testing whether two
;;;              states are equal
;;;  OUTPUT:  A list of child nodes obtained by applying the
;;;           given list of actions to NODE's state.  However, it
;;;           does *NOT* include child nodes whose states already
;;;           appear on the path from the root node to NODE.
;;;           (Uses CYCLE? to determine this.)

(defun expand 
    (node acts st-eq-func)
  ;; Define a helper function EXPAND-ACC which adds a counter
  (labels ((expand-acc (node acts acc)
	     (cond
	      ;; Base case for when the list of actions is empty
	      ((equal (car acts) nil) nil)
	      ;; If not the base case, generates a new node and adds
	      ;; it to a list using a recursive call to EXPAND-ACC
	      (t (cons (make-node
			;; State is based off of the head of the
			;; the actions list
			:state (funcall (car acts) (node-state node))
			;; Parent is the node being used with the
			;; action to create a new node
			:parent node
			;; The action last used is the head of the actions
			;; list
			:action (car acts)
			;; Depth is the previous nodes depth + 1
			:depth (+ (node-depth node) 1))
		       (expand-acc node (cdr acts) (+ 1 acc)))))))
    ;; Use CLEAR-EXTRA on the list generated after an initial call to
    ;; EXPAND-ACC
    (clear-extra st-eq-func (expand-acc node acts 0) node)))
	       




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

(defun gen-search-guts 
    (problem queue-fn rezzy)
  ;; Creates a helper function that adds NODE-LIST, a list of nodes, to the other inputs
  ;; required by GEN-SEARCH-GUTS
  (labels ((gen-search-guts-list
	       (problem queue-fn rezzy node-list) 
	     (cond
	      ;; Returns nil if the queue of nodes is empty
	      ((equal (list-length node-list) 0) nil)
	      ;; Checks if a valid result has been found, and returns it if it has
	      ((funcall (search-problem-goal-test-func problem) (node-state (car node-list)))
	       (progn
		  (setf (results-goal-node rezzy) (car node-list))
		  (setf (results-num-nodes rezzy) (list-length node-list))))
	      ;; Otherwise recursively calls GEN-SEARCH-GUTS-LIST after updating the queue of nodes
	      ;; using EXPAND and QUEUE-FN
	      (t 
	       (gen-search-guts-list problem queue-fn rezzy 
				     (funcall queue-fn (cdr node-list) 
					       (expand (car node-list) 
						       (search-problem-actions problem) 
						       (search-problem-state-eq-func problem))))))))
    ;; Initial call to GEN-SEARCH-GUTS-LIST using MAKE-ROOT-NODE to create the initial node for
    ;; NODE-LIST
    (gen-search-guts-list problem queue-fn rezzy (cons (make-root-node problem) nil))))
