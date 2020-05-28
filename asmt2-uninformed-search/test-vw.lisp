;;; -----------------------------------
;;;   CMPU-365, Spring 2019
;;;   Asmt. 2
;;;   FILE:  test-vw.lisp
;;; -----------------------------------
;;;  This file implements the MAKE-VW-PROBLEM, DO-VW-DEPTH and 
;;;  DO-VW-BREADTH functions that can be used to test the generic search 
;;;  code in "gen-search.lisp" on the implementation of the Vacuum World 
;;;  problem in "vw.lisp".


;;;  MAKE-VW-PROBLEM
;;; ----------------------------------------------------
;;;   Defines an instance of a search problem for the Vacuum World.
;;;   Use a simple problem instance until you get things working.

(defun make-vw-problem ()
  (make-search-problem 
   :init-state (make-vw-state :rob-loc '(0 0)
			      :dirt-locations '((0 0) (0 3) (1 2) (3 1) (3 3)))
   :actions (list #'north #'south #'east #'west #'vacuum)
   :goal-test-func #'vw-goal-test
   :state-eq-func #'vw-state-equal?))


;;; A second vw-problem to test on

(defun make-vw-problem2 ()
  (make-search-problem 
   :init-state (make-vw-state :rob-loc '(2 2)
			      :dirt-locations '((0 0) (1 0) (2 3) (3 2) (3 1)))
   :actions (list #'north #'south #'east #'west #'vacuum)
   :goal-test-func #'vw-goal-test
   :state-eq-func #'vw-state-equal?))


;;; ===============================================================
;;;   Helper functions for testing.
;;;   When ready to test, call:  (DO-VW-DEPTH) or (DO-VW-BREADTH).
;;; ===============================================================

;;  DO-VW-DEPTH
;; ------------------------------------------
;;  INPUTS:  None.
;;  OUTPUT:  A RESULTS struct that contains info about what
;;           happened from running Depth-First Search on the
;;           Vacuum World problem.

(defun do-vw-depth ()
  (depth-first-search (make-vw-problem)))


;; A second DO-VW-DEPTH to test on
(defun do-vw-depth2 ()
  (depth-first-search (make-vw-problem2)))

;;  DO-VW-BREADTH
;; ------------------------------------------
;;  INPUTS:  None.
;;  OUTPUT:  A RESULTS struct that contains info about what
;;           happened from running Breadth-First Search on the
;;           Vacuum World problem.

(defun do-vw-breadth()
  (breadth-first-search (make-vw-problem)))


;; A second DO-VW-BREADTH to test on
(defun do-vw-breadth2()
  (breadth-first-search (make-vw-problem2)))

