;; ========================
;;  CMPU-365, Spring 2019
;;  Asmt. 3
;;  Testing File
;; ------------------------

(load "asmt-helper.lisp")

(header "Solutions!" "3")

;;  Making sure that the TAIL-RECURSIVE optimization is done
;;  by the compiler

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t)

(defconstant *list-o-files*
    (list "basic-defns"
	  "ids-starter"
	  "a-star-starter"
	  "eights"
	  "heuristic-starter"
	  "rubik"
	  "rubik-probs-starter"))

;; ========================
;;  TESTS
;; ========================

;;  TEST-EIGHTS-IDS
;; -------------------------------
;;  INPUTS:  LOCS, a list-of-lists representation of the inital locations
;;             of the tiles in an eights puzzle, where 0 represents the
;;             blank;
;;           BX, BY, the row and column of the blank in the initial
;;              tile puzzle indicated by LOCS
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays results of doing IDS on the specified
;;                  eights tile puzzle.

(defun test-eights-ids (locs bx by)
  (ids (make-eights-problem locs bx by)))

;;  TEST-IDS-1 / TEST-IDS-2
;; ------------------------------------------------------
;;  Run IDS on different eights tile puzzles.

(defun test-eights-ids-1 ()
  (test-eights-ids '((1 2 3)
		     (4 0 5)
		     (6 8 7)) 1 1))

(defun test-eights-ids-2 ()
  (test-eights-ids '((1 8 2)
		     (7 6 0)
		     (5 3 4)) 1 2))

;;  TEST-EIGHTS-STAR
;; --------------------------------------------------
;;  INPUTS:  LOCS, a vector representing the initial locations
;;                of an eights tile puzzle
;;           BX, BY, the row and column of the blank in that
;;                that initial tile puzzle
;;           HEURISTIC, a heuristic function suitable for the
;;                eights tile domain
;;  OUTPUT:  RESULTS struct

(defun test-eights-star (locs bx by heuristic)
  (a-star-search (make-eights-problem locs bx by)
		 heuristic))

;;  TEST-EIGHTS-STAR-1m / TEST-EIGHTS-STAR-1nt
;; ----------------------------------------------------------
;;  Comparing the performance of A-STAR on the same puzzle using
;;  the NUM-TILES and MANHATTAN-DISTANCE heuristics.

(defun test-eights-star-1m ()
  (test-eights-star '((1 2 3)
		      (4 0 5)
		      (6 8 7)) 1 1 #'manhattan-heuristic))

(defun test-eights-star-1nt ()
  (test-eights-star '((1 2 3)
		      (4 0 5)
		      (6 8 7)) 1 1 #'num-tiles-heuristic))

;;  TEST-EIGHTS-STAR-2M / TEST-EIGHTS-STAR-2NT
;; -----------------------------------------------------
;;  Another comparison on another puzzle.

(defun test-eights-star-2m ()
  (test-eights-star '((1 8 2)
		      (7 6 0)
		      (5 3 4)) 1 2 #'manhattan-heuristic))

(defun test-eights-star-2nt ()
  (test-eights-star '((1 8 2)
		      (7 6 0)
		      (5 3 4)) 1 2 #'num-tiles-heuristic))

(defun test-rube-ids-three-corners-rotated
    ()
  (ids (make-fancy-rube-problem #'three-corners-rotated
				*rube-ops*)))

(defun test-rube-ids-two-corners-swapped
    ()
  (ids (make-fancy-rube-problem #'two-corners-swapped
				*rube-ops*)))

(defun test-rube-ids-two-corners-swapped-six-moves
    ()
  (ids (make-fancy-rube-problem #'two-corners-swapped
				(append *rube-ops* *rube-counter-ops*))))

(defun test-rube-ids-two-corners-rotated
    ()
  (ids (make-fancy-rube-problem #'two-corners-rotated
				*rube-ops*)))

(defun test-rube-ids-two-three-swapped
    ()
  (ids (make-fancy-rube-problem #'two-three-swapped
				*rube-ops*)))

(defun do-all-tests
    ()
  ;; MAKER is defined in asmt-helper.lisp
  ;; It compiles and loads all relevant files
  (maker *list-o-files*)
  ;; The tests
  (fancy-tester '(test-eights-ids-1))
  (fancy-tester '(test-eights-star-1nt))
  (fancy-tester '(test-eights-star-1m))
  (fancy-tester '(test-eights-ids-2))
  (fancy-tester '(test-eights-star-2nt))
  (fancy-tester '(test-eights-star-2m))
  (fancy-tester '(test-rube-ids-three-corners-rotated))
  (fancy-tester '(test-rube-ids-two-corners-swapped))
  (fancy-tester '(test-rube-ids-two-corners-rotated))
  (fancy-tester '(test-rube-ids-two-three-swapped)))
(do-all-tests)
