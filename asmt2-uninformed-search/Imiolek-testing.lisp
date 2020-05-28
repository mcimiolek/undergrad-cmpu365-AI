;;; --------------------------------------------
;;;  CMPU-365, Spring 2019
;;;  Asmt. 2 
;;;  Testing File for Matthew Imiolek
;;; --------------------------------------------
;;;  To compile and load all files and run a few tests,
;;;  simply load this file:  (load "testing.lisp" :verbose nil)


(load "asmt-helper.lisp" :verbose nil)

(header "Matthew Imiolek" 2)

;; The following expressions ensure that tail-recursive function calls 
;; are handled appropriately/efficiently by the compiler.  

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

(defparameter *my-files* (list "basic-defns"
			       "gen-search-starter"
			       "vw"
			       "test-vw"
			       "mc"
			       ))

;; COMPILE and LOAD all of the RELEVANT files

(tester '(maker *my-files*))
(newline)

;; ---------------------------------
(problem "Vacuum World Testing")
;; ---------------------------------

(tester '(do-vw-depth))
(newline)
(tester '(do-vw-breadth))
(newline)
(tester '(do-vw-depth2))
(newline)
(tester '(do-vw-breadth2))

;; ---------------------------------
(problem "Missionaries and Cannibals Testing")
;; ---------------------------------

(tester '(do-mc-depth))
(newline)
(tester '(do-mc-breadth))
