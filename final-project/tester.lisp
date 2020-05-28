;; =======================================================================
;;   CMPU-365, Spring 2019
;;   Final Project
;;   Matthew Imiolek, Amy O'Connell
;;   pm-test.lisp
;;   Testing functions for pacman game
;; =======================================================================
;; Runs our Pacman tests!!!

(defun cl (filename)
  ;;  COMPILER-FLAGS
  (setq compiler:tail-call-self-merge-switch t)
  (setq compiler:tail-call-non-self-merge-switch t)
  (compile-file filename :verbose nil)
  (load filename :verbose nil)
  t)

(cl "pacman.lisp")
(cl "search-ghost.lisp")
(cl "alpha-beta.lisp")
(cl "pm-test.lisp")
(cl "pm-test-2.lisp")

(comp-do-n-show test-game 2 4)

(comp-do-n-show test-game2 2 4)
