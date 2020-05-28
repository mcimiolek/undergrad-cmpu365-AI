;; =======================================================================
;;   CMPU-365, Spring 2019
;;   Final Project
;;   Matthew Imiolek, Amy O'Connell
;;   pm-test.lisp
;;   Testing functions for pacman game
;; =======================================================================
;; Sets up a situation where Pacman should always loose, but should move left
;; to prolong that process

;; DEF-PAC - a default pacman structs
(defconstant *test-pac1*
    (make-pacman
     :x 13
     :y 13))
;; DEF-GHOST-ONE - the default ghost one
(defconstant *test-ghost-one1*
    (make-ghost
     :x 13
     :y 12
     :ghost-id 101))
;; DEF-GHOST-TWO - the default ghost two
(defconstant *test-ghost-two1*
    (make-ghost
     :x 11
     :y 13
     :ghost-id 102))
;; DEF-GHOSTS - the default list of ghosts
(defconstant *test-ghosts1*
    (list *test-ghost-two1* *test-ghost-one1*))
;; DEF-BLANKS* - the default blank spaces in the board array
(defconstant *test-blanks1*
    '((7 5)
      (7 6)
      (6 7)
      (7 7)
      (8 7)))
;; *DEF-PELLETS* - the default pellet locations in the board array
(defconstant *test-pellets1*
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

(defvar test-game (init-game :pac *test-pac1* :ghosts *test-ghosts1* :blank-locs *test-blanks1*))

(setf (game-whose-turn? test-game) 3)
