;; =======================================================================
;;   CMPU-365, Spring 2019
;;   Final Project
;;   Matthew Imiolek, Amy O'Connell
;;   pm-test-2.lisp
;;   Testing function for pacman game
;; =======================================================================
;; Sets up a situation where Pacman should always win by moving left

;; DEF-PAC - a default pacman structs
(defconstant *test-pac2*
    (make-pacman
     :x 13
     :y 13))
;; DEF-GHOST-ONE - the default ghost one
(defconstant *test-ghost-one2*
    (make-ghost
     :x 13
     :y 12
     :ghost-id 101))
;; DEF-GHOST-TWO - the default ghost two
(defconstant *test-ghost-two2*
    (make-ghost
     :x 12
     :y 13
     :ghost-id 102))
;; DEF-GHOSTS - the default list of ghosts
(defconstant *test-ghosts2*
    (list *test-ghost-two2* *test-ghost-one2*))
;; DEF-BLANKS* - the default blank spaces in the board array
(defconstant *test-blanks2*
    '((7 5)
      (7 6)
      (6 7)
      (7 7)
      (8 7)
      (11 13)))
;; *DEF-PELLETS* - the default pellet locations in the board array
(defconstant *test-pellets2*
    '((12 13)))

(defvar test-game2 (init-game :pac *test-pac2* :ghosts *test-ghosts2* :blank-locs *test-blanks2* :pellet-locs *test-pellets2*))

(setf (game-whose-turn? test-game2) 4)
