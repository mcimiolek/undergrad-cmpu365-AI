;; ==================================
;;  CMPU-365, Spring 2019
;;  Asmt. 3
;;  RUBIK-PROBS-STARTER.LISP
;; ==================================

;;  TWO-CORNERS-ROTATED --  a GOAL-TEST function
;; ---------------------------------------------------------
;;  INPUT:  RUBY, a rubik's cube
;;  OUTPUT:  T, if all the cubes of RUBY are in their unscrambled
;;    locations with their unscrambled orientations, except that
;;    cubes 4 and 6 have been rotated:  4 clockwise, 6 counter-clockwise
;;    (while still in their original locations).

(defun two-corners-rotated
    (ruby)
    (let ((locs (rubik-positions ruby))
  	(oreos (rubik-orientations ruby)))
      (and
       ;; original locations
       (vector-equal locs #(0 1 2 3 4 5 6))
       ;; original orientations for cubes 0 thru 3, and 5
       (= 0
  	(aref oreos 0)
  	(aref oreos 1)
  	(aref oreos 2)
  	(aref oreos 3)
    (aref oreos 5))
       ;; cube 4 rotated clockwise, cude 6 rotated counter clockwise
       (= (aref oreos 4) *y*)
       (= (aref oreos 6) *y*))))

;;  TWO-THREE-SWAPPED --  a GOAL-TEST function
;; ---------------------------------------------------------
;;  INPUT:  RUBY, a rubik's cube
;;  OUTPUT:  T, if all the cubes of RUBY are in their unscrambled
;;    locations with their unscrambled orientations, except that
;;    cubes 2 and 3 have been swapped without caring about orientation.

(defun two-three-swapped
   (ruby)
   (let ((locs (rubik-positions ruby))
 	(oreos (rubik-orientations ruby)))
     (and
      ;; Swap 2 and 3
      (vector-equal locs #(0 1 3 2 4 5 6))
      ;; original orientations for cubes 0, 1, 4, 5, and 6
      (= 0
 	(aref oreos 0)
 	(aref oreos 1)
 	(aref oreos 4)
 	(aref oreos 5)
  (aref oreos 6)))))
