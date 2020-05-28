;; ------------------------------------------
;;  CMPU-365, Spring 2019
;;  Asmt. 3
;;  HEURISTIC-STARTER.LISP
;; ------------------------------------------
;;  An implementation of the MANHATTAN-DISTANCE heuristic


;;;  MANHATTAN-DISTANCE
;;; ------------------------------------------------------
;;;  INPUT:   STATE, an EIGHTS struct
;;;  OUTPUT:  The sum of the manhattan distances of the
;;;           tiles in STATE from their goal positions.
;;;  NOTE:  The blank is not counted.

(defun manhattan-distance (state)
;(format t "state: ~A~%" state)
;(format t "goal-array: ~A~%" goal-array)
  (let* (
    (goal-hash (make-hash-table))
    (locs (eights-locations state))
   (size (array-dimensions locs))
   (mh-acc 0))
  (dotimes (i (first size))
    (dotimes (j (second size))
       (setf (gethash (aref goal-array i j) goal-hash) (list i j))))
      ;; Walk through the different tile locations
   (dotimes (i (first size))
      (dotimes (j (second size))
        (let* ((search-tile (aref locs i j))
              (goal-loc (gethash search-tile goal-hash))
              (diffi (abs (- (first goal-loc) i)))
              (diffj (abs (- (second goal-loc) j))))
              ;(format t "move tile: ~A; diffi: ~A; diffj: ~A~%" search-tile diffi diffj))
              (if (= search-tile 0)
              nil
              (setf mh-acc (+ mh-acc diffi diffj))))))
    mh-acc))





;;;  MANHATTAN-HEURISTIC
;;; --------------------------------------------------
;;;  INPUT:  NODE, a node struct
;;;  OUTPUT:  The value of the manhattan-distance function
;;;           as applied to NODE's STATE.

(defun manhattan-heuristic (node)
  (manhattan-distance (node-state node)))
