;; ===================================
;;  CMPU-365, Spring 2019
;;  Asmt. 3
;;  IDS-STARTER.LISP
;; -----------------------------------
;;  Implementation of DLS and IDS from scratch, without using GEN-SEARCH.


;;  DLS-GUTS  --  should use TAIL-RECURSION or ITERATION
;; ------------------------------------------------------------------
;;  INPUTS:  PROB, a search problem instance
;;           LIMIT, a non-negative integer (cutoff limit for DLS)
;;           REZZY, a results struct (for compiling statistics)
;;  OUTPUT:  The destructively modified RESULTS struct

(defun dls-guts (prob limit rezzy)

        (let ((gt-func (search-problem-goal-test-func prob))
        (st-eq-func (search-problem-state-eq-func prob))
        (acts (search-problem-actions prob)))

          (labels
        ;;  DLS-REC  --  local tail-recursive helper function
        ;; ----------------------------------------------------
        ;;  INPUT:  QUEUE, a list of nodes (the search queue)
        ;;  OUTPUT:  The (destructively modified) RESULTS struct, REZZY
        ((dls-rec (queue cutoff-reached?)

           (cond

            ;; Base Case 1:  The search-queue is empty
            ((null queue)
             ;; Return REZZY, with GOAL-NODE=NIL, indicating failure
             rezzy)

            ;; Base Case 2:  The first node in the queue is a GOAL node
            ((funcall gt-func (node-state (first queue)))
             ;; Set GOAL-NODE in the RESULTS struct to that goal node, success!
             (setf (results-goal-node rezzy) (first queue))
             ;; Return the results
             rezzy)

             (cutoff-reached?
               (if (null (rest queue))
                (setf (results-goal-node rezzy) *cutoff*)
               (dls-rec (rest queue) nil)))

             ;; rec cas 1: cutoff reached - do not expand
            ((>= (node-depth (first queue)) limit)
              (dls-rec queue t))

            ;; Recursive Case:  More work to do
            (t
             ;; Expand first node in the queue (i.e., generate new
             ;; child nodes).  Then use the QUEUE-FN to insert those
             ;; new nodes into the REST of the queue.  The QUEUE-FN
             ;; determines whether you're doing breadth-first or
             ;; depth-first search.
             (dls-rec (front-enqueue!
                   (rest queue)
                   (expand (first queue)
                     acts
                     st-eq-func
                     rezzy)) nil)))))

            ;; BODY of LABELS (i.e., LETREC)
            ;; Call the recursive helper function with initial search queue

            (dls-rec (list (make-root-node prob)) (eq limit 0)))))

#| The inputs PROB and LIMIT are just passed through from the
DLS wrapper function; they won't change for DLS-GUTS.

REZZY is a results struct that you should use to keep track of
information during the search.  For example, whenever you use the
EXPAND function to create new nodes, you should increment the
NUM-NODES field of REZZY.  And when the search is finished, you
should set the GOAL-NODE field of REZZY to NIL, *CUTOFF*, or a
goal node.  (*CUTOFF* is a global constant defined in
"basic-defns.lisp".)

HINT: See the solution to GEN-SEARCH-GUTS in "basic-defns.lisp".
  It defines a local recursive helper function called GEN-REC.
  You can do the same thing for this function: Use the LABELS
  special form to create a local recursive function called DLS-REC
  that takes two inputs: QUEUE and CUTOFF-REACHED?, as follows.

    QUEUE is the search queue.  It starts out containing only the
    root node, but acquires new nodes created by the EXPAND
    function during the search.

    CUTOFF-REACHED? is a boolean flag that is used to keep track
    of whether the cutoff depth was ever reached during the
    search.  This is needed to distinguish the case where the
    search space was exhausted without hitting the cutoff depth
    and the case where no goal node was found but the cutoff depth
    was hit.  In the latter case, a future call to DLS with a
    bigger LIMIT might yield a goal node.

       the solution for GEN-SEARCH-GUTS, except that DLS-GUTS
       must deal with NIL vs. CUTOFF, and DLS-GUTS can *hardwire*
 the FRONT-ENQUEUE! queuing function.

       Before expanding a node, you simply check its depth.  If
       its depth is less than L, you expand it as usual; however,
       if its depth equals L, then you *don't* expand it (because
       its child nodes would be at depth L+1).

      ==> You can use EXPAND, CYCLE? and MAKE-ROOT-NODE
    from "basic-defns.lisp".

 When making your recursive function call to DLS-GUTS, be careful
 how you deal with CUTOFF-REACHED?  Also, be careful how you deal
 with the case of an empty search queue.

 By the way, you can test DLS on vacuum-world or missionaries and
 cannibals, as well as the eights-tile puzzle domain.  Using small
 depth limits makes things easy to check.  If you remove some of
 the actions, you can ensure that no solution will be found; that
 way you can check whether your NIL vs. CUTOFF answers are being
 correctly generated. |#

; 2 - 17

;;  IDS
;; -------------------------------------------------------
;;  INPUT:   PROBLEM, a search problem
;;  OUTPUT:  A RESULTS data structure that contains information
;;           about the results of doing IDS on the given search
;;           problem.

(defun ids (problem)

  (format t "==========================================~%")
  (format t " ITERATIVE-DEEPENING SEARCH ~%")
  (format t "==========================================~%")

  (let (
    (ids-rezzy (make-results :start-time (get-internal-run-time)))
    (loop-should-run t)
    (limit 0))
    (loop while loop-should-run do
      (let* ((dls-rezzy (dls problem limit))
             (goalie (results-goal-node dls-rezzy)))
        (setf (results-num-nodes ids-rezzy)
              (+ (results-num-nodes ids-rezzy)
                 (results-num-nodes dls-rezzy)))
        (print-results-stats dls-rezzy t)
        (cond
          ((eq goalie *cutoff*)
          (incf limit))
          (t
            (setf (results-goal-node ids-rezzy) (results-goal-node dls-rezzy))
          (setf loop-should-run nil)))))
    (setf (results-end-time ids-rezzy) (get-internal-run-time))
    (setf (results-elapsed-time ids-rezzy) (- (results-end-time ids-rezzy)
					  (results-start-time ids-rezzy)))
    ids-rezzy))
