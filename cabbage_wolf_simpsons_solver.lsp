;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; DFSRL takes an argument fringe (one of: a tree, an atom, or an empty list) and returns
; a list corresponding to nodes in the tree in order of visitation via depth first search
(defun DFSRL (FRINGE)

(cond ((not   FRINGE) NIL)               ; fringe is empty list, return nil
      ((atom  FRINGE) (cons FRINGE NIL)) ; fringe is a singleton, return fringe
      (t (append (DFSRL (rest FRINGE)) (DFSRL (first FRINGE))))
      ; else fringe is a list, append recursive calls
) ; cond
) ; defun

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;

; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.

(defun FINAL-STATE (S)
    (equal S '(T T T T))
) ; defun

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).

(defun NEXT-STATE (S A)
    ; For each case, we check that entities are on the same side, and not positions
    (let ((s-prime
    (cond ((equal A 'h) (cons (not (first S)) (rest S)))                ; (!h b d p)
          ((and (equal A 'b) (equal (first S) (second S)))
          (list (not (first S)) (not (second S)) (third S) (fourth S))) ; (!h !b d p)
          ((and (equal A 'd) (equal (first S) (third S))) 
          (list (not (first S)) (second S) (not (third S)) (fourth S))) ; (!h b !d p)
          ((and (equal A 'p) (equal (first S) (fourth S))) 
          (list (not (first S)) (second S) (third S) (not (fourth S)))) ; (!h b d !p)
          (t NIL) ; invalid input
    )  ; cond
    )) ; define s-prime

    (if (and (not (equal (first  s-prime) (second s-prime)))  ; if homer and baby are not on same side
        (or       (equal (second s-prime) (third  s-prime))   ; and either baby is unsupervised with dog
                  (equal (second s-prime) (fourth s-prime)))) ; or baby is unsupervised with poison                 
        NIL      ; this is illegal move
        s-prime  ; else return new position

) ; endif 
) ; let s-prime
) ; defun

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    
    ; return list with either NIL or next-state for each of the possible four moves
    (list (NEXT-STATE s 'h) (NEXT-STATE s 'b) (NEXT-STATE s 'd) (NEXT-STATE s 'p))

) ; defun

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)

    (if (not   STATES) NIL       ; states is empty list, return nil
    (if (equal (first STATES) s) ; else if first member of STATES == S
    T                            ; return T
    (ON-PATH S (rest STATES)     ; else recursively call ON-PATH (endif)
          
          
))) ; endif 
)   ; defun

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)

    (if (not STATES) NIL                    ; reached end of list, return nil
    (let ((next (DFS (first STATES) PATH))) ; run DFS on first possible move
    (if (not next)                          ; if DFS did not find goal state
    (MULT-DFS (rest STATES) PATH)           ; re-run MULT-DFS on remainder of states
    next ))                                 ; else we are done
                  
) ; cond
) ; defun

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

(defun DFS (S PATH)

(cond ((not S) NIL)                            ; reached end, return nil (never happens)
      ((FINAL-STATE S) (append PATH (list S))) ; this is final state, return full path
      ((ON-PATH S PATH) NIL)                   ; we have been here before, return nil
      ; else we need to expand this node 
      (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))

) ;cond
) ; defun