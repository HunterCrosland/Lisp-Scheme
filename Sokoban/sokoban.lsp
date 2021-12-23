;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
	;(print "in isblank")
  (= v blank)
  )

(defun isWall (v)
	;(print "in iswall")
  (= v wall)
  )

(defun isBox (v)
	;(print "in isbox")
  (= v box)
  )

(defun isKeeper (v)
	;(print "in iskeeper")
  (= v keeper)
  )

(defun isStar (v)
	;(print "in isstar")
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
	;(print "in iskstar")
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

(defun goal-test (s)
  (cond ((not s) T) 											; empty list has no boxes
	    ((atom s) (if (isBox s) NIL T)) 						; if item is box, nil, else T
	    (t (and (goal-test (first s)) (goal-test (rest s))))	; else recursively search lists
);cond
);end defun


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; This is the skeletong code altered with the suggested 4-way test

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT))))
	 (cleanUpList result);end)
	
	 
    
   );end let
);

; try-move is a 
(defun try-move (s dir)
		(let* ((keeper-pos (getKeeperPosition s 0))
	(new-keeper-pos 
		; for up and down, decrement or increment y-coordinate of keeper
		(cond ((equal dir 'UP)    (list (first keeper-pos) (- (second keeper-pos) 1)))
		 	  ((equal dir 'DOWN)  (list (first keeper-pos) (+ (second keeper-pos) 1)))
		; for left and right, increment or decrement x-coordinate of keeper
			  ((equal dir 'LEFT)  (list (- (first keeper-pos) 1) (second keeper-pos) 1))
			  ((equal dir 'RIGHT) (list (+ (first keeper-pos) 1) (second keeper-pos) 1))
	) ; cond dirs
	) ; def new-keeper-pos
	(replacement-object (get-object-at new-keeper-pos s))) ;def replacement-object

	(cond ((not replacement-object) nil)
	
		  ((isBlank replacement-object) 
		   (set-object-at keeper-pos 
		   (set-object-at new-keeper-pos s keeper) 
		   (old-keeper-position keeper-pos s)))

		  ((isStar replacement-object) 
		   (set-object-at keeper-pos 
		   (set-object-at new-keeper-pos s keeperStar) 
		   (old-keeper-position keeper-pos s)))

		  ((isWall  replacement-object) nil)

		  ((or (isBox replacement-object) (isBoxStar replacement-object))

			(if (not (handle-box-move s new-keeper-pos dir)) nil
						(set-object-at keeper-pos 
						(handle-box-move s new-keeper-pos dir) 
						(old-keeper-position keeper-pos s)))
) ; cond
) ; let new-keeper-pos, replacement-object
)) ; defun

(defun all-boxstar (s list-of-boxes)
	(print list-of-boxes)
	(cond ((not list-of-boxes) T)
		  ((equal 1 (length list-of-boxes)) 
		  (if (not (first list-of-boxes)) T (isBoxStar list-of-boxes)))
		  (t (and (isBoxStar (first list-of-boxes)) 
		  		(all-boxStar s (rest list-of-boxes)))))
)

(defun old-keeper-position (xy s)
(cond ((isKeeper 	 (get-object-at xy s)) blank)
	  ((isKeeperStar (get-object-at xy s)) star))
)

; get-object-at returns the object type at a given
; coordinate on the game state grid, values 0-6
(defun get-object-at (xy s)
(cond ((or (not (first xy)) (not (second xy))) wall)
	  ((or (< (first xy) 0) (< (second xy) 0)) wall)
	  (t (nth (first xy) (nth (second xy) s))))
) ; defun

(defun handle-box-move (s xy dir)
(let* ((new-box-pos 
		; for up and down, decrement or increment y-coordinate of box
		(cond ((equal dir 'UP)    (list (first xy) (- (second xy) 1)))
		 	  ((equal dir 'DOWN)  (list (first xy) (+ (second xy) 1)))
		; for left and right, decrement or increment x-coordinate of box
			  ((equal dir 'LEFT)  (list (- (first xy) 1) (second xy) 1))
			  ((equal dir 'RIGHT) (list (+ (first xy) 1) (second xy) 1))
	) ; cond dirs
	) ; def new-box-pos

(replacement-object (get-object-at new-box-pos s))) ;def replacement-object

	(cond ((not replacement-object) nil)

		  ((isBlank replacement-object) 
		   (set-object-at xy 
		   (set-object-at new-box-pos s box) 
		   keeper))

		  ((isStar replacement-object) 
		   (set-object-at xy 
		   (set-object-at new-box-pos s boxStar) 
		   keeperStar))

 		  ((or (isWall replacement-object) (isBox replacement-object) (isBoxStar replacement-object)) nil)

) ; cond
))

; set-object-at and its follow-up function, set-xy-coord, return
; a board state with value at specified coordinate replaced with
; newval. 

; set-object-at handles row separation
(defun set-object-at (xy s newval)
(append 
(butlast s (- (get-num-rows s) (second xy))) 		; beginning of list 
(list (set-xy-coord xy (nth (second xy) s) newval)) ; new row
(nthcdr (+ (second xy) 1) s) 			 	 		; end of list
)) ; append, defun

(defun check-deadlocks (s list-of-boxes)
	(cond ((not  list-of-boxes) nil)
		  ((atom list-of-boxes) nil)
		  (t (if (check-vert-deadlocks s (first list-of-boxes))

		  (append (check-horz-deadlocks s (first list-of-boxes)) 
		  	   	(check-deadlocks s (rest list-of-boxes))) 

		  (check-deadlocks s (rest list-of-boxes))))
		  
)	
)

(defun check-vert-deadlocks (s box)
(if (not box) nil
	
	(let* ((up-object   (get-object-at (list (first box) (- (second box) 1)) s))
		   (down-object (get-object-at (list (first box) (+ (second box) 1)) s))

		   (has-vwall-block  (or (equal wall up-object) (equal wall down-object))))

	(cond (has-vwall-block box)
		  ((and (isBox up-object) (isBox down-object)) 
		   (cleanUpList (append (check-vert-deadlocks (set-object-at box s wall) (list (first box) (- (second box) 1)))
				 			  (check-vert-deadlocks (set-object-at box s wall) (list (first box) (+ (second box) 1))))))
		  ((isBox down-object) (check-vert-deadlocks (set-object-at box s wall) (list (first box) (+ (second box) 1))))
		  ((isBox   up-object) (check-vert-deadlocks (set-object-at box s wall) (list (first box) (- (second box) 1))))
		  (t nil)
)))

) ; defun

(defun check-horz-deadlocks (s box)
	(if (not box) T
	
	(let* ((left-object  (get-object-at (list (- (first box) 1) (second box)) s))
		   (right-object (get-object-at (list (+ (first box) 1) (second box)) s))

		   (has-hwall-block  (or (equal wall left-object) (equal wall right-object))))

	(cond (has-hwall-block box)
		  ((and (or (isBox right-object) (isBoxStar right-object)) (or (isBox left-object) (isBoxStar left-object))) 
		   (cleanUpList (append (check-vert-deadlocks (set-object-at box s wall) (list (+ (first box) 1) (second box)))
				 	    (check-vert-deadlocks (set-object-at box s wall) (list (- (first box) 1) (second box))))))
		  ((or (isBox right-object) (isBoxStar right-object)) 
		  	(check-vert-deadlocks (set-object-at box s wall) (list (+ (first box) 1) (second box))))
		  ((or (isBox  left-object) (isBoxStar  left-object)) 
		  	(check-vert-deadlocks (set-object-at box s wall) (list (- (first box) 1) (second box))))
		  (t nil)
))))

; set-xy-coord handles column separation
(defun set-xy-coord (xy s newval)
(append 
(butlast s (- (get-num-rows s) (first xy))) ; beginning of list 
(list newval) 								; newval
(nthcdr (+ (first xy) 1) s) 			 	; end of list
)) ; append, defun

; get-num-rows is a helper function for set-object-at which gets
; the number of rows in a two-dimensional array (or number of 
; characters in a one-dimensional array) in order to invoke
; butlast and nthcdr.
(defun get-num-rows (s) 
	(cond ((not s)  0)						 ; reached end of list, return 0
		  ((atom s) 1) 				   		 ; reached last ele of list, return 1
		  (t (+ (get-num-rows (rest s)) 1))) ; add 1 for each element in list	
)



; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; Modified goal-test: we really just need to add 1
; when we find a box instead of returning NIL
(defun h1 (s)
  	(cond ((not s) 0) 						   ; empty list has no boxes
	      ((atom s) (if (isBox s) 1 0)) 	   ; if item is box, 1, else 0
	      (t (+ (h1 (first s)) (h1 (rest s)))) ; else recursively add list search results
) ; cond
) ; defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; get-object-y-coords recursively steps through each of the rows of s and passes them into
; get-object-x-coords, and ultimately returns a list of coordinates which correspond to 
; the occurrences of obj in s. 
(defun get-object-y-coords (orig-s s obj xy)
	; if s is nil, we have completed the step-through; return nil
	(if (not s) nil 

	; otherwise we want to recursively step through rows with get-object-y-coords and columns
	; with get-object-x-coords. 
		(append (get-object-x-coords orig-s (first s) obj (list (first xy) (second xy)))
				(get-object-y-coords orig-s (rest  s) obj (list (first xy) (+ 1 (second xy)))))
) ; cond
) ; defun

; get-object-x-coords recursively steps through each of the columns of s and returns a list 
; of coordinates which correspond to the occurrences of obj in s. 
(defun get-object-x-coords (orig-s s obj xy)
	; if s is nil, we have completed the step-through; return nil
	(cond ((not s)  nil)

	; if s is a singleton, check if it is obj, return its coordinates if so and nil if not
		  ((atom s) 
		  	(if (equal obj (get-object-at xy orig-s)) (list xy) nil))

	; otherwise we want to recursively step through the columns
		  (t (append (get-object-x-coords orig-s (first s) obj (list (+ 1 (first xy)) (second xy)))
		  		  	 (get-object-x-coords orig-s (rest  s) obj (list (+ 1 (first xy)) (second xy)))))
) ; cond
) ; defun

; custom-manhattan returns the manhattan distance of two objects, with an extra 2 steps if
; the objects are on different rows, since the keeper needs to shift twice to reorient
(defun custom-manhattan (obj1 obj2)
	(if  (equal (second obj1) (second obj2))
	(+   (abs (- (first obj1) (first obj2))) (abs (- (second obj1) (second obj2))))  ; | x_1 - x_2 | + |y_1 - y_2| 
	(+ 2 (abs (- (first obj1) (first obj2))) (abs (- (second obj1) (second obj2))))) ; | x_1 - x_2 | + |y_1 - y_2| + 2 
) ; defun

; shortest-manhattan returns the shortest custom-manhattan distance between a given box and
; all of the stars. 
(defun shortest-manhattan (box list-of-stars)
	; we have reached end of stars, return 0
	(if (not list-of-stars) 0 
	; if we are looking at just one star, measure custom-manhattan for single box and star
	(if (equal (length list-of-stars) 1) 
		(custom-manhattan box (first list-of-stars))
	; otherwise return minimum distance recursively
		(min 
		(custom-manhattan   box (first list-of-stars)) 
		(shortest-manhattan box (rest  list-of-stars))))
	)
	
) ; defun

; keeper-manhattan replaces shortest-manhattan when we are calculating
; the distance of the keeper from the boxes; specifically, we swap min
; for + in the else case in order to sum the distances from the keeper
; to each of the boxes, rather than find the smallest one

(defun keeper-manhattan (box list-of-stars)
	; we have reached end of stars, return 0
	(if (not list-of-stars) 0 
	; if we are looking at just one star, measure custom-manhattan for single box and star
	(if (equal (length list-of-stars) 1) 
		(custom-manhattan box (first list-of-stars))
	; otherwise return minimum distance recursively
		(+ 
		(custom-manhattan   box (first list-of-stars)) 
		(shortest-manhattan box (rest  list-of-stars))))
	)
	
) ; defun

; all-manhattan-distances recursively steps through each box in list-of-boxes and calls 
; shortest-manhattan to find the shortest manhattan distance for each box, and sums the 
; result of each of these calls
(defun all-manhattan-distances (list-of-boxes list-of-stars)
	; if there are no boxes, return 0
	(if (not list-of-boxes) 0 
	; else recursively step through the list of boxes and call shortest-manhattan for each
	(+ (shortest-manhattan   (first list-of-boxes) list-of-stars) 
	(all-manhattan-distances (rest  list-of-boxes) list-of-stars))
)) ; defun

; h905153494 is essentially a manhattan distance heuristic with some
; sokoban-specific additives
(defun h905153494 (s)


	; generate list-of-boxes and list-of-stars 
	(let* ((list-of-boxes (get-object-y-coords s s box  '(-1 0)))
		   (list-of-stars (get-object-y-coords s s star '(-1 0))))

	; pass keeper position and list-of-boxes into shortest-manhattan
	; pass list-of-boxes and list-of-stars into all-manhattan-distances
	; add the two returned values

	;(* 100 (+ (keeper-manhattan (getKeeperPosition s 0) list-of-boxes) 
	;(all-manhattan-distances list-of-boxes list-of-stars)))

	(+ (keeper-manhattan (getKeeperPosition s 0) list-of-boxes) 
	(all-manhattan-distances list-of-boxes list-of-stars))

)) ;let*
 ;defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	        (1 0 0 0 1 1 0)
	        (1 3 2 0 0 1 1)
	        (1 1 0 2 0 0 1)
            (0 1 1 0 2 0 1)
            (0 0 1 1 0 0 1)
            (0 0 0 1 1 4 1)
            (0 0 0 0 1 4 1)
            (0 0 0 0 1 4 1)
            (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun 
