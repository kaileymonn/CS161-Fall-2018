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
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
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

;---------------------------------------------------------------------------;
; EXERCISE: Returns true <=> s is a goal state of the game
;---------------------------------------------------------------------------;
(defun goal-test (s)
	(cond 
		((null s) t)
		((atom s) 
			(cond 
				((or (isBox s) (isKeeper s)) NIL)
				(t t)
			)
		)
		(t (and (goal-test (first s)) (goal-test (rest s))))
	)
);end defun

;---------------------------------------------------------------------------;
; Helper function:
;		IGets number of rows in a state s
;---------------------------------------------------------------------------;
(defun num-rows (s)
	(cond 
		((null s) 0)
		(t (length s))
	)
)

;---------------------------------------------------------------------------;
; Helper function:
;		IGets number of columns in a state s
;---------------------------------------------------------------------------;
(defun num-cols (s)
	(cond 
		((null s) 0)
		(t (length (first s)))
	)
)

;---------------------------------------------------------------------------;
; Helper function for get-square:
;		In row L, gets the kth element and returns it
;---------------------------------------------------------------------------;
(defun get-kth-elem (L k)
	(cond 
		((> k 0) (get-kth-elem (rest L) (- k 1)))
		(t (first L))
	)
)

;---------------------------------------------------------------------------;
; EXERCISE: Takes state S, row number r, col number c, returns integer
; 					content of state S at square (r,c).
;---------------------------------------------------------------------------;
(defun get-square (S r c)
	(let (
		(rows (num-rows S))
		(cols (num-cols S))
		) 
		(cond
			((<= rows r) 1)
			((<= cols c) 1)
			((< r 0) 1)
			((< c 0) 1)
			((> r 0) (get-square (rest S) (- r 1) c))
			(t (get-kth-elem (first S) c))
		)
	)
)

;---------------------------------------------------------------------------;
; Helper function for set-square:
;		In row L, sets kth element to v and returns a new list L'
;---------------------------------------------------------------------------;
(defun copy-set-kth-elem  (L k v)
	(let* 
		(
			(size (length L))
			(before (butlast L (- size k)))
			(after (nthcdr (+ k 1) L))
		)
		(append before (list v) after)
	)
)

;---------------------------------------------------------------------------;
; EXERCISE: Takes state S, row number r, col number c, square content v and
;						returns new state S' that is obtained by setting the square (r,c)
;						to value v. Does not modify input state.
;---------------------------------------------------------------------------;
(defun set-square (S r c v)
	(cond 
		((> r 0) 
			(cons (first S) (set-square (rest S) (- r 1) c v))
		)
		(t (cons (copy-set-kth-elem (first S) c v) (rest S)))
	)
)

;---------------------------------------------------------------------------;
; EXERCISE: Helper function for next-states.
;						Takes as argument state S and move direction D, computes the 
;						state that is the result of moving the keeper in state S in 
;						direction D. 
; D = 0 : UP
; D = 1 : DOWN
; D = 2 : LEFT
; D = 3 : RIGHT
;---------------------------------------------------------------------------;
(defun try-move (S D)
	(let* (
			(keeperRow (second (getKeeperPosition S 0)))
			(keeperCol (first (getKeeperPosition S 0)))
			(nextCoord 
				(cond 
					((= D 0) (list (- keeperRow 1) keeperCol)) ; Up
					((= D 1) (list (+ keeperRow 1) keeperCol)) ; Down
					((= D 2) (list keeperRow (- keeperCol 1))) ; Left
					((= D 3) (list keeperRow (+ keeperCol 1))) ; Right
					(t NIL)
				)
			)
			(nextSquare	(get-square S (first nextCoord) (second nextCoord)))
			(currSquare (get-square S keeperRow keeperCol)))

		(cond 
			; if nextSquare is empty or nextSquare is goal, move
			((or (isBlank nextSquare) (isStar nextSquare)) 
				(let* (
						(result 
							(cond 
								((isBlank nextSquare) (set-square S (first nextCoord) (second nextCoord) 3))
								(t (set-square S (first nextCoord) (second nextCoord) 6))))
						(result 
							(cond 
								((isKeeper currSquare) (set-square result keeperRow keeperCol 0))
								((isKeeperStar currSquare) (set-square result keeperRow keeperCol 4))
								(t NIL))))
						result))

			; elif nextSquare has box, check then move
			((or (isBox nextSquare) (isBoxStar nextSquare))
				(let* 
					((nextNextCoord 
							(cond 
								((= D 0) (list (- (first nextCoord) 1) (second nextCoord))) ; Up
								((= D 1) (list (+ (first nextCoord) 1) (second nextCoord))) ; Down
								((= D 2) (list (first nextCoord) (- (second nextCoord) 1))) ; Left
								((= D 3) (list (first nextCoord) (+ (second nextCoord) 1))) ; Right
								(t NIL)
							))
						(nextNextSquare (get-square S (first nextNextCoord) (second nextNextCoord)))
					)
					(cond 
						((or (isBlank nextNextSquare) (isStar nextNextSquare)) 
							(let* (
									(result 
										(cond 
											((isBlank nextNextSquare) (set-square S (first nextNextCoord) (second nextNextCoord) 2))
											(t (set-square S (first nextNextCoord) (second nextNextCoord) 5))))
									(result 
										(cond 
											((isBox nextSquare) (set-square result (first nextCoord) (second nextCoord) 3))
											(t (set-square result (first nextCoord) (second nextCoord) 6))))
									(result 
										(cond 
											((isKeeper currSquare) (set-square result keeperRow keeperCol 0))
											((isKeeperStar currSquare) (set-square result keeperRow keeperCol 4))
											(t NIL))))
								result))
						(t NIL))))
						
			; else, NIL
			(t NIL))
	)
)

;---------------------------------------------------------------------------;
; EXERCISE: Takes state s as argument, returns list of all states that can
; 					be reached from the given state in one move.
;---------------------------------------------------------------------------;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 0) (try-move s 1) (try-move s 2) (try-move s 3)))
	 )
    (cleanUpList result);end
   );end let
  );

;---------------------------------------------------------------------------;
; EXERCISE: Computes the trivial admissible heuristic
;---------------------------------------------------------------------------;
(defun h0 (s)
	0
)

;---------------------------------------------------------------------------;
; Helper function for h1: Counts number of misplaced boxes in a row
;---------------------------------------------------------------------------;
(defun misplaced-boxes-in-row (L) 
	(cond 
		((null L) 0)
		((isBox (first L)) (+ (misplaced-boxes-in-row (rest L)) 1))
		(t (misplaced-boxes-in-row (rest L)))
	)
)

;---------------------------------------------------------------------------;
; EXERCISE: Heuristic function to count the number of misplaced boxes in s.
; 	h1 is admissible because it does not overestimate the cost to reach the 
; 	goal state (it is an underestimation since it does not consider
; 	the cost of moving the keeper to a goal as well)
;---------------------------------------------------------------------------;
(defun h1 (s)
	(cond 
		((null s) 0)
		(t (+ (misplaced-boxes-in-row (first s)) (h1 (rest s))))
	)
)

;---------------------------------------------------------------------------;
; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; Sokoban problem is essentially the minimum-cost perfect bipartite
; matching problem. 
; Let Cost of edge (C_e) = manhattan distance (L1 norm), where each edge
;						   is incident to a box-goal pair
;					h(n) = sum of min(C_e)
;
; At any state, we compute a C_e by computing the manhattan distance from 
; a box to the closest unoccupied goal. We do so in a greedy manner
; to approximate the min-cost perfect bipartite matching solution since 
; computing the real solution is expensive - We assign each box to 
; a goal that gives the minimum manhattan dist and then remove the used
; goal from the available choices for the remaining boxes. 
;
; This heuristic is admissible because it underestimates the cost of 
; reaching the goal from a given state - the closest goal to a box may
; not alwyas be the optimum final location for it, and we don't take into 
; account having to move boxes between different goals.
;---------------------------------------------------------------------------;

; Helper function for get-stuff
(defun get-stuff-in-row (L v currRow currCol)
	(cond 
		((null L) NIL)
		((= (first L) v) 
			(cons (list currRow currCol) (get-stuff-in-row (rest L) v currRow (+ currCol 1))))
		(t (get-stuff-in-row (rest L) v currRow (+ currCol 1)))
	)
)

; Helper function, gets the coordinates of all v in current state S
(defun get-stuff (S v currRow)
	(cond 
		((null S) NIL)
		(t (append (get-stuff-in-row (first S) v currRow 0) (get-stuff (rest S) v (+ currRow 1))))
	)
)

; Helper function to compute |val|
(defun abs (val)
	(cond 
		((< val 0) (- 0 val))
		(t val)
	)
)

; Helper function, computes L1 norm between 2 coordinates
(defun l1-norm (start end)
	(+ (abs (- (first start) (first end))) (abs (- (second start) (second end))))
)

; Helper function, computes all L1 norms of a given box
(defun all-norms (boxLoc goals)
	(cond 
		((null goals) NIL)
		(t (cons (l1-norm boxLoc (first goals)) (all-norms boxLoc (rest goals))))
	)
)

; Helper function, sums the minimum L1 norms for all boxes
(defun sum-norms (L)
	(cond 
		((null L) 0)
		(t (+ (first L) (sum-norms (rest L))))
	)
)

; Returns (min(norms), index of min value)
(defun current-min-row (norms curr idx)
	(cond 
		((null norms) (list (first curr) (second curr)))
		((< (first norms) (first curr)) 
			(current-min-row (rest norms) (list (first norms) idx) (+ idx 1)))
		(t (current-min-row (rest norms) curr (+ idx 1)))
	)
)

; Returns list of ((val1, c1), (val2, c2), ...) where vali is min of row i, indexed at col ci
(defun row-mins (matrix)
	(cond 
		((null matrix) NIL)
		(t (let ((current (current-min-row (first matrix) (list (first (first matrix)) 0) 0)))
				(cons (list (first current) (second current)) (row-mins (rest matrix)))))
	)
)

; L is 2d list (val, idx), returns tuple (val, row, col) corresponding to tuple with smallest 
; val in L 
(defun min-tuple (L curr idx)
	(cond 
		((null L) curr)
		((< (first curr) (first (first L))) 
			(min-tuple (rest L) (list (first (first L)) idx (second (first L))) (+ idx 1)))
		(t (min-tuple (rest L) curr (+ idx 1)))
	)
)

; Remove k'th element in L
(defun remove-kth (L k) 
	(cond 
		((null L) NIL)
		((= k 0) (rest L))
		(t (cons (first L) (remove-kth (rest L) (- k 1))))
	)
)

; Returns list of (current_min, row, col)
(defun current-min (matrix)
	(cond 
		((null matrix) NIL)
		(t (let* ((rowMins (row-mins matrix))
				(temp (min-tuple rowMins (list (first (first rowMins)) 0 (second (first rowMins))) 0))
				(val (first temp))
				(row (second temp))
				(col (third temp)))

				(list val row col)))
	)
)

; Takes matrix, r, c and returns matrix with row r removed, column c removed.
(defun clean-matrix (matrix r c)
	(cond
		((null matrix) NIL)
		((= r 0) (clean-matrix (rest matrix) -1 c))
		(t (cons (remove-kth (first matrix) c)
			(clean-matrix (rest matrix) (- r 1) c)))
	)
)

; Returns minimum L1-norms for remaining boxes (C_1, C_2, ...)
(defun get-mins (matrix)
	(cond 
		((null matrix) NIL)
		(t (let ((curr-min (current-min matrix)))
			(cons (first curr-min) 
				(get-mins (clean-matrix matrix (second curr-min) (third curr-min))))))
	)
)

; Generates a matrix that has the following:
;	Rows - Remaining boxes, Cols - Available goals
;	get-mins() systematically reduces this matrix to retrieve its output
(defun get-matrix (stuff goals)
	(cond 
		((null stuff) NIL)
		(t (cons (all-norms (first stuff) goals) (get-matrix (rest stuff) goals)))
	)
)

(defun h704451679 (s)
	(let* (
		(goals (append (get-stuff s 4 0)))
		(stuff (append (get-stuff s 2 0)))
		(keeperPos (getKeeperPosition s 0))
		(keeperNorms (all-norms (list (second keeperPos) (first keeperPos)) stuff))
		(keeperCost (sum-norms keeperNorms))
		(matrix (get-matrix stuff goals)) ; rows - stuff, cols - goals
		(l1norms (get-mins matrix))		
		(boxCost (sum-norms l1norms)))
		
		(+ boxCost keeperCost))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59) - slower? h1 - 6.8s
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?) - 51
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?) - 41
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?) - 78
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?) - 26
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
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
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

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
