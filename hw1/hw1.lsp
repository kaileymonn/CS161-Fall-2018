;-------------------------------------------------------------------------------------;
; 1) TREE-CONTAINS
; Takes two arguments N and checks whether number N appears in the ordered tree TREE.
; Base cases: If empty tree, return NIL
;             If tree only contains 1 number, compare number with N
; Catch-all: Compare N with m. If N < m, recurse on L
;                              If N > m, recurse on R
;                              If N = m, return t
;-------------------------------------------------------------------------------------;
(defun TREE-CONTAINS (N TREE)
    (cond
        ((null TREE) NIL)
        ((numberp TREE) (= N TREE))
        ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
        ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
        (t (= N (second TREE)))
    )
)

;-------------------------------------------------------------------------------------;
; 2) TREE-MIN
; Takes one argument TREE and returns the minimum number appearing in TREE.
; Base cases: If empty tree, return NIL
; Catch-all: Recurse on L until L is a single number
;-------------------------------------------------------------------------------------;
(defun TREE-MIN (TREE)
    (cond
        ((null TREE) NIL)
        ((numberp (first TREE)) (first TREE))
        (t (TREE-MIN (first TREE)))
    )
)

;-------------------------------------------------------------------------------------;
; 3) TREE-ORDER
; Takes one argument TREE, returns pre-ordered list of numbers appearing in TREE.
; Base cases: If empty tree, return NIL
;             If tree only has 1 number, return [number]
; Catch-all: Recursively join subtrees pre-order
;-------------------------------------------------------------------------------------;
(defun TREE-ORDER (TREE)
    (cond
        ((null TREE) NIL)
        ((numberp TREE) (list TREE))
        (t (append (cons (second TREE) (TREE-ORDER (first TREE)))
                    (TREE-ORDER (third TREE))))
    )
)

;-------------------------------------------------------------------------------------;
; 4) SUB-LIST
; Takes a list L and 2 non-negative integers START and LEN, returns the sub-list of L 
; starting at position START and having length LEN.
; Base cases: If L is empty, return NIL
;             If len = 0, return NIL
;             If START = 0, else recursively call SUB-LIST on rest L until START = 0
; Catch-all: Recursively construct output list using cons
;-------------------------------------------------------------------------------------;
(defun SUB-LIST (L START LEN)
    (cond
        ((null L) NIL)
        ((= LEN 0) NIL)
        ((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
        (t (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
    )
)

;-------------------------------------------------------------------------------------;
; 5) SPLIT-LIST
; Takes a list L, returns a list of two lists L1 and L2, in that order, such that
;       - L is the result of appending L1 and L2
;       - Length of L1 minus length of L2 is 0 or 1
; Base cases: If L is empty, return NIL
;             If (first L) is an atom, convert it to a list
;             If len(first L) - len(rest L) > 0, return list (first L) (rest L)
; Catch-all: Recursively move (second L) into the list (first L)
; Note: Using SUB-LIST would be easier but I just wanted to try doing it without
;-------------------------------------------------------------------------------------;
(defun SPLIT-LIST (L)
    (cond
        ((null L) NIL)
        ((atom (first L)) (SPLIT-LIST (cons (list (first L)) (rest L))))
        ((>= (- (length (first L)) (length (rest L))) 0) (list (first L) (rest L)))
        (t (SPLIT-LIST (cons (append (first L) (list (second L))) (cddr L))))
    )
)

;-------------------------------------------------------------------------------------;
; 6) BTREE-HEIGHT
; Takes a binary tree TREE and returns the height of TREE.
; Base cases: If TREE is empty, return 0
;             If TREE is an atom, return 0
;             If len(TREE) = 1, return 1
; Catch-all: Recursively add 1 to MAX((BTREE-HEIGHT L) (BTREE-HEIGHT R))
;-------------------------------------------------------------------------------------;
(defun BTREE-HEIGHT (TREE)
    (cond
        ((null TREE) 0)
        ((atom TREE) 0)
        ((= (length TREE) 1) 1)
        (t
            (let ((L (BTREE-HEIGHT (first TREE))) (R (BTREE-HEIGHT (second TREE))))
                (if (> L R) (+ 1 L) (+ 1 R))
            )
        )
    )
)

;-------------------------------------------------------------------------------------;
; 7) LIST2BTREE
; Takes non-empty list of atoms LEAVES and returns a binary tree such that
;       - The tree leaves are the elements of leaves
;       - For any internal (non-leaf) node in the tree, the number of leaves in 
;         its left branch minus the number of leaves in its right branch is 0 or 1
; Base cases: If len(LEAVES) = 1, return (first LEAVES)
;             If len(LEAVES) = 2, return LEAVES
; Catch-all: Recursively return 
;            [LIST2BTREE (first (SPLIT-LIST LEAVES)), (second (SPLIT-LIST LEAVES))]
;-------------------------------------------------------------------------------------;
(defun LIST2BTREE (LEAVES)
    (cond 
        ((= (length LEAVES) 1) (first LEAVES))
        ((= (length LEAVES) 2) LEAVES)
        (t (list 
            (LIST2BTREE (first (SPLIT-LIST LEAVES)))
            (LIST2BTREE (second (SPLIT-LIST LEAVES)))
        ))
    )
)

;-------------------------------------------------------------------------------------;
; 8) BTREE2LIST
; Takes a binary tree TREE as input, returns a list of atoms.
; Assume TREE follows the constraints:
;       - As input is a binary tree, each node has at most 2 children
;       - This function is the inverse of LIST2BTREE. 
; Base cases: If TREE is empty, return NIL
;             If TREE is an atom, return (TREE)
; Catch-all: Recursively return (append (BTREE2LIST l) (BTREE2LIST R))
;-------------------------------------------------------------------------------------;
(defun BTREE2LIST (TREE)
    (cond
        ((null TREE) NIL)
        ((atom TREE) (list TREE))
        (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
    )
)

;-------------------------------------------------------------------------------------;
; 9) IS-SAME
; Takes 2 LISP expressions E1 and E2 whose atoms are all numbers, and checks whether
; the expressions are equal. Can only use '=' to test equality.
; Base cases: If E1 or E2 are null, return NIL
;             If E1 and E2 are atoms, return (= E1 E2)
;             Elif E1 is atom but E2 is not (or vice versa), return NIL
; Catch-all: Recursively return 
;            (and (IS-SAME (first E1) (first E2)) 
;                 (IS-SAME (rest E1) (rest E2)))
;-------------------------------------------------------------------------------------;
(defun IS-SAME (E1 E2)
    (cond 
        ((null E1) (null E2))
        ((atom E1)
            (if (atom E2) (= E1 E2) NIL)
        )
        ((atom E2)
            (if (not (atom E1)) NIL)
        )
        (t (and 
            (IS-SAME (first E1) (first E2))
            (IS-SAME (rest E1) (rest E2))
        ))
    )
)



