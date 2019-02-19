; Helper function that takes sign(True:1 or False:-1) and curr_assignments as inputs, 
;   returns new list of assignments with next unassigned variable set to sign
(defun addAssignment (sign curr_assignments)
    (append curr_assignments (list (* (+ (length curr_assignments) 1) sign)))
)

; Helper function to check if literal is satisfiable
;   if literal is unassigned, return t
;   elif literal is in curr_assignments, return t
;   else, negation of literal appears in curr_assignments so return NIL
(defun literalIsValid? (literal curr_assignments)
    (cond
        ((null curr_assignments) t) ; literal is unassigned
        ((= (first curr_assignments) literal) t) ; literal is satisfied
        ((= (+ (first curr_assignments) literal) 0) NIL) ; negation of literal
        (t (literalIsValid? literal (rest curr_assignments)))
    )
)


; Helper function that returns t if curr_assignments do not violate clause's constraints,
;   NIL otherwise
(defun clauseIsValid? (clause curr_assignments)
    (cond 
        ((null clause) NIL) ; Recursed to the end of clause, NIL since we want disjunction
        (t  (cond ; if first literal is True, don't have to check the rest of the clause
                ((literalIsValid? (first clause) curr_assignments) t) 
                (t (clauseIsValid? (rest clause) curr_assignments))
            )
        )
    )
)

; Helper function that returns t if curr_assignments do not violate delta's constraints, 
;   NIL otherwise
(defun sentenceIsValid? (delta curr_assignments)
    (cond
        ((null delta) t) ; Recursed to the end of delta
        (t 
            (and (clauseIsValid? (first delta) curr_assignments) (sentenceIsValid? (rest delta) curr_assignments))
        )
    )
)

; Performs backtrack search to determine if assignments satisfy the CNF.
; assignments is a list of variable assignments, where 0 <= len(assignments) <= n
;   if assignments violate delta, return NIL
;   elif len(assignments) == n and assignments satisfy CNF, return assignments
;   else, assign a value to next unassigned variable and recursively backtrackSearch
(defun backtrackSearch (n delta assignments)
    (cond
        ((not (sentenceIsValid? delta assignments)) NIL)
        ((= (length assignments) n) assignments)
        (t 
            (or (backtrackSearch n delta (addAssignment 1 assignments)) 
                (backtrackSearch n delta (addAssignment -1 assignments))
            )
        )
    )
)

; CNF sat solver that utilizes backtrack search
(defun sat? (n delta) 
    (backtrackSearch n delta '())
)