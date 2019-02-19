(setq L 
	'(
		"f1/sat_f1.cnf"
		"f2/sat_f2.cnf"
		"f3/sat_f3.cnf"
		"f4/sat_f4.cnf"
		"f5/sat_f5.cnf"
	)
)


(defun test-h (l)
	(cond ((null l) t)
        (t (format t (car l)) (time (print (solve-cnf (car l)))) (format t "----------~%") (test-h (cdr l)))))

(defun test ()
	(load "parse_cnf.lsp")
	(load "hw4.lsp")
	(test-h L))