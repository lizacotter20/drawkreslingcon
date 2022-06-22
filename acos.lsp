(defun acos (x)
	(cond
	((= x 0.0) (/ pi 2.0))
	((and (> x 0.0) (<= x 1.0))
	(atan (/ (sqrt (- 1.0 (* x x))) x))
	)
	((and (>= x -1.0) (< x 0.0))
	(+ pi (atan (/ (sqrt (- 1.0 (* x x))) x)))
	)
	(T nil)
	)
)