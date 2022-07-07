(defun drawkreslingmountaincon (H H0 n a b Npt crease_type hole diameter layers / M H0sqr Hsqr param rsmall Rlarge rssqr Rlsqr phi0 c v beta cosNMO NMO OMP O P apothem p0 tabwidth OPR e POM QOYm MPXp RPYm Q R topleft bottomright set1 firstt) 

	(print "still need to fix that weird thing")
	(print "hole")
	(print hole)
	(print "diameterr")
	(print diameter)

	;the second point is the desired distance from the user selected first point
	(setq M (list (+ (car Npt) a) (cadr Npt))) 

	;useful terms to clean up the calculations
	(setq H0sqr (expt H0 2))
	(setq Hsqr (expt H 2))
	(setq param (/ pi n))

	;radii of the two polygons
	(setq rsmall (/ a (* 2 (sin param))))
	(setq Rlarge (/ b (* 2 (sin param))))

	;more useful terms to clean up the calculations
	(setq rssqr (expt rsmall 2))
	(setq Rlsqr (expt Rlarge 2))

	;do the calculations for the Kresling
	(setq phi0 (- (acos (/ (- H0sqr Hsqr) (* (* (* 4 rsmall) Rlarge) (cos param)))) param)) ;phi0 is the twisting angle associated with the folded height (H0)
	(setq c (expt (- (+ H0sqr (+ rssqr Rlsqr)) (* (* (* 2 rsmall) Rlarge) (cos phi0))) 0.5))
	(setq v (expt (- (+ H0sqr (+ rssqr Rlsqr)) (* (* (* 2 rsmall) Rlarge) (cos (+ phi0 (* 2 param))))) 0.5))
	(setq beta (acos (/ (- (+ (expt b 2) (expt c 2)) (expt v 2)) (* (* 2 b) c))))
	(print "did some calculations")

	;angles in the quadrilateral for finding xy coords of the other 2 points in the panel
	(setq cosNMO (/ (- (+ (expt a 2) (expt v 2)) (expt c 2)) (* (* 2 a) v)))
	(setq NMO (acos cosNMO))
	(setq OMP (acos (/ (- (+ (expt c 2) (expt v 2)) (expt b 2)) (* (* 2 c) v))))
	(print "~angles~")

	;other two points in the panel
	(setq O (list (- (car M) (* v cosNMO)) (- (cadr M) (* v (sin NMO)))))
	(print O)
	(setq P (list (- (car M) (* c (cos (+ NMO OMP)))) (- (cadr M) (* c (sin (+ NMO OMP))))))
	(print P)
	(print "panel points")

	;find the center of the small polygon
	(setq apothem (/ a (* 2 (tan param))))
	(setq p0 (list (+ (car Npt) (* 0.5 a)) (+ (cadr Npt) apothem)))
	(print "center")

	;this is a lot of geometry... i wonder if i should make a new user coordinate system relative to OP to make this easier to read
	;calculations to find xy coords of the tab points
	"""
	(setq tabwidth (* 0.5 apothem)) 
	(setq OPR (/ (* (- n 2) param) 2))
	(setq e (* tabwidth (csc OPR)))
	(setq POM (- pi (+ OMP beta)))
	(setq QOYm (- (/ pi 2) (- (+ POM OPR) NMO)))
	(setq MPXp (+ NMO OMP))
	(setq RPYm (- (* 2 pi) (+ (+ (+ MPXp beta) OPR) (/ pi 2))))

	;two points for the tab
	(setq Q (list (+ (car O) (* e (sin QOYm))) (- (cadr O) (* e (cos QOYm)))))
	(setq R (list (- (car P) (* e (sin RPYm))) (- (cadr P) (* e (cos RPYm)))))
	"""
	(setq newP (list (- (car P) (car O)) (- (cadr P) (cadr O))))
	(command "_ucs" O newP "")
	(command "_ucs" "NA" "S" "b")
	(setq tabwidth (* 0.5 apothem)) 
	(setq j (/ tabwidth (tan (/ (* param (- n 2)) 2))))
	;two points for the tab
	(setq Q (list (+ (car O) j) (- (cadr O) tabwidth)))
	(setq R (list (- (car P) j) (- (cadr P) tabwidth)))
	(command "_ucs" "W")

	(if (= layers "1") 
		(progn
			;draw the outline
			(command "_pline" Npt O Q R P M *Cancel*)
			(command "_layer" "_n" "outline" "")
			(command "_layer" "_color" 4 "outline" "")
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		;draw the creases
			(command "_pline" Npt M O P *Cancel*)
			(command "_layer" "_n" "creases" "")
			(command "_layer" "_color" 3 "creases" "")
	 		(command "_change" (entlast) "" "_p" "_la" "creases" "")
	 		;select and group the panel and tab
			(setq topleft (list (car O) (cadr Npt)))
			(setq bottomright (list (car M) (cadr R)))
			(setq set1 (ssget "W" topleft bottomright))
			(command "_.group" "c" "*" "panel and tab" set1 "")
		)
		;draw one side panel and one tab
		(command "_pline" M Npt O P M O Q R P *Cancel*)
	)
	;(print "drew one panel and one tab")
	(setq firstt 1)
	(repeat (- n 1)
		(if (= firstt 1)
			(progn
				(command "rotate" (entlast) "" p0 (/ 360.0 n) "")
				(setq firstt 0)
				(print "first")
				(print firstt)
			)
			(progn
				(command "rotate" (entlast) "" p0 "C" (/ 360.0 n))
				(print firstt)
			)
		)	
	)
	;make polygon tab
	(command "_polygon" n "E" P O)
	(if (= layers "1")
		(progn
			;add the polygon to the outline
			(command "_change" (entlast) "" "_p" "_la" "outline" "")
			;delete the crease segment of the polygon
			(command "_break" O P)
			;draw the rest of the outline
			(command "_line" Npt O *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		(command "_line" M P *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		;draw the creases
			(command "_pline" Npt M O P *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "creases" "")
		)
		;draw one side panel and one tab
		(command "_pline" M O Npt M P *Cancel*)
	)

	(if (= hole "1")
		(progn
			;find radius of tab polygon
			;(setq r (/ b (* 2 (sin param))))
			;some terms to clean up the calculations
			;(setq k (/ (expt apothem 2) (- (* 2 apothem) b)))
			;(setq angle6 (atan (* (- (/ k r) (sin OPR)) (sec OPR))))
			;(setq yhole (* k (cos angle6)))
			;(setq xhole (expt (- (expt r 2) (expt yhole 2)) 0.5))

			;draw the circle for the top tab
			(command "_circle" p0 (/ diameter 2))
			(if (= layers "1")
				(command "_change" (entlast) "" "_p" "_la" "outline" "")
			)

			;find center of tab polygon
			(command "_ucs" "b")
			(setq apothemb (/ b (* 2 (tan param))))
			(setq p00 (list (/ b 2) (* -1 apothemb)))

			;draw the circle for the bottom tab
			(command "_circle" p00 (/ diameter 2))
			(if (= layers "1")
				(command "_change" (entlast) "" "_p" "_la" "outline" "")
			)
			(command "_ucs" "NA" "D" "b")
			(command "_ucs" "W")
		)
	)
) 