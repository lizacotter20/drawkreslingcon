(defun drawkreslingvalleycon (H H0 n a b Npt crease_type chir hole diameter layers / M H0sqr Hsqr param rsmall Rlarge rssqr Rlsqr phi0 c v beta cosNMO NMO OMP O P apothem p0 newP apothemb tabwidth j Qu Ru Q R ptList set1 firstt p00) 

	(defun *error* (msg)
		(if (= msg "Function cancelled")
			(progn
				(print "Function was canceled, exploding groups and deleting the user frame")
				(command-s "_ucs" "NA" "D" "b")
				(command-s "_ungroup" "NA" "panel_and_tab" "")
				(command-s "_ungroup" "NA" "first_rot" "")
				(command-s "_ucs" "W")
			)
			(progn
				(print "Error thrown, exploding groups and deleting the user frame")
				(command-s "_ucs" "NA" "D" "b")
				(command-s "_ungroup" "NA" "panel_and_tab" "")
				(command-s "_ungroup" "NA" "first_rot" "")
				(command-s "_ucs" "W")
			)
		)
	)

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

	;angles in the quadrilateral for finding xy coords of the other 2 points in the panel
	(setq ONP (acos (/ (- (+ (expt v 2) (expt c 2)) (expt b 2)) (* (* 2 v) c))))
	(setq PNM (acos (/ (- (+ (expt a 2) (expt c 2)) (expt v 2)) (* (* 2 a) c))))
	(setq ONX (- pi (+ PNM ONP)))
	(setq PNY (- (/ pi 2) (+ ONX ONP)))

	;other two points in the panel
	(setq O (list (- (car Npt) (* v (cos ONX))) (- (cadr Npt) (* v (sin ONX)))))
	(setq P (list (- (car Npt) (* c (sin PNY))) (- (cadr Npt) (* c (cos PNY)))))

	;find the center of the small polygon
	(setq apothem (/ a (* 2 (tan param))))
	(setq p0 (list (+ (car Npt) (* 0.5 a)) (+ (cadr Npt) apothem)))

	;set up new coordinate system with OP as the x-axis for drawing the tab
	(setq newP (list (- (car P) (car O)) (- (cadr P) (cadr O))))
	(command "_ucs" O newP "")
	(command "_ucs" "NA" "S" "b")
	(setq apothemb (/ b (* 2 (tan param))))
	(setq tabwidth (* (/ 1.0 3.0) apothemb)) 
	(setq j (/ tabwidth (tan (/ (* param (- n 2)) 2))))
	;two points for the tab
	(setq Qu (list j (* -1 tabwidth)))
	(setq Ru (list (- b j) (* -1 tabwidth)))
	(setq Q (trans Qu 1 0))
	(setq R (trans Ru 1 0))
	(command "_ucs" "W")

	;zoom to drawing area
	(setq halfwindowside (+ (+ apothem (- (cadr Npt) (cadr R))) (* 4 Rlarge)))
	(setq bottomleft (list (- (car p0) halfwindowside) (- (cadr p0) halfwindowside)))
	(setq topright (list (+ (car p0) halfwindowside) (+ (cadr p0) halfwindowside)))
	(command "_zoom" bottomleft topright)

	;start drawing
	(if (= layers "1") 
		(progn
			;draw the outline
			(command "_pline" Npt O Q R P M *Cancel*)
			(command "_layer" "_n" "outline" "")
			(command "_layer" "_color" 4 "outline" "")
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		;draw the creases
			(command "_pline" M Npt P O *Cancel*)
			(command "_layer" "_n" "creases" "")
			(command "_layer" "_color" 3 "creases" "")
	 		(command "_change" (entlast) "" "_p" "_la" "creases" "")
	 		;make a group out of the panel and tab
			(setq ptList (list Npt P O Npt M P R Q O))
			(setq set1 (ssget "F" ptList))
			(command "_.group" "c" "panel_and_tab" "panel and tab" set1 "")
		)
		;draw one side panel and one tab
		(command "_pline"  Npt P O Npt M P R Q O *Cancel*)
	)

	;make group for everything
	(setq biggroup (ssadd))

	;rotate panel and tab
	(setq firstt 1)
	(repeat (- n 1)
		(if (= firstt 1)
			(progn
				(command "rotate" (entlast) "" p0 (/ 360.0 n) "")
				(ssadd (entlast) biggroup)
				(command "_.group" "c" "first_rot" "first rotation" biggroup "")
				(setq firstt 0)
			)
			(progn
				(command "rotate" (entlast) "" p0 "C" (/ 360.0 n))
				(ssadd (entlast) biggroup)
			)
		)	
	)

	;make polygon tab
	(command "_polygon" n "E" P O)
	(ssadd (entlast) biggroup) 

	(if (= layers "1")
		(progn
			;add the polygon to the outline
			(command "_change" (entlast) "" "_p" "_la" "outline" "")
			;delete the crease segment of the polygon
			(command "_break" O P)
			;draw the rest of the outline
			(command "_line" Npt O *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		(ssadd (entlast) biggroup)
	 		(command "_line" M P *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	 		(ssadd (entlast) biggroup)
	 		;draw the creases
			(command "_pline" M Npt P O *Cancel*)
	 		(command "_change" (entlast) "" "_p" "_la" "creases" "")
	 		(ssadd (entlast) biggroup)
	 		;ungroup the panel and tab group
	 		(command "_ungroup" "NA" "panel_and_tab")
		)
		;finish first panel
		(progn
			(command "_pline" O Npt M P Npt *Cancel*)
			(ssadd (entlast) biggroup)
		)
	)

	(if (= hole "1")
		(progn
			;draw the circle for the top tab
			(command "_circle" p0 (/ diameter 2))
			(if (= layers "1")
				(command "_change" (entlast) "" "_p" "_la" "outline" "")
			)
			(ssadd (entlast) biggroup)

			;find center of tab polygon
			(command "_ucs" "P")
			(setq p00 (list (/ b 2) (* -1 apothemb)))

			;draw the circle for the bottom tab
			(command "_circle" p00 (/ diameter 2))
			(if (= layers "1")
				(command "_change" (entlast) "" "_p" "_la" "outline" "")
			)
			(ssadd (entlast) biggroup)

			(command "_ucs" "W")
		)
	)

	;flip if ccw chirality
	(if (= chir "ccw")
		(progn
			(setq mid (list (+ (car Npt) (/ a 2)) (cadr Npt)))
			(command "mirror" biggroup "" p0 mid "Y")
		)
	)

	;delete the ucs
	(command "_ucs" "NA" "D" "b")
	;delete the group
	(command "_ungroup" "NA" "first_rot")
)