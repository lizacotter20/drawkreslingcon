(defun drawkreslingpolycon (H H0 n a b Npt crease_type chir hole diameter layers / M H0sqr Hsqr param rsmall Rlarge rssqr Rlsqr phi0 c v beta cosNMO NMO OMP O P apothem p0 toptabwidth j_a U W newP apothemb bottomtabwidth j_b Qu Ru Q R halfwindowside bottomleft topright ptList set1 
						biggroup PMY Mmove ONM NrNY Nrot newNpt i newNpt2 newNptt endtabwidth j_e tabN tabO origin firstt p00 mid) 

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
	(setq cosNMO (/ (- (+ (expt a 2) (expt v 2)) (expt c 2)) (* (* 2 a) v)))
	(setq NMO (acos cosNMO))
	(setq OMP (acos (/ (- (+ (expt c 2) (expt v 2)) (expt b 2)) (* (* 2 c) v))))

	;other two points in the panel
	(setq O (list (- (car M) (* v cosNMO)) (- (cadr M) (* v (sin NMO)))))
	(setq P (list (- (car M) (* c (cos (+ NMO OMP)))) (- (cadr M) (* c (sin (+ NMO OMP))))))

	;find the center of the small polygon
	(setq apothem (/ a (* 2 (tan param))))
	(setq p0 (list (+ (car Npt) (* 0.5 a)) (+ (cadr Npt) apothem)))

	;two points for the top tab
	(setq toptabwidth (* (/ 1.0 3.0) apothem)) 
	(setq j_a (/ toptabwidth (tan (/ (* param (- n 2)) 2))))
	(setq U (list (+ (car Npt) j_a) (+ (cadr Npt) toptabwidth)))
	(setq W (list (- (car M) j_a) (+ (cadr M) toptabwidth)))

	;set up new coordinate system with OP as the x-axis for drawing the tab
	(setq newP (list (- (car P) (car O)) (- (cadr P) (cadr O))))
	(command "_ucs" O newP "")
	(command "_ucs" "NA" "S" "b")
	(setq apothemb (/ b (* 2 (tan param))))
	(setq bottomtabwidth (* (/ 1.0 3.0) apothemb)) 
	(setq j_b (/ bottomtabwidth (tan (/ (* param (- n 2)) 2))))
	;two points for the tab
	(setq Qu (list j_b (* -1 bottomtabwidth)))
	(setq Ru (list (- b j_b) (* -1 bottomtabwidth)))
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
			;draw the top tab
			(command "_pline" Npt U W M *Cancel*)
			(command "_layer" "_n" "outline" "")
			(command "_layer" "_color" 4 "outline" "")
			(command "_change" (entlast) "" "_p" "_la" "outline" "")
			(command "_pline" P R Q O *Cancel*)
			(command "_change" (entlast) "" "_p" "_la" "outline" "")
			;draw the creases
			(command "_pline" Npt M O P M *Cancel*)
			(command "_layer" "_n" "creases" "")
			(command "_layer" "_color" 3 "creases" "")
			(command "_change" (entlast) "" "_p" "_la" "creases" "")
			;make a group out of the panel and tab
			(setq ptList (list P R Q O P M W U Npt M O))
			(setq set1 (ssget "F" ptList))
			(command "_.group" "c" "panel_and_tab" "panel and tab" set1 "")
		)
		;draw one side panel and one tab
		(command "_pline" P R Q O P M W U Npt M O *Cancel*)
	)

	;make group for everything
	(setq biggroup (ssadd))

	(setq PMY (- (/ pi 2) (+ NMO OMP)))
	(setq Mmove (list (+ (car O) (* c (sin PMY))) (+ (cadr O) (* c (cos PMY)))))
	(setq ONM (acos (/ (- (+ (expt a 2) (expt c 2)) (expt v 2)) (* (* 2 a) c))))
	(setq NrNY (- (* 2 pi) (+ (/ pi 2) (+ (+ ONM NMO) OMP)))) 
	(setq Nrot (list (- (car Npt) (* a (sin NrNY))) (+ (cadr Npt) (* a (cos NrNY)))))
	(setq newNpt (list (- (car Npt) (car Nrot)) (- (cadr Npt) (cadr Nrot))))

	;rotate panel and tab
	(setq i 0)
	(repeat (- n 1)
		(if (= i 0)
			(progn
				;move the tab then rotate it into the correct position
				(command "move" (entlast) "" P O "")
				(command "rotate" (entlast) "" O "R" O Mmove Npt)
				(ssadd (entlast) biggroup)
				(command "_.group" "c" "first_rot" "first rotation" biggroup "")
				;make a new user frame for the next rotation
				(command "_ucs" Nrot newNpt "")
				(setq i (+ i 1))
			)
			(progn
				(command "copy" (entlast) "" P O "")
				(command "rotate" (entlast) "" O "R" O Mmove Npt)
				(ssadd (entlast) biggroup)
				(command "_ucs" Nrot newNpt "")
				;if this is our last time through
				(if (= i (- n 2))
					(progn
						;set up new coordinate system for drawing the end tab
						(setq newNpt2 (list (- (car Npt) (car O)) (- (cadr Npt) (cadr O))))
						(command "_ucs" O newNpt2 "")
						(setq distNO (expt (+ (expt (- (cadr Npt) (cadr O)) 2) (expt (- (car Npt) (car O)) 2)) 0.5))
						(setq newNptt (list distNO 0))
						;another tabwidth
						(setq endtabwidth (/ (+ bottomtabwidth toptabwidth) 2))
						(setq j_e (/ endtabwidth (tan (/ (* param (- n 2)) 2))))
						;two points for the tab
						(setq tabN (list (- (car newNptt) j_e) endtabwidth))
						(setq tabO (list j_e endtabwidth))
						;draw the end tab
						(setq origin (list 0 0))
						(command "_pline" newNptt tabN tabO origin *Cancel*)
						(if (= layers "1")
							;add the tab to the outline
							(command "_change" (entlast) "" "_p" "_la" "outline" "")
						)
						(ssadd (entlast) biggroup)
						(command "_line" newNptt origin *Cancel*)
						(if (= layers "1")
							;last crease
							(command "_change" (entlast) "" "_p" "_la" "creases" "")
						)
						(ssadd (entlast) biggroup)
						(command "_ucs" "W")
					)
				)
				(setq i (+ i 1))
			)
		)	
	)

	;make polygon tabs
	(command "_polygon" n "E" P O)
	(if (= layers "1")
		;add the tab to the outline
		(command "_change" (entlast) "" "_p" "_la" "outline" "")
	)
	(ssadd (entlast) biggroup)
	(command "_polygon" n "E" Npt M) 
	(ssadd (entlast) biggroup)

	(if (= layers "1")
		(progn
			;add the polygon to the outline
			(command "_change" (entlast) "" "_p" "_la" "outline" "")
			;delete the crease segments of the polygons
			(command "_break" O P)
			(command "_break" Npt M)
			;draw the last segment of the outline
			(command "_line" M P *Cancel*)
			(command "_change" (entlast) "" "_p" "_la" "outline" "")
			(ssadd (entlast) biggroup)
			;draw the creases
			(command "_pline" Npt M O P *Cancel*)
			(command "_change" (entlast) "" "_p" "_la" "creases" "")
			(ssadd (entlast) biggroup)
			;ungroup the panel and tab group
			(command "_ungroup" "NA" "panel_and_tab")
		)
		;finish first panel
		(progn
			(command "_pline" Npt M O P M *Cancel*)
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
			(command "_ucs" "R" "b")
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