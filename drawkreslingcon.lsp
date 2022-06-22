(defun c:drawkreslingcon ()

;get relevant parameters from user
(setq H (getreal "\nEnter deployed height:"))
(setq H0 (getreal "\nEnter folded height:"))
(setq n (getint "\nEnter number of polygon edges:"))

;check the design constraint
(if (> (abs (- (expt H 2) (expt H0 2))) (expt (cot (/ pi n)) 2))
    (progn
       (print "Your parameters have failed to meet the design constraint |H^2 - H0^2| <= cot^2(pi/n)")
       (print "Please adjust your parameters and try again")
       (exit) 
    )
    (print '())
)

;continue getting relevant parameters from user
(setq a (getreal "\nEnter length of the edges of the top polygon:"))
(setq b (getreal "\nEnter length of the edges of the bottom polygon:"))
(setq Npt (getpoint "\nPick starting point for first polygon edge:"))

;ask if the user wants the outline and creases in different layers
(setq bad_res T)
(setq layers nil)
(while bad_res
	(setq response (getstring "\nWould you like the outline and creases to be in different layers (for laser cutting)? <Y/n>"))
	(cond 
		((= response "Y") (setq layers T) (setq bad_res nil)) 
		((= response "y") (setq layers T) (setq bad_res nil))  
		((= response "N") (setq layers nil) (setq bad_res nil))  
		((= response "n") (setq layers nil) (setq bad_res nil)) 
		(T (print "Invalid response, please type Y, y, N, or n")) 
	)
)
(print "heyyyyyyy")

;the second point is the desired distance from the user selected first point
(setq M (list (+ (car Npt) a) (cadr Npt))) 
(print M)

;useful terms to clean up the calculations
(setq H0sqr (expt H0 2))
(print H0sqr)
(setq Hsqr (expt H 2))
(print Hsqr)
(print n)
(print Npt)
(setq param (/ pi n))
(print param)
(print "useful terms")

;radii of the two polygons
(setq rsmall (/ a (* 2 (sin param))))
(setq Rlarge (/ b (* 2 (sin param))))
(print "radii")

;more useful terms to clean up the calculations
(setq rssqr (expt rsmall 2))
(setq Rlsqr (expt Rlarge 2))
(print "more useful terms")

;do the calculations for the conical Kresling
(setq phi (- (/ pi 2) param))
(setq c (expt (- (+ Hsqr (+ rssqr Rlsqr)) (* (* (* 2 rsmall) Rlarge) (cos phi))) 0.5))
(setq d (expt (- (+ Hsqr (+ rssqr Rlsqr)) (* (* (* 2 rsmall) Rlarge) (cos (+ phi (* 2 param))))) 0.5))
(setq beta (acos (/ (- (+ (expt b 2) (expt c 2)) (expt d 2)) (* (* 2 b) c))))
(print "did some calculations")

;angles in the quadrilateral for finding xy coords of the other 2 points in the panel
(setq cosNMO (/ (- (+ (expt a 2) (expt b 2)) (expt c 2)) (* (* 2 a) b)))
(setq NMO (acos cosNMO))
(setq OMP (acos (/ (- (+ (expt c 2) (expt d 2)) (expt b 2)) (* (* 2 c) d))))
(print "~angles~")

;other two points in the panel
(setq O (list (- (car M) (* d cosNMO)) (- (cadr M) (* d (sin NMO)))))
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
(setq tabwidth (* 0.5 apothem)) 
(setq OPR (/ (* (- n 2) param) 2))
(setq e (* tabwidth (csc OPR)))
(setq POM (- pi (+ OMP beta)))
(setq QOYm (- (/ pi 2) (- (+ POM OPR) NMO)))
(setq MPXp (+ NMO OMP))
(setq RPYm (- (* 2 pi) (+ (+ (+ MPXp beta) OPR) (/ pi 2))))
(print "more geometric calcs")

;two points for the tab
(setq Q (list (+ (car O) (* e (sin QOYm))) (- (cadr O) (* e (cos QOYm)))))
(setq R (list (- (car P) (* e (sin RPYm))) (- (cadr P) (* e (cos RPYm)))))
(print "~tab points~")



(if layers
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
		(setq bottomright (list (car P) (cadr Q)))
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
(command "_polygon" 5 "E" P O)
(if layers
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
	(command "_pline" M O N M P *Cancel*)
)
)