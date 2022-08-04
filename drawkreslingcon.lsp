(prompt "\nType drawkreslingcon to run.....")
 
(defun c:drawkreslingcon ( / dcl_id flag Npt H H0 n a b diameter)

	;flag is for discerning whether the dialog was canceled or hidden for starting point selection
	(setq flag 5)

	;load the dialog 
	(setq dcl_id (load_dialog "drawkreslingcon.dcl"))

	;while the flag is not accept or cancel
	(while (> flag 2)
		;make a new dialog
		(if (not (new_dialog "drawkreslingcon" dcl_id))
			(exit)
		)
		
		;set the values of the edit_boxes to their previous values, if there is one
		(if (= Hstrcon nil)
			(action_tile "H" "(setq Hstrcon $value)")
			(set_tile "H" Hstrcon)
		)
		(if (= H0strcon nil)
			(action_tile "H0" "(setq H0strcon $value)")
			(set_tile "H0" H0strcon)
		)
		(if (= nstrcon nil)
			(action_tile "n" "(setq nstrcon $value)")
			(set_tile "n" nstrcon)
		)
		(if (= astrcon nil)
			(action_tile "a" "(setq astrcon $value)")
			(set_tile "a" astrcon)
		)
		(if (= bstrcon nil)
			(action_tile "b" "(setq bstrcon $value)")
			(set_tile "b" bstrcon)
		)
		(if (= xstrcon nil)
			(progn
				(action_tile "x" "(setq xstrcon $value)")
				(setq xstrcon "0")
			)
			(set_tile "x" xstrcon)
		)
		(if (= ystrcon nil)
			(progn
				(action_tile "y" "(setq ystrcon $value)")
				(setq ystrcon "0")
			)
			(set_tile "y" ystrcon)
		)   

		;update string values with the values in the boxes, if they've been changed
		(action_tile "H" "(setq Hstrcon $value)")
		(action_tile "H0" "(setq H0strcon $value)") 
		(action_tile "n" "(setq nstrcon $value)")
		(action_tile "a" "(setq astrcon $value)")
		(action_tile "b" "(setq bstrcon $value)")
		(action_tile "x" "(setq xstrcon $value)")
		(action_tile "y" "(setq ystrcon $value)") 

		;set the insertion point to what is in the x and y boxes
		(setq Npt (list (distof (get_tile "x")) (distof (get_tile "y"))))

		;remember which radio button was chosen last time
		(cond
		    ((= crease_type_con nil) (setq crease_type_con "m"))
		    ((= crease_type_con "m") (print "mountain") (set_tile "mountain" "1"))
		    ((= crease_type_con "v") (print "valley") (set_tile "valley" "1")) 
		    ((= crease_type_con "p") (print "polygon") (set_tile "polygon" "1")) 
		)

		;remember which radio button was chosen last time
		(cond
		    ((= chir_con nil) (setq chir_con "cw"))
		    ((= chir_con "cw") (set_tile "cw" "1"))
		    ((= chir_con "ccw") (set_tile "ccw" "1"))
		)

		;radio buttons
		(action_tile "mountain" "(setq crease_type_con \"m\")")
		(action_tile "valley" "(setq crease_type_con \"v\")")
		(action_tile "polygon" "(setq crease_type_con \"p\")")
		(action_tile "cw" "(setq chir_con \"cw\")")
		(action_tile "ccw" "(setq chir_con \"ccw\")")

		;the diameter edit_box is only enabled when the hole toggle is turned on
		(if (= holecon nil)
			(progn
				(action_tile "hole" "(mode_tile \"diameter\" (- 1 (atoi $value))) (setq holecon $value)")
				(mode_tile "diameter" 1)
			)
			(progn
				(set_tile "hole" holecon)
				(mode_tile "diameter" (- 1 (atoi holecon)))
				(set_tile "diameter" diameterstrcon)
			)
		)
		(action_tile "hole" "(mode_tile \"diameter\" (- 1 (atoi $value))) (setq holecon $value)")
		(action_tile "diameter" "(setq diameterstrcon $value)") 
		
		;remember whether the user previously had the layerscon option turned on
		(if (= layerscon nil)
			(action_tile "layers" "(setq layerscon $value)")
			(set_tile "layers" layerscon)
		)
		(action_tile "layers" "(setq layerscon $value)")

		;in order for the user to be able to press ok, make sure the design constrconaints are not violated and that the parameter types are correct
		(action_tile "accept" "(checktypescon)")

		;set canceled to true if the dialog was canceled so we dont do unecessary calculations + drawings
		(action_tile "cancel" "(setq canceled T)")

		;flag to hide the dialog box is 5
		(action_tile "select_pt" "(done_dialog 5)")

		;set the flag to whatever start_dialog pulls from done_dialog
		(setq flag (start_dialog))

		;if the select point button was clicked 
		(if (= flag 5)
			;get the point from the user
			(progn
				(setq Npt (getpoint))
				(setq xstrcon (rtos (car Npt)))
				(setq ystrcon (rtos (cadr Npt)))
			)
		)
	)

	(unload_dialog dcl_id)
	
	;if the dialog was canceled, don't draw anything, otherwise call the appropriate routine to do calculations and drawing
	(if canceled
		(setq canceled nil)
		(progn
			;convert string values to reals or ints
			(setq H (distof Hstrcon))
			(setq H0 (distof H0strcon))
			(setq n (atoi nstrcon))
			(setq a (distof astrcon))
			(setq b (distof bstrcon))
			(if (= holecon "1")
				(setq diameter (distof diameterstrcon))
				(setq diameter 0)
			)
			;get the latest point from the box
			(setq Npt (list (distof xstrcon) (distof ystrcon)))
			;call appropriate drawing routine based on crease pattern type
			(cond
				((= crease_type_con "m") (drawkreslingmountaincon H H0 n a b Npt crease_type_con chir_con holecon diameter layerscon))
				((= crease_type_con "v") (drawkreslingvalleycon H H0 n a b Npt crease_type_con chir_con holecon diameter layerscon)) 
				((= crease_type_con "p") (drawkreslingpolycon H H0 n a b Npt crease_type_con chir_con holecon diameter layerscon))
			)
		)
	)
	(princ)
)
