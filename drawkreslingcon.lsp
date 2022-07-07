(prompt "\nType drawkreslingcon to run.....")
 
(defun c:drawkreslingcon ( / dcl_id flag Hstr H0str nstr astr bstr xstr ystr Npt crease_type hole diameterstr layers H H0 n a b diameter)
     (print "c0ne baby")

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
          (if (= Hstr nil)
               (action_tile "H" "(setq Hstr $value)")
               (set_tile "H" Hstr)
          )
          (if (= H0str nil)
               (action_tile "H0" "(setq H0str $value)")
               (set_tile "H0" H0str)
          )
          (if (= nstr nil)
               (action_tile "n" "(setq nstr $value)")
               (set_tile "n" nstr)
          )
          (if (= astr nil)
               (action_tile "a" "(setq astr $value)")
               (set_tile "a" astr)
          )
          (if (= bstr nil)
               (action_tile "b" "(setq bstr $value)")
               (set_tile "b" bstr)
          )
          (if (= xstr nil)
               (progn
                    (action_tile "x" "(setq xstr $value)")
                    (setq xstr "0")
               )
               (set_tile "x" xstr)
          )
          (if (= ystr nil)
               (progn
                    (action_tile "y" "(setq ystr $value)")
                    (setq ystr "0")
               )
               (set_tile "y" ystr)
          )   

          ;update string values with the values in the boxes, if they've been changed
          (action_tile "H" "(setq Hstr $value)")
          (action_tile "H0" "(setq H0str $value)") 
          (action_tile "n" "(setq nstr $value)")
          (action_tile "a" "(setq astr $value)")
          (action_tile "b" "(setq bstr $value)")
          (action_tile "x" "(setq xstr $value)")
          (action_tile "y" "(setq ystr $value)") 

          (if (= Npt nil)
               (setq Npt (list (distof (get_tile "x")) (distof (get_tile "y"))))
          )

          ;remember which radio button was chosen last time
          (cond
              ((= crease_type nil) (setq crease_type "m"))
              ((= crease_type "m") (print "mountain") (set_tile "mountain" "1"))
              ((= crease_type "v") (print "valley") (set_tile "valley" "1")) 
              ((= crease_type "p") (print "polygon") (set_tile "polygon" "1")) 
          )

          ;radio buttons
          (action_tile "mountain" "(setq crease_type \"m\")")
          (action_tile "valley" "(setq crease_type \"v\")")
          (action_tile "polygon" "(setq crease_type \"p\")")

          ;the diameter edit_box is only enabled when the hole toggle is turned on
          ;(mode_tile "diameter" 1)
          (if (= hole nil)
               (progn
                    (action_tile "hole" "(mode_tile \"diameter\" (- 1 (atoi $value))) (setq hole $value)")
                    (mode_tile "diameter" 1)
               )
               (progn
                    (set_tile "hole" hole)
                    (mode_tile "diameter" (- 1 (atoi hole)))
                    (set_tile "diameter" diameterstr)
               )
          )
          (action_tile "hole" "(mode_tile \"diameter\" (- 1 (atoi $value))) (setq hole $value)")
          (action_tile "diameter" "(setq diameterstr $value)") 
          
          ;remember whether the user previously had the layers option turned on
          (if (= layers nil)
               (action_tile "layers" "(setq layers $value)")
               (set_tile "layers" layers)
          )
          (action_tile "layers" "(setq layers $value)")

          (print "layers")
          (print layers)
          (print "hole")
          (print hole)

          ;in order for the user to be able to press ok, make sure the design constraints are not violated and that the parameter types are correct
          (action_tile "accept" "(checktypescon)")
          ;(action_tile "accept" "(done_dialog 1)")
          (print "hole2")

          ;set canceled to true if the dialog was canceled so we dont do unecessary calculations + drawings
          (action_tile "cancel" "(setq canceled T)")
          ;(action_tile "cancel" "(done_dialog 0)")

          ;flag to hide the dialog box is 5
          (action_tile "select_pt" "(done_dialog 5)")

          ;set the flag to whatever start_dialog pulls from done_dialog
          (setq flag (start_dialog))

          ;if the select point button was clicked 
          (if (= flag 5)
               ;get the point from the user
               (progn
                    (setq Npt (getpoint))
                    (setq xstr (rtos (car Npt)))
                    (setq ystr (rtos (cadr Npt)))
               )
          )
     )

     (unload_dialog dcl_id)
     """
     (print canceled)
     (print crease_type)
     
     (print (car Npt))
     (print (cadr Npt))
     (print (distof Hstr))
     (print (distof H0str))       
     (print (atoi nstr))
     (print (distof bstr)) 
     """
     (print "layers")
     (print layers)
     (print "hole")
     (print hole)
     
     (print "diameter")
     (print diameterstr)

     (print xstr)
     (print ystr)

     (if canceled
          (setq canceled nil)
          (progn
               (print "not canceled ig")
               ;convert string values to reals or ints
               (setq H (distof Hstr))
               (setq H0 (distof H0str))
               (setq n (atoi nstr))
               (setq a (distof astr))
               (setq b (distof bstr))
               (if (= hole "1")
               	(setq diameter (distof diameterstr))
               	(setq diameter 0)
               )
               ;get the latest point from the box
               (setq Npt (list (distof xstr) (distof ystr)))
               ;call drawkresling
               (drawkreslingmountaincon H H0 n a b Npt crease_type hole diameter layers)
          )
     )
     (princ)
)
