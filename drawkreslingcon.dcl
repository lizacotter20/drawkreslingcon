drawkreslingcon : dialog { label = "Draw Conical Kresling";

	// would love to add some color to this situation, primarily to make it easier to differentiate between sets of options, but also bc I think it would look nicer

	: boxed_column {
		label = "Geometry Parameters";
		key = "geometry";	
		: edit_box {
			label = "Enter deployed height, H:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "H";
		}

		: edit_box { 
			label = "Enter folded height, H0:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "H0";
		}

		: edit_box { 
			label = "Enter number of polygon edges, n:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "n";
		}

		: edit_box { 
			label = "Enter length of the top polygon edges, a:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "a";
		}

		: edit_box { 
			label = "Enter length of the bottom polygon edges, b:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "b";
		}
	}
	: boxed_column {label = "Insertion point";	
		: row {
			: text {
				label = "Enter a starting point for the first edge of the polygon:";
			}

			: edit_box {
				key = "x";
				label = "x";
				alignment = right;
				edit_limit = 10;
				edit_width = 10;
				value = 0;
			}

			: edit_box {
				key = "y";
				label = "y";
				alignment = right;
				edit_limit = 10;
				edit_width = 10;
				value = 0;
			}
		}
		// want to make this on the right side of the box
		: button {
			key = "select_pt";
			label = "Select a point on screen";
			alignment = right;
		}
	}
	// would be SO COOL if when you hovered over each of these an image popped up that showed what each looks like
	: boxed_radio_column {label = "Crease Pattern Type";	
		: radio_button {
			key = "mountain";
			label = "Mountain";
			value = "1";
		}
		: radio_button {
			key = "valley";
			label = "Valley";
		}
		: radio_button {
			key = "polygon";
			label = "Polygon";
		}
	}

	: boxed_radio_column {label = "Chirality";	
		: radio_button {
			key = "cw";
			label = "Clockwise";
			value = "1";
		}
		: radio_button {
			key = "ccw";
			label = "Counterclockwise";
		}
	}
	
	// i think it would be better if this had less text and instead the user could hover over a shorter description and get more detail then
	: boxed_column {label = "More Options";	
		: row {
			: toggle {
				key = "hole";
				label = "Circular hole in the top and bottom polygons?";
				// value = nil;
			}
			: edit_box {
				key = "diameter";
				label = "Diameter";
			}
		}
		: toggle {
			key = "layers";
			label = "Creases and outline in different layers for laser cutting?";
			alignment = left;
			// value = nil;
		}
	}
	ok_cancel;
	
	: errtile
	{
	width = 100;
	}
}
