(in-package :sleepsaver)

(defconstant all-action-colors
  '("Red" "SeaGreen" "SteelBlue" "Orange" "Purple" "Yellow" "Brown" "Pink"))

(defun get-unused-action-color (actions)
  "Given a list of existing ACTIONS, return the first unused color."
  (let ((used-colors (mapcar #'action-color actions)))
    (loop :for color :in all-action-colors 
	  :when (not (member color used-colors))
	    :return color)))
