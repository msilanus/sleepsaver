(in-package :sleepsaver)

(defstruct robot-state
  (x 0.0 :type float)
  (y 0.0 :type float)
  (theta 0.0 :type float)
  (time 0.0 :type float)
  (score 0 :type integer))

(defun robot-state-update-time (rstate move-duration)
  "update RSTATE by adding MOVE-DURATION to its current time."
  (setf (robot-state-time rstate) (+ (robot-state-time rstate) move-duration))
  rstate)

(defun robot-state-update-score (rstate move-score)
  "update RSTATE by adding MOVE-score to its current score."
  (setf (robot-state-score rstate) (+ (robot-state-score rstate) move-score))
  rstate)

(defun make-robot-state-input-form (obj on-input &optional robot-state)
  "Create a form for inputting a robot state. If ROBOT-STATE is given,
will use it for default values."
  (let ((x (if robot-state (format nil "~A" (robot-state-x robot-state)) "0.0"))
	(y (if robot-state (format nil "~A" (robot-state-y robot-state)) "0.0"))
	(theta (if robot-state (format nil "~A" (robot-state-theta robot-state)) "0.0"))
	(time (if robot-state (format nil "~A" (robot-state-time robot-state)) "0.0"))
	(score (if robot-state (format nil "~A" (robot-state-score robot-state)) "0"))
       )
    (clog-gui:form-dialog
     obj
     "Input robot-state"
     (list (list "x" "x" :text x)
	   (list "y" "y" :text y)
	   (list "theta" "theta" :text theta)
	   (list "time" "time" :text time)
	   (list "score" "score" :text score ))
     (lambda (result)
       (when result
	 (funcall
	  on-input
	  (make-robot-state
	   :x (clog-form-dialog-get-float result "x")
	   :y (clog-form-dialog-get-float result "y")
	   :theta (clog-form-dialog-get-float result "theta") 
	   :time (clog-form-dialog-get-float result "time")
	   :score (clog-form-dialog-get-interger result "score")
	   )))))))

(defstruct action
  (start 0 :type integer)
  (end 0 :type integer)
  (color nil))

(defstruct state
  (moves '() :type list)
  (actions '() :type list)
  (initial-robot-state (make-robot-state :x 0.0 :y 0.0 :theta 0.0) :type robot-state)
  config)

(defparameter *current-state* (make-state) "Application state")


;;; State manipulation
;;; ------------------

(defun push-move (move)
  "Push a move to the list of moves to perform."
  (let ((moves (state-moves *current-state*)))
    ;; If an action spans up to the last move, include MOVE in this action
    (when (> (length moves) 1)
      (let ((last-action (get-move-action (car (last moves)))))
	(when last-action
	  (setf (action-end last-action) (1+ (action-end last-action))))))
    ;; Finally, append MOVE
    (setf (state-moves *current-state*) (append moves (list move)))))

(defun insert-move-after (move prev-move)
  "Insert MOVE just after PREV-MOVE in the list of moves to perform."
  ;; insert MOVE
  (setf (state-moves *current-state*) (insert-after (state-moves *current-state*) move prev-move))
  ;; Adjust actions coordinates after PREV-MOVE
  (mapboundaries-actions-after
   prev-move
   (lambda (action) (setf (action-start action) (1+ (action-start action))))
   (lambda (action) (setf (action-end action) (1+ (action-end action)))))
  ;; Adjust PREV-MOVE action end if needed
  (let ((prev-move-action (get-move-action prev-move)))
    (when
	(and prev-move-action
	     (= (action-end prev-move-action)
		(position prev-move (state-moves *current-state*))))
      (setf (action-end prev-move-action) (1+ (action-end prev-move-action))))))

(defun insert-move-before (move next-move)
  "Insert MOVE just after NEXT-MOVE in the list of moves to perform."
  ;; insert MOVE
  (setf (state-moves *current-state*) (insert-before (state-moves *current-state*) move next-move))
  ;; Adjust actions coordinates after MOVE
  (mapboundaries-actions-after
   move
   (lambda (action) (setf (action-start action) (1+ (action-start action))))
   (lambda (action) (setf (action-end action) (1+ (action-end action)))))
  ;; Adjust NEXT-MOVE action start if needed
  (let ((next-move-action (get-move-action next-move)))
    (when (and next-move-action
	       (= (action-start next-move-action)
		  (position next-move (state-moves *current-state*))))
      (setf (action-start next-move-action) (1- (action-start next-move-action))))))

(defun clear-moves ()
  "Clear the list of moves to perform."
  (setf (state-moves *current-state*) nil)
  (setf (state-actions *current-state*) nil))

(defun replace-move (move new-move)
  "Replace MOVE by NEW-MOVE."
  (let ((moves (state-moves *current-state*)))
    (setf (nth (position move moves) moves) new-move)))

(defun delete-move (move)
  "Delete MOVE from the list of moves to perform."
  ;; Adjust actions that are after MOVE
  (mapboundaries-actions-after
   move
   (lambda (action) (setf (action-start action) (1- (action-start action))))
   (lambda (action) (setf (action-end action) (1- (action-end action)))))
  ;; Finally, delete MOVE
  (setf (state-moves *current-state*) (remove move (state-moves *current-state*))))

(defun delete-action (action)
  "Delete ACTION from the list of actions."
  (setf (state-actions *current-state*) (remove action (state-actions *current-state*))))

(defun push-action (start end)
  "Create and push a new action with an automatic new color."
  (let ((color (get-unused-action-color (state-actions *current-state*))))
    (push (make-action :start start :end end :color color)
	  (state-actions *current-state*))))

(defun swap-moves (move-1 move-2)
  "Swap MOVE-1 and MOVE-2 order."
  (let ((moves (state-moves *current-state*)))
    (rotatef (nth (position move-1 moves) moves)
	     (nth (position move-2 moves) moves))))

(defun get-move-action (move)
  "Get the action enclosing a MOVE (if any)."
  (let ((move-position (position move (state-moves *current-state*)))
	(actions (state-actions *current-state*)))
    (loop for action in actions
	  when (and (>= move-position (action-start action))
		    (<= move-position (action-end action)))
	    return action)))

(defun action-moves (action)
  "Get all the moves associated to ACTION."
  (let ((moves (state-moves *current-state*)))
    (loop :for move :in moves
	  :when (let ((move-pos (position move moves)))
		  (and (>= move-pos (action-start action))
		       (<= move-pos (action-end action))))
	    :collect move)))

(defun action-before (pos)
  "Get the action just before POS."
  (let ((previous-actions (remove-if
			   (lambda (saction) (>= (action-start saction) pos))
			   (state-actions *current-state*))))
    (first (sort (copy-seq previous-actions)
		 (lambda (a1 a2) (> (action-start a1) (action-start a2)))))))

(defun action-after (pos)
  "Get the action just after POS."
  (let ((next-actions (remove-if
		       (lambda (saction) (<= (action-start saction) pos))
		       (state-actions *current-state*))))
    (first (sort (copy-seq next-actions)
		 (lambda (a1 a2) (< (action-start a1) (action-start a2)))))))

(defun mapboundaries-actions-after (move start-fn end-fn)
  "Execute START-FN/END-FN for all actions that have a start/end greater
than MOVE's position."
  (let ((move-position (position move (state-moves *current-state*))))
    (loop for action in (state-actions *current-state*)
	  when (> (action-start action) move-position)
	    do (funcall start-fn action)
	  when (> (action-end action) move-position)
	    do (funcall end-fn action))))

(defun action-overlapping-p (action)
  "Check if ACTION is overlapping with any action from *CURRENT-STATE*."
  (loop for s-action in (state-actions *current-state*)
	thereis (or (and (>= (action-start action) (action-start s-action))
		      (<= (action-start action) (action-end s-action)))
		 (and (>= (action-end action) (action-start s-action))
		      (<= (action-end action) (action-end s-action))))))

;;; Saving/Loading session
;;; ----------------------

;;; Writing
;;; ~~~~~~~
;;; writing a state simply works by writing the STATE structure. The
;;; state structure contains a list of MOVE, which are classes. The
;;; DEFINE-MOVE macro contains a definition of PRINT-OBJECT that allows
;;; printing of any move with the form #m(move-type :arg1 value1 ...)

(defun save-state (state path)
  "Save STATE to file at PATH."
  (with-open-file (f path :direction :output :if-exists :supersede)
  	;; (format t "~A~%" state)
    (prin1 state f)))

;;; Reading
;;; ~~~~~~~
;;; reading is simply delegated to the lisp reader thanks to the
;;; MOVE-READER read macro. An expression such as:
;;;   #m(rotate-move :theta 0.0)
;;; is simply read as:
;;;   (make-instance 'rotate-move :theta 0.0)

(defun move-reader (stream char arg)
  (declare (ignore char arg))
  (let* ((lst (read stream t nil t)))
    `(make-instance ',(first lst) ,@(cdr lst))))

(set-dispatch-macro-character #\# #\m #'move-reader)

(defun load-state (path)
  "Load a state from file at PATH."
  (with-open-file (f path)
    (let ((state (read f)))
      ;; HACK: for some reason (??), moves are read with MOVE-READER
      ;; but not evaluated - so we eval them here
      (setf (state-moves state) (mapcar (lambda (move) (eval move)) (state-moves state)))
      state)))

