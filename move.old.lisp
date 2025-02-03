(in-package :sleepsaver)

(defclass move () ())

(defgeneric move-format (obj)
  (:documentation "Format a move."))

(defgeneric move-perform-terrain-move (obj rstate)
  (:documentation "Perform move on the terrain."))

(defgeneric move-click-make (move-class-name x y)
  (:documentation "Create a move from a click event. Return a MOVE if applicable, `nil'
otherwise."))

(defun move-click-makable-p (move-class)
  "Check if a move is click-makable."
  (not (null (move-click-make move-class 0.0 0.0))))

(defgeneric move-drag (obj x y)
  (:documentation "Drag a move across the terrain."))

(defun move-draggable-p (move)
  "Check if a move is draggable."
  (not (null (move-drag move 0.0 0.0))))

(defgeneric move-scroll (obj scroll)
  (:documentation "Scroll a move."))

(defun move-scrollable-p (move)
  "Check if a move is scrollable."
  (not (null (move-scroll move 0.0 0.0))))

(defmacro define-move (name parameters apply-template &key terrain-move click-make drag scroll)
  "Create a move named NAME, that can be formatted using APPLY-TEMPLATE
and with class params PARAMETERS. Optionally, TERRAIN-MOVE defines how
the move displace the robot on the terrain. CLICK-MAKE defines how to
create the move on a click event. DRAG defines how to drag the move
across the terrain. SCROLL defines how to scroll the move."
  (let* ((move-name (concatenate 'string (symbol-name name) "-MOVE"))
	 (move-symbol (intern move-name))
	 (parameters-names (mapcar (lambda (param) (first param)) parameters)))
    `(progn
       (defclass ,move-symbol (move)
	 ,(loop for param in parameters
		collect (append param
				(list :accessor (intern (format nil "~a" (first param)))))))
       (defmethod move-format ((move ,move-symbol))
	 (funcall ,apply-template move))
       ;; writing (useful, for example, in the case of serialization)
       (defmethod print-object ((move ,move-symbol) stream)
	 (format stream
		 "#m(SLEEPSAVER::~a ~{~a~^ ~})"
		 ,(string-upcase move-name)
		 (list ,@(mapcar (lambda (param-name)
				   `(format nil
					    ":~a ~a"
					    ,(symbol-name param-name)
					    ,(list param-name 'move)))
				 parameters-names))))
       ;; performing terrain move
       (defmethod move-perform-terrain-move ((move ,move-symbol) rstate)
	 ,(if terrain-move
	      (list 'funcall terrain-move 'move 'rstate)
	      'rstate))
       ;; creating a move on click
       (defmethod move-click-make ((move-class-name (eql (find-class ',move-symbol))) x y)
	 ,(if click-make
	      (list 'funcall click-make 'move-class-name 'x 'y)
	      'nil))
       ;; drag the move
       (defmethod move-drag ((move ,move-symbol) x y)
	 ,(if drag
	      (list 'funcall drag 'move 'x 'y)
	      'nil))
       ;; scroll the move
       (defmethod move-scroll ((move ,move-symbol) scroll)
	 ,(if drag
	      (list 'funcall scroll 'move 'x 'y)
	      'nil))
       ;; Return the created class for convenience
       (find-class ',move-symbol))))

(defun format-moves (moves)
  "Format a list of moves."
  (format nil "~{~A~^~%~}" (mapcar #'move-format moves)))

(defun move-easy-format (move format-string)
  "Format a MOVE according to FORMAT-STRING, by iterating over the slots
of MOVE."
  (let ((slot-names (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of move)))))
    (apply #'format
	   (append (list nil format-string)
		   (mapcar (lambda (slot-name) (slot-value move slot-name)) slot-names)))))


(defun %parse-move-input-form (results move-class)
  "Magically parse RESULTS as slot values for MOVE-CLASS. Output a list
with each element of the form `(slot-keyword slot-value)' Never use
directly, see MAKE-MOVE-INPUT-FORM."
  (loop :for slot :in (sb-mop:class-slots move-class)
	:collect (let* ((slot-name (sb-mop:slot-definition-name slot))
			(slot-type (sb-mop:slot-definition-type slot))
			(value (first (a:assoc-value results (string slot-name) :test #'string=))))
		   (cons (intern (symbol-name slot-name) "KEYWORD")
			 (cond
			   ((eq slot-type 'integer) (parse-integer value))
			   ((eq slot-type 'string) value)
			   ((eq slot-type 'float) (parse-float:parse-float value))
			   ((eq slot-type 'symbol) (intern (string-upcase value) "SLEEPSAVER"))
			   (t (error "Unsupported move slot type: ~A" slot-type)))))))

(defun %move-from-slot-values (move-class slot-values)
  "Small util function to create a move of class MOVE-CLASS using
SLOT-VALUES, a list where each element is of the form (slot-keyword
slot-value)."
  (apply #'make-instance (append (list (class-name move-class)) (a:flatten slot-values))))

(defun make-move-input-form (obj move-class on-input)
  "Create a form for inputting a move of class MOVE-CLASS. When a move is
returned, call (ON-INPUT move)."
  (c2cl:ensure-finalized move-class)
  (clog-gui:form-dialog
   obj
   (format nil "Create ~A" (class-name move-class))
   (mapcar (lambda (slot)
	     (let ((slot-name (sb-mop:slot-definition-name slot)))
	       (list slot-name slot-name)))
	   (sb-mop:class-slots move-class))
   (lambda (results)
     (when (or results (= (length (sb-mop:class-slots move-class)) 0))
       (funcall
	on-input
	(%move-from-slot-values move-class (%parse-move-input-form results move-class)))))))

(defun make-move-edit-form (obj move on-input)
  "Create a form for editing MOVE. When the new move is returned,
call (ON-INPUT move)."
  (let ((move-class (class-of move)))
    (clog-gui:form-dialog
     obj
     (format nil "Edit ~A" (class-name move-class))
     ;; TODO: add default values
     (mapcar (lambda (slot)
	       (let ((slot-name (sb-mop:slot-definition-name slot)))
		 (list slot-name slot-name :text (slot-value move slot-name))))
	     (sb-mop:class-slots move-class))
     (lambda  (results)
       (when (or results (= (length (sb-mop:class-slots move-class)) 0))
	 (funcall
	  on-input
	  (%move-from-slot-values move-class (%parse-move-input-form results move-class))))))))
