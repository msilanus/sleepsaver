(in-package :sleepsaver)

(defclass config ()
  ((name
    :initarg :name
    :accessor name)
   (background-map
    :initarg :background-map
    :accessor background-map)
   (draw-robot-fun
    :initarg :draw-robot-fun
    :accessor draw-robot-fun
    :documentation "a function to draw the robot. The function should take two arguments:
CANVAS, an SVG canvas, and RSTATE, the current robot-state. See
`svgrender-robot-default' for an example.")
   (moves
    :initarg :moves
    :accessor moves
    :documentation "The list of allowed move classes.")))

(defmethod config-click-makables (config)
  "Return all the moves of CONFIG that are click-makables."
  (remove-if (lambda (move-class) (not (move-click-makable-p move-class))) (moves config)))

(defparameter *configs* (list)
  "An alist, with each element of the form (CONFIG-NAME . CONFIG).")

(defparameter *current-config* nil
  "The currently loaded configuration.")

(defun make-config (name background-map draw-robot-fun moves)
  (let ((config (make-instance
		 'config
		 :name name
		 :background-map background-map
		 :draw-robot-fun draw-robot-fun
		 :moves moves)))
    (setf (a:assoc-value *configs* name) config)
    config))

(defun config-on-move-menu-item (obj move-class)
  "Function called when the user click on the contextual menu item of a
specific move."
  (c2cl:ensure-finalized move-class)
  (if (sb-mop:class-slots move-class)
      ;; the class has more than 1 slot:
      ;; display input form
      (make-move-input-form
       obj
       move-class
       (lambda (move)
	 (push-move move)
	 (render-moves)))
      ;; otherwise: create move directly
      (progn (push-move (make-instance (class-name move-class)))
	     (render-moves))))

(declaim (ftype (function (clog:clog-element background-map float float) list) mapdiv->map))
(defun mapdiv->map (mapdiv map x y)
  "Convert a coordinate from the mapdiv frame to the map frame."
  (list
   (* (/ x (clog:width mapdiv)) (background-map-boundary-x map))
   (* (/ y (clog:height mapdiv)) (background-map-boundary-y map))))

(defun map-mapdiv ()
  "Gets the HTML div containing current map."
  (clog-element-by-id (map-window-body) "map-div"))

(defun set-config-map-ctx-menu (config mapdiv)
  "Set the map context menu for CONFIG. MAPDIV is the div containing the
map."
  (let ((background-map (background-map config)))
    (clog-set-context-menu
     (map-window-body)
     (append
      ;; a menu item for set-initial-state
      (list
       (cons "Set initial state here"
	     (lambda (obj x y)
	       (declare (ignore obj))
	       (setf (state-initial-robot-state *current-state*)
		     (destructuring-bind (x y)
			 (mapdiv->map mapdiv background-map (float x) (float y))
		       (make-robot-state :x x :y y)))
	       (render-moves))))
      ;; a menu item for each config move
      (loop :for move-class :in (moves config)
	    :collect
	    (let ((move-class move-class))
	      (cons (class-name move-class)
		    (lambda (obj x y)
		      (if (move-click-makable-p move-class)
			  (destructuring-bind (x y)
			      (mapdiv->map mapdiv background-map (float x) (float y))
			    (push-move (move-click-make move-class x y))
			    (render-moves))
			  (config-on-move-menu-item obj move-class))))))))))

(defun load-config (config-name)
  "load config CONFIG-NAME by loading background map in *MAP-WINDOW* and creating
menu items in *CONFIG-DDOWN*."
  (let* ((config (a:assoc-value *configs* config-name))
	 (background-map (background-map config))
	 ;; display map in mapdiv
	 (mapdiv (clog:create-div
		  (map-window-body)
		  :html-id "map-div"
		  :content (background-map-svg background-map)
		  :style "position: absolute; top: 0; width: 100%; height: 100% z-index: 2;")))
    ;; create menu items according to the config moves
    (loop :for move-class :in (moves config)
	  :do (let ((move-class move-class))
		(clog-gui:create-gui-menu-item
		 *config-ddown*
		 :content (class-name move-class)
		 :on-click (lambda (obj) (config-on-move-menu-item obj move-class)))))
    ;; set ctx menu for map
    (set-config-map-ctx-menu config mapdiv)
    ;; update globals, re-render
    (setf *current-config* config)
    (setf (state-config *current-state*) config-name)
    (render-moves)
    (format t "loaded config ~A~%" config-name)
    *current-config*))

(defun unload-config (config-name)
  "Delete CONFIG-NAME menus and unload background-map in WIN."
  ;; should delete menus according to the current existing moves
  (clog-pdestroy-children *config-ddown*)
  (clog-pdestroy-children (map-window-body))
  (setf *current-config* nil)
  (setf (state-config *current-state*) nil)
  (clog:set-on-mouse-click (map-window-body) nil)
  (format t "unloaded config ~A~%" config-name))

(defun switch-config (config-name)
  (when *current-config*
    (unload-config (name *current-config*)))
  (load-config config-name))


;; Configs definition
;; ==================
(make-config
 :2025
 (a:assoc-value *maps* :2025)
 #'svgrender-robot-default
 (flet ((map-to-robot (x y)
	  (let* ((initial-state (state-initial-robot-state *current-state*))

		 (initial-x (robot-state-x initial-state))
		 (initial-y (robot-state-y initial-state))
		 (initial-theta (robot-state-theta initial-state))
		 (v0 (2d-transform (make-2d-transform (- initial-x) (- initial-y) 0)
				   (make-array 2 :initial-contents (list x y))))
		 (v1 (2d-transform (make-2d-transform 0 0 (- initial-theta))
				   v0))
		 (v2 (2d-transform (make-array '(2 3)
					       :initial-contents (list (list 0 (/ 1 1000) 0)
								       (list (/ 1 1000) 0 0)))
				   v1)))
	    (coerce v2 'list))))
   ;; Useful Constants
   ;; ----------------
   (let ((+robot-vmax+ 0.4)		; max speed in m/s
	 (+robot-wmax+ pi))		; max angular speed in rad/s
     ;; Basic moves definition
     ;; ----------------------
     ;; Rotate
     (list
      (define-move 2025-rotate
	  ((theta :initarg :theta :initform 0.0 :type float))
	(lambda (move)
	  (let* ((initial-state (state-initial-robot-state *current-state*))
		 (initial-theta (robot-state-theta initial-state)))
	    (format
	     nil
	     "TRY_MOVE(rotate_to(~,3F, 10000));"
	     (- (- (theta move)) initial-theta))))
	:terrain-move
	(lambda (move rstate)
	  (let ((robot-theta (robot-state-theta rstate)))
	    (setf (robot-state-theta rstate) (theta move))
	    (robot-state-update-time
	     rstate
	     (/ (* 2 (abs (- (theta move) robot-theta))) +robot-wmax+)))
	  rstate))
      ;; Translate
      (define-move 2025-translate
	  ((x :initarg :x :initform 0.0 :type float)
	   (y :initarg :y :initform 0.0 :type float))
	(lambda (move)
	  (destructuring-bind (x y) (map-to-robot (x move) (y move))
	    (format nil "TRY_MOVE(move_to(~,3F, ~,3F, 10000));" x y))) 
	:terrain-move
	(lambda (move rstate)
	  (let* ((move-x (x move))
		 (move-y (y move))
		 (robot-x (robot-state-x rstate))
		 (robot-y (robot-state-y rstate))
		 (dist (euclidian-distance robot-x robot-y move-x move-y))
		 (new-theta (atan (- move-y robot-y) (- move-x robot-x)))
		 (theta-dist (abs (- (robot-state-theta rstate) new-theta))))
	    (setf (robot-state-theta rstate) new-theta)
	    (setf (robot-state-x rstate) move-x)
	    (setf (robot-state-y rstate) move-y)
	    (robot-state-update-time
	     rstate
	     (+ (/ (* 2 theta-dist) +robot-wmax+) ; rotation towards target
		(/ (* 2 dist) +robot-vmax+)	     ; translation
		)))
	  rstate)
	:click-make
	(lambda (move-class x y)
	  (make-instance '2025-translate-move :x x :y y))
	:drag
	(lambda (move x y)
	  (make-instance '2025-translate-move :x x :y y)))
      (define-move 2025-take () (lambda (move) "TRY_MOVE(take(10000));"))
      (define-move 2025-put () (lambda (move) "TRY_MOVE(put(10000));"))))))

(make-config
 :2025-ceri
 (a:assoc-value *maps* :2025)
 #'svgrender-robot-ceri
 (flet ((translate-along-axis (rstate offset)
	  (let* ((x           (robot-state-x rstate))
		 (y           (robot-state-y rstate))
		 (theta       (robot-state-theta rstate)))
	    (setf (robot-state-x rstate) (+ x (* offset (cos theta))))
	    (setf (robot-state-y rstate) (+ y (* offset (sin theta))))
	    rstate)))
   (list
    ;; F(orward)
    (define-move 2025-ceri-forward
	((offset :initarg :offset :initform 0 :type integer))
	  (lambda (move)
		(let ((command (format nil "move(\"F\", ~a)," (offset move))))
		  (move-easy-format move command)))  ;; Utilise command dans move-easy-format
	  :terrain-move 
	  (lambda (move rstate)
	(translate-along-axis rstate (offset move))
	(robot-state-update-time rstate (+ (/ (* 2.25 (offset move)) 1000) 0.8))))  ;; 2.25*d/1000

    ;; B(ackward)
	(define-move 2025-ceri-backward
	  ((offset :initarg :offset :initform 0 :type integer))
	  (lambda (move)
		(let ((command (format nil "move(\"B\", ~a)," (offset move))))
		  (move-easy-format move command)))  ;; Utilise command dans move-easy-format
	  :terrain-move
	  (lambda (move rstate)
		(translate-along-axis rstate (- (offset move)))
		(robot-state-update-time rstate (+ (/ (* 2.25 (offset move)) 1000) 1))))
		
	;; R(otate)
    (define-move 2025-ceri-rotate
	((theta :initarg :theta :initform 0 :type integer))
      (lambda (move) (move-easy-format move "rotate(~a),")) 
      :terrain-move
      (lambda (move rstate)
	;; theta is in degree => convert it to rad
	(let* ((theta-rad (* (theta move) (/ pi 180)))
           (theta-old (robot-state-theta rstate)))  ; Récupère l'ancienne valeur de theta
      (setf (robot-state-theta rstate) (+ theta-rad theta-old))  ; Mise à jour de theta
      (robot-state-update-time rstate (+ (/ (* 2.25 (* (abs theta-rad) 100)) 1000) 1)))))  ; Retourne l'état du robot
    
    ;; U(p)
    (define-move 2025-ceri-up
	((offset :initarg :offset :initform 0 :type integer))
      (lambda (move) (move-easy-format move "screw(\"U\", ~a),"))
      :terrain-move
      (lambda (move rstate) (robot-state-update-time rstate 0.8))
    )
    ;; D(own)
    (define-move 2025-ceri-down
	((offset :initarg :offset :initform 0 :type integer))
      (lambda (move) (move-easy-format move "screw(\"D\", ~a),"))
      :terrain-move
      (lambda (move rstate) (robot-state-update-time rstate 0.8))
    )
    ;; H(igh)
    (define-move 2025-ceri-high
	((axis :initarg :axis :initform "A" :type string))  ;;modification du type
      (lambda (move) (move-easy-format move "arm(\"H\", ~a),"))
      :terrain-move
      (lambda (move rstate) (robot-state-update-time rstate 1.0))
      )
    ;; L(ow)
    (define-move 2025-ceri-low
	((axis :initarg :axis :initform "A" :type string))  ;;modification du type
      (lambda (move) (move-easy-format move "arm(\"L\", ~a),"))
      :terrain-move
      (lambda (move rstate) (robot-state-update-time rstate 1.0))
      )
    ;; P(oint)
    (define-move 2025-ceri-point
       ((value :initarg :value :initform 0 :type integer))
       (lambda (move) (move-easy-format move "point(~a),"))
       :terrain-move
       (lambda (move rstate) (robot-state-update-score rstate (value move) ))
     )
      
)))

(make-config
 :2024
 (a:assoc-value *maps* :2024)
 #'svgrender-robot-default
 (flet ((map-to-robot (x y)
	  (let* ((initial-state (state-initial-robot-state *current-state*))

		 (initial-x (robot-state-x initial-state))
		 (initial-y (robot-state-y initial-state))
		 (initial-theta (robot-state-theta initial-state))
		 (v0 (2d-transform (make-2d-transform (- initial-x) (- initial-y) 0)
				   (make-array 2 :initial-contents (list x y))))
		 (v1 (2d-transform (make-2d-transform 0 0 (- initial-theta))
				   v0))
		 (v2 (2d-transform #2A((0 1 0) (1 0 0))
				   v1)))
	    (coerce v2 'list))))
   ;; Useful Constants
   ;; ----------------
   (let ((+robot-vmax+ 0.4)		; max speed in m/s
	 (+robot-wmax+ pi))		; max angular speed in rad/s
     ;; Basic moves definition
     ;; ----------------------
     ;; Rotate
     (list
      (define-move 2024-rotate
	  ((theta :initarg :theta :initform 0.0 :type float))
	(lambda (move)
	  (let* ((initial-state (state-initial-robot-state *current-state*))
		 (initial-theta (robot-state-theta initial-state)))
	    (format
	     nil
	     "TRY_MOVE(rotate_to(~,3F, 10000));"
	     (- (- (theta move)) initial-theta))))
	:terrain-move
	(lambda (move rstate)
	  (let ((robot-theta (robot-state-theta rstate)))
	    (setf (robot-state-theta rstate) (theta move))
	    (robot-state-update-time
	     rstate
	     (/ (* 2 (abs (- (theta move) robot-theta))) +robot-wmax+)))
	  rstate))
      ;; Translate
      (define-move 2024-translate
	  ((x :initarg :x :initform 0.0 :type float)
	   (y :initarg :y :initform 0.0 :type float))
	(lambda (move)
	  (destructuring-bind (x y) (map-to-robot (x move) (y move))
	    (format nil "TRY_MOVE(move_to(~,3F, ~,3F, 10000));" x y))) 
	:terrain-move
	(lambda (move rstate)
	  (let* ((move-x (x move))
		 (move-y (y move))
		 (robot-x (robot-state-x rstate))
		 (robot-y (robot-state-y rstate))
		 (dist (euclidian-distance robot-x robot-y move-x move-y))
		 (new-theta (atan (- move-y robot-y) (- move-x robot-x)))
		 (theta-dist (abs (- (robot-state-theta rstate) new-theta))))
	    (setf (robot-state-theta rstate) new-theta)
	    (setf (robot-state-x rstate) move-x)
	    (setf (robot-state-y rstate) move-y)
	    (robot-state-update-time
	     rstate
	     (+ (/ (* 2 theta-dist) +robot-wmax+) ; rotation towards target
		(/ (* 2 dist) +robot-vmax+)	     ; translation
		)))
	  rstate)
	:click-make
	(lambda (move-class x y)
	  (make-instance '2024-translate-move :x x :y y)))
      (define-move 2024-take-plant ()
	(lambda (move) "TRY_MOVE(take_plant(10000));")
	:terrain-move
	(lambda (move rstate)
	  (robot-state-update-time rstate 10)))
      (define-move 2024-place-ground-plant ()
	(lambda (move) "TRY_MOVE(place_ground_plant(10000));"))
      (define-move 2024-place-garden-plant ()
	(lambda (move) "TRY_MOVE(place_garden_plant(15000))")
	:terrain-move
	(lambda (move rstate)
	  (robot-state-update-time rstate 3)))
      (define-move 2024-rotate-solar-panel ()
	(lambda (move) "TRY_MOVE(rotate_solar_panel(10000));"))
      (define-move 2024-pass-claw-content-up ()
	(lambda (move) "TRY_MOVE(pass_claw_content_up(10000));"))
      (define-move 2024-pass-claw-content-down ()
	(lambda (move) "TRY_MOVE(pass_claw_content_down(10000));"))
      (define-move 2024-open-top-claws ()
	(lambda (move) "TRY_MOVE(open_top_claws(10000));"))
      (define-move 2024-push-collect ()
	(lambda (move) "TRY_MOVE(push_collect(15000));"))))))

(make-config
 :2024-ceri
 (a:assoc-value *maps* :2024)
 #'svgrender-robot-default
 (flet ((translate-along-axis (rstate axis offset)
	  (let* ((x           (robot-state-x rstate))
		 (y           (robot-state-y rstate))
		 (theta       (robot-state-theta rstate))
		 (axis-theta (cond ((eq axis 'x) theta)
				   ((eq axis 'y) (+ theta (/ (* 2 pi) 3)))
				   ((eq axis 'z) (+ theta (/ (* 4 pi) 3)))
				   (t (error "unknown axis")))))
	    (setf (robot-state-x rstate) (+ x (* offset (cos axis-theta))))
	    (setf (robot-state-y rstate) (+ y (* offset (sin axis-theta))))
	    rstate)))
   (list
    ;; F(orward)
    (define-move 2024-ceri-forward
	((axis :initarg :axis :initform 'x :type symbol)
	 (offset :initarg :offset :initform 0 :type integer))
      (lambda (move) (move-easy-format move "move(\"F\", \"~a\", ~a),"))
      :terrain-move
      (lambda (move rstate)
	(translate-along-axis rstate (axis move) (offset move))))
    ;; B(ackward)
    (define-move 2024-ceri-backward
	((axis :initarg :axis :initform 'x :type symbol)
	 (offset :initarg :offset :initform 0 :type integer))
      (lambda (move) (move-easy-format move "move(\"B\", \"~a\", ~a),"))
      :terrain-move
      (lambda (move rstate)
	(translate-along-axis rstate (axis move) (- (offset move)))))
    ;; U(p)
    (define-move 2024-ceri-up
	((axis :initarg :axis :initform 'x :type symbol)
	 (offset :initarg :offset :initform 0 :type integer))
      (lambda (move) (move-easy-format move "gripper(\"U\", \"~a\", ~a),")))
    ;; D(own)
    (define-move 2024-ceri-down
	((axis :initarg :axis :initform 'x :type symbol)
	 (offset :initarg :offset :initform 0 :type integer))
      (lambda (move) (move-easy-format move "gripper(\"D\", \"~a\", ~a),")))
    ;; R(otate)
    (define-move 2024-ceri-rotate
	((theta :initarg :theta :initform 0 :type integer))
      (lambda (move) (move-easy-format move "rotate(~a),")) 
      :terrain-move
      (lambda (move rstate)
	;; theta is in degree => convert it to rad
	(let ((theta-rad (* (theta move) (/ pi 180))))
	  (setf (robot-state-theta rstate) theta-rad)
	  rstate)))
    ;; O(pen)
    (define-move 2024-ceri-open
	((axis :initarg :axis :initform 'x :type symbol))
      (lambda (move) (move-easy-format move "claw(\"O\", \"~a\"),")))
    ;; C(lose)
    (define-move 2024-ceri-close
	((axis :initarg :axis :initform 'x :type symbol))
      (lambda (move) (move-easy-format move "claw(\"C\", \"~a\"),"))))))

(make-config
 :2023
 (a:assoc-value *maps* :2023)
 #'svgrender-robot-default
 '())
