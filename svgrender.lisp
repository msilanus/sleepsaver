(in-package :sleepsaver)

(defmacro with-map-canvas ((map canvas) &body body)
  `(let ((,canvas (cl-svg:make-svg-toplevel
		  'svg:svg-1.1-toplevel
		  :width "100%"
		  :height "100%"
		  :view-box (format
			     nil
			     "~A ~A ~A ~A"
			     (background-map-origin-x map)
			     (background-map-origin-y map)
			     (background-map-boundary-x map)
			     (background-map-boundary-y map)))))
     ,@body))

(defun svg->str (svg)
  (let ((ostr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s ostr) 
      (cl-svg:stream-out s svg))
    ostr))

(defmacro svgrender-arrow (canvas x1 y1 x2 y2 alen &rest args)
  "Draw an arrow in CANVAS, going from (X1, Y1) to (X2, Y2) with arrow dimension ALEN."
  `(cl-svg:make-group ,canvas (,@args)
     (cl-svg:draw* (:line :x1 ,x1 :y1 ,y1 :x2 ,x2 :y2 ,y2))
     (let ((v (make-array 2 :initial-contents (list (- ,x2 ,x1) (- ,y2 ,y1)))))
       (when (not (null-vector-p v))
	 (let ((vnorm (mul v (/ 1 (norm v)))))
	   (cl-svg:draw*
	       (:polygon
		:points
		(format nil
			"~A,~A ~A,~A ~A,~A"
			,x2 ,y2
			(- ,x2 (* ,alen (+ (aref vnorm 0) (/ (aref vnorm 1) 4))))
			(- ,y2 (* ,alen (- (aref vnorm 1) (/ (aref vnorm 0) 4))))
			(- ,x2 (* ,alen (- (aref vnorm 0) (/ (aref vnorm 1) 4))))
			(- ,y2 (* ,alen (+ (aref vnorm 1) (/ (aref vnorm 0) 4))))))))))))


(defun svgrender-robot-default (canvas rstate)
  (let ((width 200)  ;; Largeur de 200
        (x (robot-state-x rstate))
        (y (robot-state-y rstate))
        (theta (robot-state-theta rstate))
        (alen 50))
    (cl-svg:transform (cl-svg:rotate (rad->deg theta) x y)
      (cl-svg:draw canvas
          (:rect :x (- x (/ width 2))
                 :y (- y (/ 400 2))  ;; Ajustement de la hauteur
                 :height 400  ;; Hauteur de 400
                 :width 200  ;; Largeur de 200
                 :fill "lime"
                 :fill-opacity 0.2
                 :stroke-width 5
                 :stroke "lime")))
    (format t "theta = ~A~%" theta)
    (format t "x1 = ~A~%" x)
    (format t "y1 = ~A~%" y)
    (format t "x2 = ~A~%" (+ x  (* (+ alen (/ width 2)) (cos theta))))
    (format t "y2 = ~A~%" (+ y  (* (+ alen (/ width 2)) (sin theta))))
    (svgrender-arrow
     canvas
     x
     y
     (+ x  (* (+ alen (/ width 2)) (cos theta)))
     (+ y  (* (+ alen (/ width 2)) (sin theta)))
     alen
     :fill "lime")))



(defun move-render-id (move-i)
  (format nil "move-render-~A" move-i))

(defun svgrender-robot-states (canvas rstates config)
  (let ((prev-rstate nil)
	(draw-robot (draw-robot-fun config)))
    (loop :for rstate :in rstates
	  :for i :from 0
	  :do (progn
		(funcall draw-robot canvas rstate)
		(when prev-rstate
		  (svgrender-arrow
		   canvas
		   (robot-state-x prev-rstate)
		   (robot-state-y prev-rstate)
		   (robot-state-x rstate)
		   (robot-state-y rstate)
		   50
		   :stroke "lime"
		   :stroke-width 5
		   :fill "lime"
		   :id (move-render-id (- i 1))
		   :class "move-render"))
		(setf prev-rstate rstate)))))
