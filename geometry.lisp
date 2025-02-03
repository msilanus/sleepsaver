(in-package :sleepsaver)

(defun deg->rad (deg)
  (* pi (/ deg 180)))

(defun rad->deg (rad)
  (* 180 (/ rad pi)))

(defun euclidian-distance (x1 y1 x2 y2)
  "Compute the euclidian distance between (X1 Y1) and (X2 Y2)."
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(defun null-vector-p (v)
  "Check if vector V is the null vector."
  (every (lambda (x) (= x 0)) v))

(defun norm (v)
  "Compute the norm of vector V."
  (sqrt (reduce #'+ (map 'vector (lambda (x) (expt x 2)) v))))

(defun mul (v alpha)
  "Multiply a vector V by scalar ALPHA."
  (map 'vector (lambda (x) (* x alpha)) v))

(defun dot (v1 v2)
  "Compute the dot product between V1 and V2."
  (loop for i from 0 to (- (length v1) 1)
	sum (* (aref v1 i)
	       (aref v2 i))))

(defun make-2d-transform (x y theta)
  "Create a 2D affine transform with translation #(X Y) and rotation
THETA."
  (let ((cos-theta (cos theta))
	(sin-theta (sin theta)))
    (make-array '(2 3)
		:initial-contents
		(list (list cos-theta (- sin-theta) x)
		      (list sin-theta cos-theta     y)))))

(defun 2d-transform (transform vec)
  "Apply the 2x3 2D TRANSFORM matrix to 2D vector VEC. TRANSFORM is of
the form #2A((cos(θ) -sin(θ) tx) (sin(θ) cos(θ) ty))."
  (let ((output (make-array 2))
	(extended-vec
	  (make-array 3 :initial-contents (list (aref vec 0) (aref vec 1) 1))))
    (loop for i from 0 to 1
	  do (setf (aref output i)
		   (dot (make-array 3 :initial-contents (list (aref transform i 0)
							      (aref transform i 1)
							      (aref transform i 2))) 
			extended-vec)))
    output))
