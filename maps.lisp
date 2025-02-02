(in-package :sleepsaver)

(defstruct background-map
  (origin-x 0.0 :type float)
  (origin-y 0.0 :type float)
  (boundary-x 0.0 :type float)
  (boundary-y 0.0 :type float)
  orientation
  svg)

(defun background-map-real-width (map)
  (if (eq (background-map-orientation map) :portrait) 2.0 3.0))

(defun background-map-real-height (map)
  (if (eq (background-map-orientation map) :portrait) 3.0 2.0))

(defun make-background-map-from-path (path)
  "Import a map from PATH. PATH should be the top directory of a map
directory, and should not end with a '/'."
  (let ((background-path (format nil "~a/background.svg" path))
	(data-path (format nil "~a/data.lisp" path)))
    (with-open-file (f data-path)
      (let* ((map-data (read f))
	     (origin (cdr (assoc :origin map-data)))
	     (boundary (cdr (assoc :boundary map-data)))
	     (orientation (cdr (assoc :orientation map-data))))
	(make-background-map :origin-x (first origin) :origin-y (second origin)
			     :boundary-x (first boundary) :boundary-y (second boundary)
			     :orientation orientation
			     :svg (uiop:read-file-string background-path))))))

(defparameter *maps* `((:2023 . ,(make-background-map-from-path "./maps/2023"))
		       (:2024 . ,(make-background-map-from-path "./maps/2024"))
		       (:2025 . ,(make-background-map-from-path "./maps/2025"))))
