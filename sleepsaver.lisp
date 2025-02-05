;;;; sleepsaver.lisp
;;;;
;;;; Sleepsaver is a strategy editor for the FRC

(in-package :sleepsaver)


;;; Style
;;; =====

(defparameter *style-block* nil
  "The main style block of the application.")

(defparameter *style-alist* nil
  "An alist where each element is of the form (CSS SELECTOR . STYLE).")

(defun update-style ()
  (setf (clog:inner-html *style-block*)
	(format nil "~{~A~^~%~}" (loop :for (selector . style) :in *style-alist*
				       :collect (format nil "~A { ~A }" selector style)))))

(defun set-style (selector style)
  "Set STYLE for SELECTOR. Will erase the previous style set for
SELECTOR, and refresh *STYLE-BLOCK*."
  (setf (a:assoc-value *style-alist* selector :test #'string=) style)
  (update-style))

(defun get-style (selector)
  "Get STYLE associated with SELECTOR."
  (a:assoc-value *style-alist* selector :test #'string=))

(defun get-object-style (obj)
  (let* ((objkey (if (typep obj 'string) obj (clog:html-id obj)))
	 (selector (format nil "#~A" objkey)))
    (a:assoc-value *style-alist* selector :test #'string=)))

(defun set-object-style (obj style)
  "Set STYLE for the id of OBJ, using `set-style'."
  (let* ((objkey (if (typep obj 'string) obj (clog:html-id obj)))
	 (selector (format nil "#~A" objkey)))
    (set-style selector style)))

(defun unset-style (selector)
  "Unset style for SELECTOR. Will refresh *STYLE-BLOCK*."
  (setf *style-alist* (remove selector *style-alist* :key #'car :test #'string=))
  (update-style))

(defun unset-object-style (obj)
  "Unset style for SELECTOR using `unset-style'."
  (let* ((objkey (if (typep obj 'string) obj (clog:html-id obj)))
	 (selector (format nil "#~A" objkey)))
    (unset-style selector)))


;;; Rendering Functions
;;; ===================

(defun compute-robot-states (initial-rstate moves)
  (let ((rstate (copy-robot-state initial-rstate)))
    (append (list (copy-robot-state initial-rstate))
	    (loop :for move :in moves
		  :do (setf rstate (move-perform-terrain-move move rstate))
		  :collect (copy-robot-state rstate)))))

(defun toggle-highlight-move-render (move onp)
  (loop :named moveloop
	:for i :from 0 :to (length (state-moves *current-state*))
	:do (let ((render-id (move-render-id i)))
	      (when (eql (clog-propstore-read render-id :move) move)
		(if onp
		    (set-object-style
		     render-id
		     "fill: yellow; stroke: yellow; outline: 5px dashed yellow; outline-offset: 30px")
		    (unset-object-style render-id))
		(return-from moveloop)))))

(defun render-robot-states (moves robot-states)
  "Render the moves of *CURRENT-STATE* on *MAP-WINDOW*."
  (let* ((map (background-map *current-config*))
	 (svg (with-map-canvas (map canvas)
		(svgrender-robot-states canvas robot-states *current-config*)
		(svg->str canvas)))
	 (body (map-window-body))
	 (moves-overlay (clog-element-by-id body "map-moves-div"))
	 (draw-robot (if (draw-robot-fun *current-config*)
			 (draw-robot-fun *current-config*)
			 #'svgrender-robot-default)))
    (if (string= (clog:inner-html moves-overlay) "undefined")
	(clog:create-div
	 body
	 :html-id "map-moves-div"
	 :content svg
	 :style "position: absolute; top: 0; width: 100%; height: 100% z-index: 2;")
	(setf (clog:inner-html moves-overlay) svg))
    ;; write propstore to associate a move to each move-render
    (loop :for move :in moves
	  :for i :from 0
	  :do (progn (clog-propstore-write (move-render-id i) :move move)
		     (unset-object-style (move-render-id i))))
    ;; set drag and drop actions
    ;; NOTE: it is not actually possible to use the drag and drop API
    ;; with SVG element, so we retorts to using mouse-down and
    ;; mouse-up events
    (loop :for move :in moves
	  :for i :from 0
	  :do (when (move-draggable-p move)
		(let ((move-obj (clog-element-by-id body (move-render-id i)))
		      (cur-i i)
		      (cur-move move))
		  (clog:set-on-mouse-down
		   move-obj
		   (lambda (obj event)
		     (declare (ignore obj event))
		     (let* ((start-coords
			      (mapdiv->map (map-mapdiv)
					   map
					   (- (float *mousex*) (clog:offset-left body))
					   (- (float *mousey*) (clog:offset-top body))))
			    (startx (first start-coords))
			    (starty (second start-coords))
			    (drag-preview-div
			      (clog:create-div
			       body
			       :html-id "drag-preview-div"
			       :style "position: absolute; top: 0; width: 100%; height: 100% z-index: 2;")))
		       ;; preview
		       (clog:set-on-mouse-move
			(map-window-body)
			(lambda (obj event)
			  (declare (ignore obj))
			  (setf (clog:inner-html drag-preview-div)
				(destructuring-bind (mousex mousey)
				    (mapdiv->map (map-mapdiv)
						 map
						 (float (getf event :x))
						 (float (getf event :y)))
				  (with-map-canvas (map canvas)
				    (let ((robot-state (nth cur-i robot-states)))
				      (funcall
				       draw-robot
				       canvas
				       (make-robot-state
					:x mousex
					:y mousey
					:theta (atan (- mousey (robot-state-y robot-state))
						     (- mousex (robot-state-x robot-state)))
					:time (robot-state-time robot-state)))
				      (svgrender-arrow
				       canvas
				       startx starty
				       mousex mousey
				       50
				       :stroke "orange"
				       :stroke-width 5
				       :fill "orange")
				      (svg->str canvas))))))))
		     ;; release
		     (clog:set-on-mouse-up
		      (map-window-body)
		      (lambda (obj event)
			(declare (ignore obj))
			(destructuring-bind (x y)
			    (mapdiv->map (map-mapdiv)
					 map
					 (float (getf event :x))
					 (float (getf event :y)))
			  (setf (nth cur-i (state-moves *current-state*)) (move-drag cur-move x y)))
			
			(clog:set-on-mouse-move (map-window-body) nil)
			(clog:set-on-mouse-up (map-window-body) nil)
			(clog:destroy (clog-element-by-id body "drag-preview-div"))
			(render-moves))))))))))

(defun set-move-summary-ctx-menu (p move)
  (let* ((action (get-move-action move))
	 (move-i (position move (state-moves *current-state*)))
	 (previous-action (action-before move-i))
	 (next-action (action-after move-i)))
    (clog-set-context-menu
     p
     (remove-if
      #'null
      (append
       (list
	(cons "Edit"
	      (lambda (obj x y)
		(declare (ignore x y))
		(make-move-edit-form
		 obj
		 move
		 (lambda (new-move)
		   (replace-move move new-move)
		   (render-moves)))))
	(cons "Copy"
	      (lambda (obj x y)
		(declare (ignore obj x y))
		(trivial-clipboard:text (move-format move))))
	(cons "Delete"
	      (lambda (obj x y)
		(declare (ignore obj x y))
		(delete-move move)
		(render-moves))))
       (if action
	   (list
	    (cons "Copy Action"
		  (lambda (obj x y)
		    (declare (ignore obj x y))
		    (trivial-clipboard:text (format-moves (action-moves action)))))
	    (cons "Delete Action"
		  (lambda (obj x y)
		    (declare (ignore obj x y))
		    (delete-action action)
		    (render-moves))))
	   (list (cons "New Action"
		       (lambda (obj x y)
			 (declare (ignore obj x y))
			 (let ((move-position (position move (state-moves *current-state*))))
			   (push-action move-position move-position))
			 (render-moves)))
		 (when previous-action
		   (cons (format nil "End ~A Action Here" (action-color previous-action))
			 (lambda (obj x y)
			   (declare (ignore obj x y))
			   (setf (action-end previous-action) move-i)
			   (render-moves))))
		 (when next-action
		   (cons (format nil "Start ~A Action Here" (action-color next-action))
			 (lambda (obj x y)
			   (declare (ignore obj x y))
			   (setf (action-start next-action) move-i)
			   (render-moves)))))))))))

(defun write-to-file (filename content)
  "Écrit le contenu de 'content' dans un fichier spécifié par 'filename'.
   Si 'overwrite-p' est non-nil, le fichier est écrasé. Sinon, on ajoute à la fin."
  (ensure-directories-exist "~/strategies/")
  (with-open-file (stream filename 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~A~%" content)))

;; TODO
;; summary of each move in a dedicated window. For each move, also add
;; the estimated match time and robot pose.
(defun render-summary (moves robot-states)
  (declare (ignore robot-states)) ;; TODO: use robot-states info
  (clog-pdestroy-children (summary-window-body))
  (let* ((body (summary-window-body))
  	 ;; Modification du comportement du bouton copy d'origine -> Save to strategy.txt
	 (copy-button (clog:create-button body :content "Save to strategy.txt")))
    (clog:set-on-click copy-button
		       (lambda (obj)
			 (declare (ignore obj))
			 ;;(trivial-clipboard:text (format-moves moves))
			 (setf formatted-moves 
			 	(subseq (format-moves moves) 0 (1- (length (format-moves moves)))))
			 (setf strategy (concatenate 'string 
                            "STRATEGY = [ " 
                            (string #\Newline)  ;; Transforme #\Newline en chaîne
                            formatted-moves 
                            (string #\Newline)  ;; Transforme à nouveau #\Newline en chaîne
                            "]"))
			 (write-to-file "~/strategies/strategy.txt" strategy)
			 (princ "Strategy saved in ")
			 ))
    (loop :for move :in moves
	  :do (let* ((action (get-move-action move))
		     (style (if action
				(format nil "margin: 3px; color: ~A;" (action-color action))
				"margin: 3px;"))
		     (p (clog:create-p body :content (move-format move)
					    :class "move-summary-entry"
					    :style style))
		     (inner-move move))
		;; enable drag and drop
		(setf (clog:attribute p "draggable") "true")
		(clog:set-on-drag-start
		 p
		 (lambda (obj) (declare (ignore obj)))
		 :drag-data (position inner-move (state-moves *current-state*)))
		(clog:set-on-drag-enter p (lambda (obj) (declare (ignore obj)))) ;; set as target
		(clog:set-on-drag-over p (lambda (obj) (declare (ignore obj)))) ;; set as target
		(clog:set-on-drop
		 p
		 (lambda (obj event)
		   (declare (ignore obj))
		   (let* ((dragged-move-pos (parse-integer (getf event :drag-data)))
			  (dragged-move (nth dragged-move-pos (state-moves *current-state*))))
		     (when (not (eql dragged-move inner-move))
		       (delete-move dragged-move)
		       (insert-move-before dragged-move inner-move)
		       (render-moves)))))
		;; move highlight
		(clog:set-on-mouse-enter
		 p
		 (lambda (event)
		   (declare (ignore event))
		   (toggle-highlight-move-render inner-move t)))
		(clog:set-on-mouse-leave
		 p
		 (lambda (event)
		   (declare (ignore event))
		   (toggle-highlight-move-render inner-move nil)))
		;; context menu
		(set-move-summary-ctx-menu p inner-move)))
    (set-style ".move-summary-entry:hover" "outline: 2px inset; outline-offset: -2px;")))

(defun render-moves ()
  (let* ((initial-rstate (state-initial-robot-state *current-state*))
	 (moves (state-moves *current-state*))
	 (robot-states (compute-robot-states initial-rstate moves)))
    (render-summary moves robot-states)
    (render-robot-states moves robot-states)))

;;;; Run
;;;; ===

(defparameter *mousex* nil)
(defparameter *mousey* nil)

(defparameter *body* nil)

(defparameter *map-window* nil)

(defun map-window-body ()
  (clog:next-sibling (clog:first-child *map-window*)))

(defparameter *summary-window* nil)

(defun summary-window-body ()
  (clog:next-sibling (clog:first-child *summary-window*)))

(defparameter *file-ddown* nil)
(defparameter *edit-ddown* nil)
(defparameter *config-ddown* nil)

(defun on-set-initial-state (obj)
  (make-robot-state-input-form
   obj
   (lambda (rstate)
     (setf (state-initial-robot-state *current-state*) rstate)
     (render-moves))
   (state-initial-robot-state *current-state*)))

(defun on-open (obj)
  (clog-gui:server-file-dialog obj
   "Open..."
   "."
   (lambda (path)
     (when path
       (setf *current-state* (load-state path))
       (switch-config (state-config *current-state*))
       (render-moves)))))

(defun on-save (obj)
  (clog-gui:server-file-dialog obj "Save..." "." (lambda (path) (save-state *current-state* path))))

(defun run-sleepsaver ()
  (clog:initialize
   (lambda (body)
     (setf (clog:title (clog:html-document body)) "Sleepsaver 2")
     (clog-gui:clog-gui-initialize body)
     (let* ((style-block (clog:create-style-block body :content ""))
	    (menu (clog-gui:create-gui-menu-bar body))
	    (file-ddown (clog-gui:create-gui-menu-drop-down menu :content "File"))
	    (open-button
	      (clog-gui:create-gui-menu-item file-ddown :content "Open..." :on-click #'on-open))
	    (save-button
	      (clog-gui:create-gui-menu-item file-ddown :content "Save..." :on-click #'on-save))
	    (edit-ddown (clog-gui:create-gui-menu-drop-down menu :content "Edit"))
	    (set-initial-state-button
	      (clog-gui:create-gui-menu-item
	       edit-ddown
	       :content "Set Initial State..."
	       :on-click #'on-set-initial-state))
	    (clear-moves-button
	      (clog-gui:create-gui-menu-item
	       edit-ddown
	       :content "Clear moves"
	       :on-click (lambda (obj) (clear-moves) (render-moves)))))
       (setf *body* body)
       (setf *style-block* style-block)
       (setf *file-ddown* file-ddown)
       (setf *config-ddown* edit-ddown)
       (setf *config-ddown* (clog-gui:create-gui-menu-drop-down menu :content "Moves"))
       (setf *map-window* (clog-gui:create-gui-window body :title "map" :width 800 :height 600))
       (setf *summary-window* (clog-gui:create-gui-window body :title "summary" :width 450 :height 600)))
     ;; track mouse pointer
     (clog:set-on-pointer-move body (lambda (obj event)
				      (setf *mousex* (getf event :x))
				      (setf *mousey* (getf event :y))))))
  (clog:open-browser)
  ;; wait for windows to initialize
  (loop :while (or (null *map-window*)
		   (null *summary-window*))
	:do (sleep 0.1))
  (switch-config :2025-ceri) ;;
  (render-moves))

(defun run-sleepsaver-forever ()
  (run-sleepsaver)
  (sleep most-positive-fixnum))

(defun stop-sleepsaver ()
  (clog:shutdown)
  (setf
   *file-ddown* nil
   *config-ddown* nil
   *map-window* nil
   *summary-window* nil))
