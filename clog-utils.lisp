(in-package :sleepsaver)

;; Propstore
;; ---------

(defparameter *clog-propstore* (make-hash-table :test #'equal)
  "A store for storing custom properties relative to clog-elements.")

(declaim (ftype (function ((or clog:clog-obj string) symbol t)) clog-propstore-write))
(defun clog-propstore-write (obj key value)
  (let* ((objkey (if (typep obj 'string) obj (clog:html-id obj)))
	 (current-alist (gethash objkey *clog-propstore*)))
    (if current-alist
	;; extend the existing alist...
	(setf (a:assoc-value current-alist key) value)
	;; ... or create a new one if it doesn't exist yet
	(setf (gethash objkey *clog-propstore*) (list (cons key value))))))

(declaim (ftype (function ((or clog:clog-obj string) symbol)) clog-propstore-read))
(defun clog-propstore-read (obj key)
  (let* ((objkey (if (typep obj 'string) obj (clog:html-id obj)))
	 (current-alist (gethash objkey *clog-propstore*)))
    (when current-alist
      (a:assoc-value current-alist key))))

(declaim (ftype (function ((or clog:clog-obj string))) clog-propstore-rem))
(defun clog-propstore-rem (obj)
  (let ((objkey (if (typep obj 'string) obj (clog:html-id obj))))
    (remhash objkey *clog-propstore*)))

(declaim (ftype (function (&optional clog:clog-obj)) clog-propstore-rem))
(defun clog-propstore-cleanup (&optional obj)
  (loop :for key :in (a:hash-table-keys *clog-propstore*)
	:do (when (string= (clog:html-id (clog-element-by-id (or obj *body*) key)) "undefined")
	      (remhash key *clog-propstore*))))


;; General utils
;; -------------

(defun clog-pdestroy (obj)
  "Destroy OBJ, and clean up *CLOG-PROPSTORE* correctly."
  (clog-propstore-rem obj)
  (clog:destroy obj))

(defun clog-set-context-menu (obj menu)
  "Activate a context menu for OBJ. MENU is an alist where each element
is of the form (TEXT . FUNCTION). Each FUNCTION must have (obj x y)
arguments. x and y correspond to the position of the click that
created the menu relative to its parent."
  (clog:set-on-context-menu
   obj
   (lambda (obj)
     (let* ((menudiv
	      (clog:create-div
	       obj
	       :class "ctxmenu"
	       :style (format nil "z-index: 999; position: fixed; left: ~Apx; top: ~Apx; display: block; border: 1px solid; background-color: #C0C0C0;" *mousex* *mousey*)))
	    (x (- (clog:offset-left menudiv) (clog:offset-left obj)))
	    (y (- (clog:offset-top menudiv) (clog:offset-top obj))))
       (loop :for (text . fn) :in menu
	     :do (let ((div (clog:create-div menudiv :content text :class "ctxmenudiv"))
		       (fn fn)) 
		   (clog:set-on-click div (lambda (obj) (funcall fn obj x y)) :one-time t)))
       (set-style ".ctxmenudiv:hover" "background-color: lightblue;")
       (clog:set-on-click
	*body*
	(lambda (obj) (loop :for element :in (clog-elements-by-class obj "ctxmenu")
		       :do (clog-pdestroy element)))
	:one-time t)))))


(defun clog-element-by-id (obj html-id)
  (clog:attach-as-child
   obj
   (clog:jquery-query obj (format nil "find('#~A').prop('id')" html-id))))

(defun clog-elements-by-class (obj class)
  (mapcar
   (lambda (id) (clog:attach-as-child obj id))
   (ppcre:split
    ","
    (clog:js-query
     *body*
     (format nil "$.map($('.~A'), function(n, i){return n.id;});" class)))))

(defun clog-pdestroy-children (obj)
  (setf (clog:inner-html obj) "")
  (clog-propstore-cleanup obj))

(defun clog-form-dialog-get-str (result key)
  (first (a:assoc-value result key :test #'string=)))

(defun clog-form-dialog-get-float (result key)
  (parse-float:parse-float (first (a:assoc-value result key :test #'string=))))
