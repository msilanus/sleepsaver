(in-package :sleepsaver)

(defun insert-after (lst elt prev-elt)
  "Insert ELT in LST just after PREV-ELT."
  (let ((index (position prev-elt lst)))
    (push elt (cdr (nthcdr index lst)))
    lst))

(defun insert-before (lst elt next-elt)
  "Insert ELT in LST just before NEXT-ELT."
  (let ((index (position next-elt lst)))
    (if (= index 0)
	(push elt lst)
	(push elt (cdr (nthcdr (1- index) lst))))
    lst))

(defun alist-keys (alist)
  "Get all the keys of ALIST."
  (mapcar (lambda (elt) (car elt)) alist))
