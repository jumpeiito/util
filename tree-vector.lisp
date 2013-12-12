;; (load-lib "util")
(in-package :util)

(defun tree-map (function tree)
  (declare (type function function) (type list tree)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((inner (subt r)
	     (if (null (the list subt))
		 (the list (reverse (the list r)))
		 (inner (the list (cdr subt))
			(if (atom (car subt))
			    (cons (the list (funcall function (the list (car subt))))
				  (the list r))
			    (cons (the list (tree-map function (the list (car subt))))
				  (the list r)))))))
    (inner (the list tree) '())))

(defun tree-map-with-index (function tree)
  (declare (type function function) (type list tree)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((inner (subt r)
	     (if (null (the list subt))
		 (the list (reverse (the list r)))
		 (inner (the list (cdr subt))
			(if (atom (car subt))
			    (cons (the list (funcall function (the list (car subt))))
				  (the list r))
			    (cons (the list (tree-map function (the list (car subt))))
				  (the list r)))))))
    (inner (the list tree) '())))

(defun tree-for-each (function tree)
  (declare (type function function) (type list tree)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((inner (subt)
	     (if subt
		 (if (atom (car subt))
		     (progn
		       (funcall function (the list (car subt)))
		       (inner (the list (cdr subt))))
		     (progn
		       (tree-for-each function (the list (car subt)))
		       (tree-for-each function (the list (cdr subt))))))))
    (inner (the list tree))))

(defun array-length (ar)
  (array-total-size ar))

(defun array-last (ary)
  (aref (the vector ary) (- (length ary) 1)))

(defun vector-map-with-index (fn vector)
  (declare (function fn) (vector vector)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let1 v (the vector (copy-seq vector))
    (loop for i from 0 to (- (the fixnum (length (the vector v))) 1)
    	 do (setf (elt (the vector v) i) (funcall fn i (elt (the vector v) i))))
    (the vector v)))

(defun vector-join (list-of-vector)
  (declare (type list list-of-vector)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((sub (subl r)
	     (if (null subl)
		 r
		 (funcall #'sub (the list (cdr subl))
			  (the array (concatenate 'vector r (car subl)))))))
    (funcall #'sub (cdr list-of-vector) (car list-of-vector))))

(defun vector-append (&rest vectors)
  (vector-join vectors))

(defun vector-flatten (v-of-v)
  (declare (type array v-of-v) (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((sub (subv r)
	     (if (eq 0 (length subv))
		 r
		 (funcall #'sub (the array (subseq subv 1 (the fixnum (length subv))))
			  (the array (vector-append (the array r) (the array (elt subv 0))))))))
    (funcall #'sub (subseq (the array v-of-v) 1 (the fixnum (length v-of-v)))
	     (elt (the array v-of-v) 0))))

(defun vector-car (vector)
  (declare (type array vector) (optimize (speed 3) (safety 0) (debug 0)))
  (elt (the array vector) 0))

(defun vector-cdr (vector)
  (declare (type array vector) (optimize (speed 3) (safety 0) (debug 0)))
  (subseq (the array vector) 1 (length (the array vector))))

(defun vector-null (vector)
  (declare (type array vector) (optimize (speed 3) (safety 0) (debug 0)))
  (eq 0 (length (the vector vector))))

(defun vector-take (vector num)
  (declare (type array vector) (type fixnum num)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (the array (subseq vector 0 num)))

(defun vector-drop (vector num)
  (declare (type array vector) (type fixnum num)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (the array (subseq vector num (length vector))))

(defun vector-group (vector num)
  (declare (type array vector) (type fixnum num)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((sub (subv r)
	     (cond
	       ((vector-null subv) r)
	       ((> num (length (the array subv)))
		(vector-append r (vector subv)))
	       (t
		(funcall #'sub (vector-drop subv num)
			 (vector-append r (vector (vector-take subv num))))))))
    (funcall #'sub vector #())))

(defun vector-map (function vector)
  (declare (type array vector)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let1 size (the fixnum (length vector))
    (let1 v (the array (copy-seq vector))
      (dotimes (i size)
	(setf (aref (the array v) i) (funcall function (aref (the array v) i))))
      v)))

(defun vector-find-if (pred v)
  (iter (for el :in-vector v)
	(aif (funcall pred el)
	     (leave it)
	     (next-iteration))))

(defun vector-find-if->index (pred v)
  (iter (for el :in-vector v)
	(for c :upfrom 0)
	(aif (funcall pred el)
	     (leave c)
	     (next-iteration))))

(defmacro aref-1+ (array &rest subscripts)
  `(setf (aref ,array ,@subscripts)
	 (the fixnum (1+ (the fixnum (aref ,array ,@subscripts))))))

(defmacro aref-cons (array value &rest subscripts)
  `(setf (aref ,array ,@subscripts)
	 (cons ,value (aref ,array ,@subscripts))))

(defun vector-sum (v1 v2)
  (map 'vector
       #'+
       (coerce v1 'list)
       (coerce v2 'list)))

;; (defparameter xl (mapcar (lambda (_) (random 100000)) (iota :from 0 :to 100000)))
;; (defparameter xf (map 'vector #'identity xl))

;; (time (find-if (lambda (el) (eq el 100001)) xl))
;; (time (vector-find-if (lambda (el) (eq el 100001)) xf))

(in-package :cl-user)
