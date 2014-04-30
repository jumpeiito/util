(in-package :util)

(defun sethash (key val hash)
  (setf (gethash key hash) val))

(defmacro push-hash (key val hash)
  `(setf (gethash ,key ,hash)
	 (cons ,val (gethash ,key ,hash))))

(defmacro defun-hash-table (fnname value)
  `(defun ,fnname (keyfn valfn l &key (test #'equal))
     (let1 hash (make-hash-table :test test)
       (dolist (i l)
	 (let1 key (funcall keyfn i)
	   (sethash key ,value hash)))
       hash)))

(defun-hash-table simple-hash-table
    (funcall valfn i))

(defun-hash-table push-hash-table
    (aif (gethash key hash)
	 (cons (funcall valfn i) it)
	 (list (funcall valfn i))))

(defun-hash-table count-hash-table
    (aif (gethash key hash) (+ 1 it) 1))
