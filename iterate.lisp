(in-package :util)

(defmacro-driver (FOR var IN-CSV filename &optional CODE code)
  `(progn
     (for ,var :in (csv-read-to-list ,filename :code ,code))))

(defmacro-driver (FOR var IN-DIRECTORY dir &optional TYPE type REGEXP regexp)
  (let ((list (gensym)))
    `(progn
       (with ,list = (directory-list ,dir :type ,type :regexp ,regexp))
       (for ,var :in ,list))))

(defmacro-driver (FOR var IN-ALLF dir &optional TYPE type REGEXP regexp)
  (let ((list (gensym)))
    `(progn
       (with ,list = (allf ,dir :type ,type :regexp ,regexp))
       (for ,var :in ,list))))

(defmacro-clause (phash el &optional KEY key VALUE value TEST test CONDITION condition)
  (let ((hash (gensym)))
    `(progn
       (with ,hash = (make-hash-table :test ,(or test '#'equal)))
       (for k  = (funcall ,(or key '#'identity) ,el))
       (for v  = (funcall ,(or value '#'identity) ,el))
       (if ,condition
	   (setf (gethash k ,hash) (cons v (gethash k ,hash))))
       (finally (return ,hash)))))

;; (defmacro-clause (chash el &optional KEY key TEST test)
;;   (let ((hash (gensym)))
;;     `(progn
;;        (with ,hash = (make-hash-table :test ,(or test '#'equal)))
;;        (for k  = (funcall ,(or key '#'identity) ,el))
;;        (setf (gethash k ,hash)
;; 	     (aif (gethash k ,hash) (1+ it) 1))
;;        (finally (return ,hash)))))

(defmacro-clause (chash el &optional KEY key TEST test)
  `(progn
     (with hash = (make-hash-table :test ,(or test '#'equal)))
     (for k  = (funcall ,(or key '#'identity) ,el))
     (setf (gethash k hash)
	   (aif (gethash k hash) (1+ it) 1))
     (finally (return hash))))

(defmacro-clause (shash el &optional KEY key VALUE value TEST test CONDITION condition)
  (let ((hash (gensym)))
    `(progn
       (with ,hash = (make-hash-table :test ,(or test '#'equal)))
       (for k  = (funcall ,(or key '#'identity) ,el))
       (for v  = (funcall ,(or value '#'identity) ,el))
       (if ,condition
	   (setf (gethash k ,hash) v))
       (finally (return ,hash)))))

(in-package :cl-user)
