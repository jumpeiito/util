(in-package :util)

;; (defmacro dlambda (arg &rest clause)
;;   `(lambda (key &rest ,@arg)
;;      (case key
;;        ,@(mapcar
;; 	  (lambda (xl)
;; 	    `(,(car xl) ,@(cdr xl)))
;; 	  clause))))

(defmacro dlambda (key &rest clause)
  `(lambda (,key &rest args)
     (case ,key
       ,@(mapcar
	  (lambda (l)
	    `(,(first l) (apply (lambda ,(second l) ,@(nthcdr 2 l)) args)))
	  clause))))

(defmacro def-clojure (name arglist vallist &rest clause)
  `(defun ,name ,arglist
     (let* ,vallist
       (labels ((self ()
	 (dlambda arg
	   ,@(append clause
		     (mapcar (lambda (l)
			       (list (alexandria:make-keyword (car l))
				     nil
				     (car l)))
			     vallist)))))
	 (self)))))

;; (defmacro def-clojure (name arglist vallist &rest clause)
;;   (let ((dlambda-arg (gensym)))
;;     `(defun ,name ,arglist
;;        (let* ,vallist
;; 	 (macrolet ((ref (sym) `(fn (fn #'self) ,sym)))
;; 	   (labels ((self ()
;; 		      (dlambda (arg)
;; 		        ,@(append clause
;; 		      		(mapcar (lambda (l)
;; 		      			  (list (alexandria:make-keyword (car l))
;; 		      				(car l)))
;; 		      			vallist)))
;; 		      ))
;; 	     (funcall #'self)))))))

(in-package :cl-user)
