(in-package :util)

;; (unless (fboundp 'str+)
;;   (defmacro str+ (&rest args)
;;     `(concatenate 'string ,@args)))

(defmacro dbind (&rest arg)
  `(destructuring-bind ,@arg))

(defmacro mvbind (&rest arg)
  `(multiple-value-bind ,@arg))

(defmacro let1 (sym val &rest body)
  `(let ((,sym ,val))
     ,@body))

(defmacro rlet1 (sym val &body clauses)
  `(let1 ,sym ,val
     ,@clauses
     ,sym))

(defmacro while (test &rest body)
  `(loop
      :while ,test
      :do    ,@body))

(defmacro rvlet1 (sym val &body clauses)
  `(let1 ,sym ,val
     ,@clauses
     (reverse ,sym)))

(defmacro if-let1 (sym val ifbody &rest elsebody)
  `(if ,val
       (let ((,sym ,val)) ,ifbody)
       ,@elsebody))

;; (defun os-expand (pathname)
;;   (str+ (if (asdf::os-windows-p)
;; 	    "f:/"
;; 	  "/media/DE13-7421/")
;; 	pathname))

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

(defun fn (function &rest args)
  (apply function args))

(defmacro ^ (var &rest args)
  `(lambda ,var ,@args))

(defmacro cut (&rest parameter)
  (if (null parameter)
      `(lambda ())
      (let ((cutarg (gensym)))
	(labels ((%inner (l r)
		   (if (null l)
		       (reverse r)
		       (funcall #'%inner (cdr l)
				(cond
				  ((atom (car l))
				   (if (eq (car l) '<>)
				       (cons cutarg r)
				       (cons (car l) r)))
				  ((listp (car l))
				   (cons (funcall #'%inner (car l) '()) r)))))))
	  `(lambda (,cutarg) ,(funcall #'%inner parameter '()))))))

(defmacro aif (test thenbody &optional elsebody)
  ;; `(if ,test
  ;; 	 (let ((it ,test)) ,thenbody)
  ;; 	 ,elsebody)
  `(let ((it ,test))
     (if it ,thenbody ,elsebody)))

(defmacro awhen (test &rest body)
  `(let ((it ,test))
     ,@body))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro & (&rest args)
  (reduce #'list args :from-end t))

(defmacro any (list)
  `(or ,@list))

;; (defmacro acond (&rest clause)
;;   (if clause
;;       `(let1 it ,(caar clause)
;; 	 (if it
;; 	     (progn ,@(cdar clause))
;; 	     (acond ,@(cdr clause))))
;;       nil))

(defmacro with-obj-slots (instance &body body)
  `(with-slots ,(mapcar #'c2mop:slot-definition-name
			(c2mop::class-slots (class-of instance))) ,instance
     ,@body))


(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
       #'(lambda (stream char1 char2)
           (apply fn
                  (read-delimited-list right stream t))))))

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(defdelim #\[ #\] (&rest args)
  (if (keywordp (car args))
      (let ((function (gensym)))
	`(lambda (,function) (funcall ,function ,@args)))
      ;; (funcall ,(symbol-function (car args))
      ;; 	       ,@(cdr args))
      `(funcall ,@args)))


(defun percent (number divisor)
  (format nil
	  "~A%" (float (/ (round (* (float (/ number divisor)) 1000)) 10))))

(defun compact-str (number-list)
  (labels ((in (subl flag start r)
	     (optima:match subl
	       ((type NULL)
		(cdr
		 (reverse
		  (if (eq flag 1)
		      (cons start r)
		      (cons (list start (1- (+ start flag))) r)))))
	       ((LIST* car cdr)
		(if (eq car (+ flag start))
		    (in cdr (1+ flag) start r)
		    (if (eq flag 1)
			(in cdr 1 car (cons start r))
			(in cdr 1 car (cons (list start (1- (+ start flag))) r))))))))
    (in number-list 1 (car number-list) nil)))

(in-package :cl-user)
