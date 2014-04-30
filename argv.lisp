(in-package :util)
;; (optcase
;;  ("-a" "--hoge")    string-case
;;  ("-v" "--verbose") string-occur
;;  :else              string-help)

;; (cond
;;   ((or (equal (second *posix-argv*) "-a")
;;        (equal (second *posix-argv*) "--hoge"))
;;    (apply (symbol-function string-case) w(nthcdr 3 *posix-argv*)))
;;   ..
;;   (t   (apply (symbol-function string-help) case)))
;; (defparameter argv cl-user::*posix-argv*)

(defmacro optcase (&rest clause)
  `(cond
     ,@(mapcar
       (lambda (pair)
	 `(,(if (listp (car pair))
		`(or ,@(mapcar
			(lambda (smallpair) `(equal (second cl-user::*posix-argv*) ,smallpair))
			(car pair)))
		(if (eq :else (car pair))
		    t
		    `(equal (second cl-user::*posix-argv*) ,(car pair))))
	    (apply ,(if (listp (second pair))
			(second pair)
			`(symbol-function ,(second pair)))
		   (nthcdr 2 cl-user::*posix-argv*))))
       (group clause 2))))

(in-package :cl-user)
