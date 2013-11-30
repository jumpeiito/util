;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   ;; (load-with-os "test.fasl")
;;   )
(in-package :util)

(defparameter alph "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defparameter alphlist (coerce alph 'list))

(defun push! (obj vec)
  (declare (vector vec) (optimize (speed 3) (safety 0) (debug 0)))
  (vector-push-extend obj vec)
  (the vector vec))

(defmacro char-case (char-sym &rest clause)
  `(cond
     ,@(mapcar
	(lambda (cl)
	  (if (eq t (car cl))
	      `(t ,(second cl))
	      `((char-equal (the character ,char-sym) (the character ,(car cl)))
		,(second cl))))
	clause)))

(defun ary ()
  (make-array 0 :adjustable t :fill-pointer t))

(defun csv-read-core (ip &key (to nil) (separator #\,))
  (labels ((fn (c col line r)
	     (if (and c (or (not to) (> to (length r))))
		 (char-case c
			    (#\Newline
			     (fn (the character (read-char ip nil nil nil))
				 (the vector (ary))
				 (the vector (ary))
				 (the vector (push! (the vector (push! (the vector (coerce col 'string)) (the vector line))) r))))
			    (separator
			     (fn (the character (read-char ip nil nil nil))
				 (the vector (ary))
				 (the vector (push! (the vector (coerce col 'string)) (the vector line)))
				 (the vector r)))
			    (#\Return
			     (fn (the character (read-char ip nil nil nil))
				 (the vector col)
				 (the vector line)
				 (the vector r)))
			    (#\"
			     (fn (the character (read-char ip nil nil nil))
				 (the vector col)
				 (the vector line)
				 (the vector r)))
			    (t
			     (fn (the character (read-char ip nil nil nil))
				 (the vector (push! c col))
				 (the vector line)
				 (the vector r))))
		 r)))
    (the vector (fn (read-char ip nil nil nil)
		    (the vector (ary))
		    (the vector (ary))
		    (the vector (ary))))))

;; (defun csv-read (filename &key (code :UTF8) (to nil))
;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
;;   (with-open-file (ip filename :external-format (list code))
;;     (csv-read-core ip :to to)))

;; #P"f:/kcsv2/00263129_FKAC522_20130619_011.csv"

(defun csv-read (ip &key (to nil) (separator #\,))
  (labels ((scanning () (read-char ip nil nil nil)))
      (labels ((in (c col line r)
		 (if (and c (or (not to) (> to (length r))))
		     (char-case c
				(#\Newline
				 (in (scanning) '() '() (cons (reverse (cons (coerce (reverse col) 'string) line)) r)))
				(separator
				 (in (scanning) '() (cons (coerce (reverse col) 'string) line) r))
				(#\Return
				 (in (scanning) col line r))
				(#\"
				 (in (scanning) col line r))
				(t
				 (in (scanning) (cons c col) line r)))
		     (reverse r))))
	(in (read-char ip nil nil nil) '() '() '()))))

(defun csv-read-to-list
    (filename &key (code #+sbcl :UTF8 #+clisp charset:utf-8)
		(to nil) (separator #\,))
  ;; (with-open-file (ip filename :external-format (list code))
  (with-open-file (ip filename :external-format code)
    (csv-read ip :to to :separator separator)))

(defun last1 (list) (car (last list)))

(defun jrc-joint (l1 l2)
  `(,@(butlast l1) ,(str+ (last1 l1) (car l2)) ,@(cdr l2)))

(defun joint-corruption-repair (csvdata)
  (let1 size (length (car csvdata))
    (labels ((in (subl r)
	       (if (null subl)
		   (reverse r)
		   (if (eq (length (car subl)) size)
		       (in (cdr subl) (cons (car subl) r))
		       (in (cddr subl) (cons (jrc-joint (first subl) (second subl)) r))))))
      (in csvdata '()))))

(defun csv-read-to-list-joint-repair (filename &key (code :UTF8) (to nil))
  (joint-corruption-repair
   (csv-read-to-list filename :code code :to to)))

(defun excel-column-base (num)
  (declare (fixnum num) (optimize (speed 3) (safety 0) (debug 0)))
  (if (< num 26)
      (the list (list (elt alphlist num)))
      (cons (elt alphlist (- (truncate (/ num 26)) 1))
	    (excel-column-base (mod num 26)))))

(defun excel-column (num)
  (declare (fixnum num) (optimize (speed 3) (safety 0)))
  (str+ (the list  (excel-column-base num))))

(defun excel-data (2dary)
  (declare (vector 2dary) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((col 0) (row 0))
    (setf col 0 row 3)
    (vector-map
     (lambda (v)
       (the vector
	 (prog1
	     (vector-map
	      (lambda (d)
		(prog1
		    (the vector (vector (str+ (excel-column col)
					      (write-to-string row))
					d))
		  (setf col (+ 1 col))))
	      v)
	   (setf col 0)
	   (setf row (+ row 1)))))
     (the vector 2dary))))

(defun power (num digit)
  (labels ((sub (d acc)
	     (if (eq d 0)
		 acc
		 (funcall #'sub (- d 1) (* num acc)))))
    (funcall #'sub digit 1)))

(defun divide (a b)
  (values (truncate (/ a b)) (mod a b)))

;; (defun test (target num)
;;   (labels ((sub (subt num r)
;; 	     (mvbind ())))))

(defun excel-row->number (excel-row)
  (declare (string excel-row) (optimize (speed 3) (safety 0)))
  (labels ((subfn (subl counter acc)
	     (if (null subl)
		 acc
		 (funcall #'subfn (cdr subl) (+ 1 counter)
			  (the fixnum
			    (+ (* (the fixnum (power 26 counter))
				  (the fixnum (+ 1 (the fixnum (array-index (car subl) alph)))))
			     acc))))))
    (funcall #'subfn (the list (reverse (coerce excel-row 'list)))
	     0 0)))

(defun csv-read-array (filename row-size column-size &key (code :SJIS))
  ;; (declare (type integer row-size column-size)
  ;; 	   (optimize (speed 3) (safety 0) (debug 0)))
  (with-open-file (file filename :direction :input :external-format code)
    (iter (with array = (make-array `(,row-size ,column-size)))
	  (with row   = 0)
	  (with col   = 0)
	  (with charl = '())
	  (for c = (read-char file nil nil nil))
	  (unless c (leave array))
	  (cond
	    ((char-equal c #\Newline)
	     (setf (aref array row col) (coerce (reverse charl) 'string)
	  	   charl nil
	  	   row (+ row 1)
	  	   col 0))
	    ((char-equal c #\Return)
	     (next-iteration))
	    ((char-equal c #\")
	     (next-iteration))
	    ((char-equal c #\,)
	     (setf (aref array row col) (coerce (reverse charl) 'string)
	  	   charl nil
	  	   col (+ col 1)))
	    (t
	     (setf charl (cons c charl)))))))
;; #P"f:/kcsv2/00263129_FKAC522_20130618_050.csv"
(in-package :cl-user)
