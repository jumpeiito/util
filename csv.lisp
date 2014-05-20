;; -*- coding:utf-8 -*-
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

(defun csv-read (ip &key (to nil) (separator #\,))
  (labels ((scanning () (read-char ip nil nil nil)))
      (labels ((in (c col line linec r)
		 (if (and c (or (not to) (> to linec)))
		     (char-case c
				(#\Newline
				 (in (scanning) '() '() (1+ linec) (cons (reverse (cons (coerce (reverse col) 'string) line)) r)))
				(separator
				 (in (scanning) '() (cons (coerce (reverse col) 'string) line) linec r))
				(#\Return
				 (in (scanning) col line linec r))
				(#\"
				 (in (scanning) col line linec r))
				(t
				 (in (scanning) (cons c col) line linec r)))
		     (if line
			 (reverse (cons (reverse line) r))
			 (reverse r)))))
	(in (read-char ip nil nil nil) '() '() 0 '()))))

(defun csv-string-read (string)
  (with-input-from-string (i string)
    (csv-read i)))

(declaim (inline get-output-stream-string char-equal))

(defun %read-iter (ip func &key (to nil) (separator #\,))
  ;; (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
  ;; 	   (type stream ip)
  ;; 	   (type (or nil fixnum) to))
  (symbol-macrolet ((scan (read-char ip nil nil nil)))
    (labels ((in (c col line counter)
	       ;; (declare (dynamic-extent c col line))
	       (if (and c (or (not to) (> (the fixnum to)
					  (the fixnum counter))))
		   (char-case c
			      (separator
			       (let ((o (get-output-stream-string col)))
				 (in scan col (cons o line) counter)))
			      (#\Return
			       (in scan col line counter))
			      (#\Newline
			       (let* ((o (get-output-stream-string col)))
				 (funcall func (reverse (cons o line)))
				 (in scan col '() (incf (the integer counter)))))
			      (#\"
			       (in scan col line counter))
			      (t
			       (progn
				 (write-char c col)
				 (in scan col line counter))))
		   nil)))
    (in scan (make-string-output-stream) '() 0))))

(defun %read-map-filter (ip func pred &key (to nil) (separator #\,))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
	   (type stream ip))
  (symbol-macrolet ((scan (read-char ip nil nil nil)))
    (labels ((in (c col line r)
	       (declare (dynamic-extent c col line r))
	       (symbol-macrolet ((acc (cons o line))
				 (rev (reverse (cons o line))))
		 (if (and c (or (not to) (> (the fixnum to)
					    (the fixnum (length (the list r))))))
		     (char-case c
				(separator
				 (let ((o (get-output-stream-string col)))
				   (declare (dynamic-extent o))
				   (in scan col acc r)))
				(#\Newline
				 (in scan col '()
				     (let* ((o (get-output-stream-string col))
					    (_rev rev))
				       (declare (dynamic-extent o _rev))
				       (if (funcall pred _rev)
					   (cons (funcall func _rev) r)
					   r))))
				(#\Return
				 (in scan col line r))
				(#\"
				 (in scan col line r))
				(t
				 (progn
				   (write-char c col)
				   (in scan col line r))))
		     (let ((o (get-output-stream-string col)))
		       (declare (dynamic-extent o))
		       (if (and (string-null o) (not line))
			   (reverse r)
			   (let ((l (funcall func rev)))
		       	    (if (funcall pred l)
		       		(reverse (cons l r))
		       		(reverse r)))))))))
    (in scan (make-string-output-stream) '() '()))))

(defun %read-filter-map (ip func pred &key (to nil) (separator #\,))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type stream ip))
  (symbol-macrolet ((scan (read-char ip nil nil nil)))
    (labels ((in (c col line r)
	       (symbol-macrolet ((acc (cons o line))
				 (rev (reverse (cons o line))))
		 (if (and c (or (not to) (> (the fixnum to)
					    (the fixnum (length (the list r))))))
		     (char-case c
				(#\Newline
				 (in scan col '()
				     (let* ((o (get-output-stream-string col))
					    (l (funcall func rev)))
				       (if (funcall pred l) (cons l r) r))))
				(separator
				 (let ((o (get-output-stream-string col)))
				   (in scan col acc r)))
				(#\Return
				 (in scan col line r))
				(#\"
				 (in scan col line r))
				(t
				 (progn
				   (write-char c col)
				   (in scan col line r))))
		     (let ((o (get-output-stream-string col)))
		       (optima:match (list o line)
			 ((LIST "" nil) (reverse r))
			 (_
			  (let ((l (funcall func rev)))
			    (if (funcall pred l)
				(reverse (cons l r))
				(reverse r))))))))))
    (in scan (make-string-output-stream) '() '()))))

(defun csv-read-to-list
    (filename &key (code #+sbcl :UTF8 #+clisp charset:utf-8)
		(to nil) (separator #\,))
  (declare (optimize speed))
  (with-open-file (ip filename :external-format code)
    (%read-filter-map ip #'identity #'identity :to to :separator separator)))

(defun csv-read-iter
    (filename func &key (code #+sbcl :UTF8 #+clisp charset:utf-8)
		(to nil) (separator #\,))
  (with-open-file (ip filename :external-format code)
    (%read-iter ip func :to to :separator separator)))

(defun csv-read-filter-map
    (filename func pred &key (code #+sbcl :UTF8 #+clisp charset:utf-8)
		(to nil) (separator #\,))
  (with-open-file (ip filename :external-format code)
    (%read-filter-map ip func pred :to to :separator separator)))

(defun csv-read-map-filter
    (filename func pred &key (code #+sbcl :UTF8 #+clisp charset:utf-8)
		(to nil) (separator #\,))
  (with-open-file (ip filename :external-format code)
    (%read-map-filter ip func pred :to to :separator separator)))

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

(in-package :cl-user)
