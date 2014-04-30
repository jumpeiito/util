(in-package :util)

(declaim (ftype (function (string fixnum) fixnum) gengou->year))

(unless (fboundp 'let1)
  (defmacro let1 (sym val &rest body)
    `(let ((,sym ,val))
       ,@body)))

(defun make-date (nsec sec min hour day month year)
  (local-time:encode-timestamp nsec sec min hour day month year))

(defun make-date-literally (day month year)
  (make-date 0 0 0 0 day month year))

(defmacro datedef (name gen)
  `(defun ,name (date)
     (funcall #',gen date)))
(datedef date-year  local-time:timestamp-year)
(datedef date-month local-time:timestamp-month)
(datedef date-day   local-time:timestamp-day)

(defgeneric normal-date-string (arg))
(defmethod normal-date-string ((s String))
  (normal-date-string (strdt s)))
(defmethod normal-date-string ((dt dt:date-time))
  (format nil "~A/~2,'0d/~2,'0d"
	  (dt:year-of dt)
	  (dt:month-of dt)
	  (dt:day-of dt)))
(defmethod normal-date-string ((lt local-time:timestamp))
  (format nil "~A/~2,'0d/~2,'0d"
	  (date-year  lt)
	  (date-month lt)
	  (date-day   lt)))
(defmethod normal-date-string ((n Number))
  (normal-date-string (write-to-string n)))
(defmethod normal-date-string ((f Float))
  (normal-date-string (truncate f)))
(defmethod normal-date-string ((n Null))
  nil)

(defun normal-date->string (date &key (zero-padding t))
  (if date
      (format nil
	      (if zero-padding "~A/~2,'0d/~2,'0d" "~A/~A/~A")
	      (date-year date)
	      (date-month date)
	      (date-day date))
      nil))

(defun date-string-normalize (date)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if date
      (let ((month (the (or fixnum nil) (date-month date)))
	    (day   (the (or fixnum nil) (date-day date))))
	(with-output-to-string (i)
	  (declare (type stream i))
	  (write (date-year date) :stream i)
	  (write '/ :stream i)
	  (if (> 10 month)
	      (write 0 :stream i))
	  (write month :stream i)
	  (write '/ :stream i)
	  (if (> 10 day)
	      (write 0 :stream i))
	  (write day :stream i)))
      nil))

(defun date->japanese-string (date)
  (format nil "~A年~A月~A日" (date-year date)
	  (date-month date) (date-day date)))

(defun date->japanese-month (date)
  (format nil "~A年~2,'0d月" (date-year date)
	  (date-month date)))

(defmacro rxmatch-case (str &rest clause)
  (declare (string str) (optimize (speed 3) (safety 0)))
  `(cond
     ,@(append (mapcar
  		(lambda (c)
  		  (list `(ppcre:scan ,(car c) ,str)
  			`(multiple-value-bind (all data) (ppcre:scan-to-strings ,(car c) ,str)
  			   (declare (ignore all))
  			   (apply #'(lambda ,(second c) ,(car (last c)))
  				  (coerce data 'list)))))
  		clause)
  	       `((t nil)))))

(defmacro rxmatch-case2 (str &rest clause)
  (optima:match (car clause)
    ((NULL) nil)
    ((LIST* t rest)
     `(progn ,@rest))
    ((LIST* regexp vars rest)
     `(multiple-value-bind (all data)
	  (ppcre:scan-to-strings ,regexp ,str)
	(if all
	    (apply (lambda ,vars ,@rest) (coerce data 'list))
	    (rxmatch-case2 ,str ,@(cdr clause)))))))

(defmacro rxmatch-case-date (str &rest clause)
  (declare (string str) (optimize (speed 3) (safety 0)))
  `(rxmatch-case ,str
		 ,@(mapcar
		    (lambda (xxl)
		      `(,(first xxl) ,(second xxl)
			 (make-date-literally ,@(mapcar
						 (lambda (small)
						   `(read-from-string ,small))
						 (third xxl)))))
		    clause)))

(defvar gengou
  (let1 h (make-hash-table :test #'equal)
	(dolist (i '(("m" . 1867) ("M" . 1867) ("明" . 1867) ("明治" . 1867)
		     ("t" . 1911) ("T" . 1911) ("大" . 1911) ("大正" . 1911) 
		     ("s" . 1925) ("S" . 1925) ("昭" . 1925) ("昭和" . 1925) 
		     ("h" . 1988) ("H" . 1988) ("平" . 1988) ("平成" . 1988)))
	  (setf (gethash (car i) h) (cdr i)))
	h))

(defgeneric gengou->seireki (g yy))
(defmethod  gengou->seireki (g (yy Integer))
  (write-to-string (+ (gethash g gengou) yy)))
(defmethod  gengou->seireki (g (yy String))
  (gengou->seireki g (read-from-string yy)))

(defun gengou->year (g yy)
  (+ (funcall (cond
		((stringp yy)  #'read-from-string)
		((integerp yy) #'identity)
		(t (error "Argument Error"))) yy) (gethash g gengou)))

(defvar english-month-hash
  (alexandria:alist-hash-table
   '(("Jan"		. "1")
     ("Feb"		. "2")
     ("Mar"		. "3")
     ("Apr"		. "4")
     ("May"		. "5")
     ("Jun"		. "6")
     ("Jul"		. "7")
     ("Aug"		. "8")
     ("Sep"		. "9")
     ("Oct"		. "10")
     ("Nov"		. "11")
     ("Dec"		. "12")
     ("January"		. "1")
     ("February"	. "2")
     ("March"		. "3")
     ("April"		. "4")
     ("May"		. "5")
     ("June"		. "6")
     ("July"		. "7")
     ("August"		. "8")
     ("September"	. "9")
     ("October"		. "10")
     ("November"	. "11")
     ("December"	. "12"))
   :test #'equal))

(defun english-month-name (eng)
  (gethash eng english-month-hash 1))

(defun string->date (str)
  (declare (string str) (optimize (speed 3) (safety 0)))
  (the string
    (rxmatch-case-date str
		       ("^(\\d{4})(\\d{2})(\\d{2})$"
			(yyyy mm dd) (dd mm yyyy))
		       ("^(\\d{4})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*(\\d{1,2})[日]*$"
			(yyyy mm dd) (dd mm yyyy))
		       ("^([MTSHmtsh明大昭平])[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\.月][ ]*(\\d{1,2})[日]*$"
			(g yy mm dd) (dd mm (gengou->seireki g yy)))
		       ("^(明治|大正|昭和|平成)[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*(\\d{1,2})[-/\. 日]$"
			(g yy mm dd) (dd mm (gengou->seireki g yy)))
		       ("^(\\d{4})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*$"
			(yyyy mm) ("1" mm yyyy))
		       ("^([MTSHmtsh明大昭平])[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\.月][ ]*"
			(g yy mm) ("1" mm (gengou->seireki g yy)))
		       ("^(明治|大正|昭和|平成)[ ]*(\\d{1,2})[-/\. 年][ ]*(\\d{1,2})[-/\. 月][ ]*"
			(g yy mm) ("1" mm (gengou->seireki g yy)))
		       ("[A-Za-z]+ ([A-Za-z]+) ([0-9]+) 00:00:00 ........ ([0-9]+)"
			(mm dd yyyy) (dd (english-month-name mm) yyyy))
		       ("^(\\d{4})(\\d{2})"
			(yyyy mm dd) ("1" mm yyyy))
		       ("^(\\d{4})-(\\d{2})-(\\d{2}) 00:00:00 .+"
			(yyyy mm dd) (dd mm yyyy)))))

(defmacro irrcase (string &rest clause)
  (let1 pair (take clause 2)
    `(aif (cl-irregsexp:if-match-bind ,(first pair) ,string
				      ,(second pair))
	  it
	  ,(if (drop clause 2)
	       `(irrcase ,string ,@(drop clause 2))
	       nil))))

(defun make-date-literally-lt (day month year)
  (make-date-literally day month year))

(defun strdt (string &key (function #'make-date-literally))
  (declare (type simple-string string)
	   (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (irrcase (the simple-string string)
	   ((year (integer :length 4)) (or #\- #\/ #\. #\  #\年) (* (space))
	    (month (integer :max-length 2)) (or #\- #\/ #\. #\  #\月) (* (space))
	    (day (integer :max-length 2)) (* "日") (last))
	   (funcall function day month year)

	   ((y (integer :length 4)) (m (integer :length 2)) (d (integer :length 2)))
	   (funcall function d m y)

	   ((g (or #\M #\T #\S #\H #\m #\t #\s #\h #\明 #\大 #\昭 #\平))
	    (* (space)) (y (integer :max-length 2)) (* (or #\- #\/ #\. #\  #\年))
	    (* (space)) (m (integer :max-length 2)) (* (or #\- #\/ #\. #\  #\月))
	    (* (space)) (d (integer :max-length 2)) (* (or #\日 #\- #\/ #\. #\ )))
	   (funcall function d m (the fixnum (gengou->year g y)))

	   ((g (or "明治" "大正" "昭和" "平成"))
	    (* (space)) (y (integer :max-length 2)) (or #\- #\/ #\. #\  #\年)
	    (* (space)) (m (integer :max-length 2)) (or #\- #\/ #\. #\  #\月)
	    (* (space)) (d (integer :max-length 2)) (* (or #\日 #\- #\/ #\. #\ )))
	   (funcall function d m (gengou->year g y))

	   ((y (integer :length 4)) (or #\- #\/ #\. #\  #\年) (* (space))
	    (m (integer :max-length 2)) (* (or #\月 #\- #\/ #\. #\ )))
	   (funcall function 1 m y)

	   ((g (or #\M #\T #\S #\H #\m #\t #\s #\h #\明 #\大 #\昭 #\平))
	    (* (space)) (y (integer :max-length 2)) (or #\- #\/ #\. #\  #\年)
	    (* (space)) (m (integer :max-length 2)) (or #\- #\/ #\. #\  #\月))
	   (funcall function 1 m (gengou->year g y))

	   ((g (or "明治" "大正" "昭和" "平成"))
	    (* (space)) (y (integer :max-length 2)) (or #\- #\/ #\. #\  #\年)
	    (* (space)) (m (integer :max-length 2)) (or #\- #\/ #\. #\  #\月))
	   (funcall function 1 m (gengou->year g y))

	   ((y (integer :length 4)) (m (integer :length 2)))
	   (funcall function 1 m y)

	   ((y (integer :length 4)) "-" (m (integer :length 2)) "-" (d (integer :length 2))
	    " 00:00:00 " (+ (string)))
	   (funcall function d m y)))

(defun timestamp-p (obj)
  (if (equal 'local-time:timestamp (type-of obj)) obj nil))

(defgeneric datenumber (d))
(defmethod datenumber ((d LOCAL-TIME:TIMESTAMP))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ (the fixnum (* 100 (the fixnum (date-month d))))
		 (the fixnum (date-day d)))))
(defmethod datenumber ((d DT:DATE-TIME))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ (the fixnum (* 100 (the fixnum (dt:month-of d))))
		 (the fixnum (dt:day-of d)))))

(defgeneric how-old (b1 b2))
(defmethod  how-old ((birth DT:DATE-TIME) (base DT:DATE-TIME))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (- (the fixnum (dt:year-of base)) (the fixnum (dt:year-of birth))
     (if (>= (the fixnum (- (the fixnum (datenumber base))
			    (the fixnum (datenumber birth))))
	     0)
	 0 1)))
(defmethod  how-old ((birth DT:DATE-TIME) (base LOCAL-TIME:TIMESTAMP))
  (how-old birth (dt:make-date (local-time:timestamp-year base)
			       (local-time:timestamp-month base)
			       (local-time:timestamp-day base))))
(defmethod  how-old ((birth DT:DATE-TIME) (base String))
  (how-old birth (strdt base)))
(defmethod  how-old ((birth DT:DATE-TIME) (base Number))
  (how-old birth (strdt (write-to-string base))))
(defmethod  how-old ((birth LOCAL-TIME:TIMESTAMP) (base LOCAL-TIME:TIMESTAMP))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (- (the fixnum (date-year base)) (the fixnum (date-year birth))
     (if (>= (the fixnum (- (the fixnum (datenumber base))
			    (the fixnum (datenumber birth))))
	     0)
	 0 1)))
(defmethod  how-old ((birth LOCAL-TIME:TIMESTAMP) (base DT:DATE-TIME))
  (how-old (dt:make-date (local-time:timestamp-year birth)
			 (local-time:timestamp-month birth)
			 (local-time:timestamp-day birth))
	   base))
(defmethod  how-old ((birth LOCAL-TIME:TIMESTAMP) (base String))
  (how-old birth (strdt base)))
(defmethod  how-old ((birth LOCAL-TIME:TIMESTAMP) (base Number))
  (how-old birth (strdt (write-to-string base))))
(defmethod  how-old ((birth String) (base String))
  (how-old (strdt birth) (strdt base)))
(defmethod  how-old ((birth String) (base Number))
  (how-old (strdt birth) (strdt (write-to-string base))))
(defmethod  how-old ((birth String) (base DT:DATE-TIME))
  (how-old (strdt birth) base))
(defmethod  how-old ((birth String) (base LOCAL-TIME:TIMESTAMP))
  (how-old (strdt birth) base))
(defmethod  how-old ((birth Number) (base Number))
  (how-old (write-to-string birth) (write-to-string base)))
(defmethod  how-old ((birth Number) base)
  (how-old (write-to-string birth) base))
(defmethod  how-old ((n Null) s)
  0)

(defun nendo (n)
  (values (make-date-literally 1 4 n)
	  (make-date-literally 31 3 (+ n 1))))

(defgeneric nendo-year (d))
(defmethod nendo-year ((d local-time:timestamp))
  (- (date-year d) (if (<= (date-month d) 3) 1 0)))
(defmethod nendo-year ((dt dt:date-time))
  (- (dt:year-of dt) (if (<= (dt:month-of dt) 3) 1 0)))
(defmethod nendo-year ((n null))
  nil)
(defmethod nendo-year ((s string))
  (aif (strdt s) (nendo-year (the local-time:timestamp it)) nil))

(defun nendo= (d1 d2)
  (eq (nendo-year d1) (nendo-year d2)))

(defgeneric date-8 (d))
(defmethod  date-8 ((d LOCAL-TIME:TIMESTAMP))
  (format nil "~4,'0d~2,'0d~2,'0d"
	  (date-year  d)
	  (date-month d)
	  (date-day   d)))
(defmethod  date-8 ((d DT:DATE-TIME))
  (format nil "~4,'0d~2,'0d~2,'0d"
	  (dt:year-of  d)
	  (dt:month-of d)
	  (dt:day-of   d)))
(defmethod  date-8 ((s String))
  (date-8 (strdt s)))

(defun today-8 ()
  (date-8 (local-time:today)))

(defmacro date-comparison-def (name function)
  `(defun ,name (date1 date2)
     (,function (local-time:timestamp-difference date1 date2) 0)))

(date-comparison-def date<  <)
(date-comparison-def date>  >)
(date-comparison-def date<= <=)
(date-comparison-def date>= >=)
(date-comparison-def date=  =)


(defun date-string-normalization (date-string)
  (normal-date->string (string->date date-string)))

(defun nendo-end (year &key (string nil))
  (let1 date (make-date-literally 31 3 (+ 1 year))
    (funcall (if string
		 #'normal-date->string
		 #'identity)
	     date)))

(defun nendo-month-list (&key (format nil))
  (iter (for month :from 4 :to 15)
	(for m = (if (> month 12) (- month 12) month))
	(collect (if format
		     (format nil format m)
		     m))))

(defun now-string ()
  (let1 now (local-time:now)
    (format nil "~A~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
	    (local-time:timestamp-year now)
	    (local-time:timestamp-month now)
	    (local-time:timestamp-day now)
	    (+ (local-time:timestamp-hour now) 9)
	    (local-time:timestamp-minute now)
	    (local-time:timestamp-second now))))

(defgeneric to-string (arg))

(defmethod to-string ((dt dt:date-time))
  (format nil "~A/~2,'0d/~2,'0d"
	  (dt:year-of dt) (dt:month-of dt) (dt:day-of dt)))

(defmethod to-string ((lt local-time:timestamp))
  (format nil "~A/~2,'0d/~2,'0d"
	  (local-time:timestamp-year lt)
	  (local-time:timestamp-month lt)
	  (local-time:timestamp-day lt)))

(defmethod to-string ((s string))
  s)

(defmethod to-string ((n null))
  "")

(defun this-year-kanji ()
  (format nil "~A年" (dt:year-of (dt:now))))

(defun this-month-kanji (&key (padding t))
  (let ((now (dt:now)))
    (format nil (if padding "~A年~2,'0d月" "~A年~A月")
	    (dt:year-of now)
	    (dt:month-of now))))

(defun this-date-kanji (&key (padding t))
  (let ((now (dt:now)))
    (format nil (if padding "~A年~2,'0d月~2,'0d日" "~A年~A月~A日")
	    (dt:year-of now)
	    (dt:month-of now)
	    (dt:day-of now))))

(in-package :cl-user)
