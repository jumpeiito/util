(in-package :excel)

(defconstant xldown		-4121)
(defconstant xltoright		-4161)
(defconstant xlcalcmanual	-4135)
(defconstant xlcalcauto		-4105)
(defconstant xlblack		1)
(defconstant xlwhite		2)
(defconstant xlred		3)
(defconstant xlgreenyellow	4)
(defconstant xlblue		5)
(defconstant xlyellow		6)
(defconstant xlpink		7)
(defconstant xlskyblue		8)
(defconstant xlbrown		9)
(defconstant xlgray25		15)
(defconstant xlgray50		16)
(defconstant xlcenter		-4108)
(defconstant xlleft		-4131)
(defconstant xlright		-4152)
(defconstant xlvtop		-4160)
(defconstant xlvcenter		-4108)
(defconstant xlvbottom		-4107)
(defconstant xlB4		12)
(defconstant xlB5		13)
(defconstant xlA4		9)
(defconstant xlA3		8)
(defconstant xlPortrait		1)
(defconstant xlLandscape	2)
#|
51 = xlOpenXMLWorkbook (without macro's in 2007, xlsx)
52 = xlOpenXMLWorkbookMacroEnabled (with or without macro's in 2007, xlsm)
50 = xlExcel12 (Excel Binary Workbook in 2007 with or without macro’s, xlsb)
56 = xlExcel8 (97-2003 format in Excel 2007, xls)
|#
(defconstant xlXLSX		51)
(defconstant xlXLSM		52)
(defconstant xlXLSB		50)
(defconstant xlXLS		56)

(defmacro with-excel
    ((excel-application &key (visible t) (quit t) (debugger nil)) &rest body)
  `(cl-win32ole-sys::with-co-initialize
     (let ((,excel-application (create-object "Excel.Application")))
       (setf (slot-value ,excel-application :visible) ,visible
	     (slot-value ,excel-application :displayalerts) nil)
       (handler-case
	   (prog1
	       (progn ,@body)
	     ,(if quit
		  `(progn
		     (ole ,excel-application :quit)
		     (setf ,excel-application nil))))
	 (error (e)
	   (print e)
	   (ole ,excel-application :quit)
	   (setf ,excel-application nil)
	   ,(if debugger
		`(with-simple-restart (continue "Excel")
		   (invoke-debugger e))))))))

(defmacro with-excel-book
    ((excel book-variable pathname &key (close t) (debugger nil)) &rest body)
  `(let ((,book-variable (ole ,excel :workbooks :open ,pathname)))
     (handler-case
	 (prog1
	     (progn ,@body)
	   ,(if close
		`(ole ,book-variable :close)))
       (error (e)
	 (print e)
	 (ole ,book-variable :close)
	 ,(if debugger
	      `(with-simple-restart (continue "Excel Book")
		(invoke-debugger e)))))))

(defmacro ole-value (&rest args)
  `(car (slot-value (ole ,@args) :value)))

(defun lastrow (sheet-dispatch &key (y 1) (x 1))
  (ole sheet-dispatch :cells y x :end xldown :row))

(defun lastcol (sheet-dispatch &key (y 1) (x 1))
  (ole sheet-dispatch :cells y x :end xltoright :column))

(defun range (sheet-dispatch start-col start-row end-col end-row)
  (ole sheet-dispatch :range (format nil "~A~A:~A~A"
				     start-col start-row end-col end-row)))

(defun cells (sheet-dispatch col row)
  (ole sheet-dispatch :cells col row))

(defun cell-value! (sheet-dispatch col row value)
  (setf (slot-value (cells sheet-dispatch col row) 'value)
	value))

(defun get-color (dispatch type)
  (slot-value (slot-value dispatch type) 'color))

(defun set-color (dispatch type color)
  (setf (slot-value (slot-value dispatch type) 'color)
	color))

(defparameter alphhash
  (iter (with hash = (make-hash-table :test 'equal))
	(for char :in-vector util::alph)
	(for c :upfrom 1)
	(setf (gethash char hash) c)
	(finally (return hash))))

(defun col-to-number (row)
  (iter (for c :in-vector (reverse (string-upcase row)))
	(for exp :upfrom 0)
	(sum (* (expt 26 exp) (gethash c alphhash)))))

(defun number-to-col-compact (list)
  (remove-if
   (lambda (n) (eq n 0))
   list))

(defun number-to-col-alphabetize (list)
  (coerce
   (mapcar (lambda (n) (aref util::alph (1- n)))
	   list)
   'string))

(defun number-to-col (number)
  (labels ((in (subn r)
  	     (if (> 26 subn)
  		 (if (>= 26 number)
		     (format nil "~A" (aref util::alph (1- number)))
		     (number-to-col-alphabetize
		      (number-to-col-compact
		       (cons subn r))))
		 (let ((modulo (mod subn 26)))
		   (if (eq modulo 0)
		       (in (1- (floor (/ subn 26)))
			   (cons 26 r))
		       (in (floor (/ subn 26)) (cons (mod subn 26) r)))))))
    (in number nil))
  )

;; (range sheet (:a 10))
;; (range sheet (:a 10) (:n 200))
;; => (ole sheet :range "A10:N200")
;; (let ((n :b))
;;   (range sheet (:a 10) (n 200)))
;; => (ole sheet :range "A10:B200")
;; (range sheet :a)
;; => (ole sheet :range "A:A")
;; (range sheet 1)
;; => (ole sheet :range "1:1")
;; (range sheet :a :b)
;; => (ole sheet :range "A:B")
(defmacro range (sheet start &optional finish)
  (optima:match start
    ((list sx sy)
     (if finish
	 (destructuring-bind (ex ey) finish
	   `(cl-win32ole:ole ,sheet :range (format nil "~A~A:~A~A" ,sx ,sy ,ex ,ey)))
	 `(cl-win32ole:ole ,sheet :cells ,sy (col-to-number (format nil "~A" ,sx)))))
    (sym
     `(cl-win32ole:ole ,sheet
		       :range ,(if finish
				   `(format nil "~A:~A" ,sym ,finish)
				   `(format nil "~A:~:*~A" ,sym))))))

(defmacro value (sheet start &optional finish)
  `(slot-value (range ,sheet ,start ,finish) :Value))

(defmacro value! (sheet start &rest args)
  (optima:match args
    ((list end val)
     `(setf (slot-value (range ,sheet ,start ,end) :Value) ,val))
    ((list val)
     `(setf (slot-value (range ,sheet ,start) :Value) ,val))))

;; (set-colorindex sheet (:a 1) (:n 200) :interior excel::xlred)
;; => "A1:N200"までの背景色を赤にする。
;; (set-colorindex sheet (:a 1) :font excel::xlred)
;; => "A1"の文字色を赤にする。
(defmacro set-%color (sheet symbol &rest args)
  (optima:match args
    ((list start end type color)
     `(setf (slot-value (slot-value (range ,sheet ,start ,end) ,type)
			,symbol)
	    ,color))
    ((list start type color)
     `(setf (slot-value (slot-value (range ,sheet ,start) ,type)
			,symbol)
	    ,color))))

(defmacro set-colorindex (sheet &rest args)
  `(set-%color ,sheet :ColorIndex ,@args))

(defmacro set-color (sheet &rest args)
  `(set-%color ,sheet :Color ,@args))

;;; (set-column-width sheet (:a :o) vector)
(defun column-expand (start end)
  (iter (for i :from (col-to-number start) :to (col-to-number end))
	(collect i)))

;; (set-column-width sheet (:a :c) vec)

(defmacro set-colwidth (sheet-dispatch width &key (from :a))
  `(iter (for w :in-vector ,width)
	 (for col :upfrom (col-to-number ,from))
	 (setf (slot-value (ole ,sheet-dispatch :range (format nil "~A:~:*~A" (number-to-col col)))
			   :ColumnWidth)
	       w)))

(defmacro set-column-width (sheet-dispatch column width)
  (format t "set-column-width is obsolete. Use set-colwidth.")
  (optima:match column
    ((list start end)
     (let1 columns (column-expand start end)
       (let ((sym     (gensym))
	     (counter (gensym)))
	 `(if (eq ,(length columns) (length ,width))
	      (iter (for ,sym :in ',columns)
		    (for ,counter :upfrom 0)
		    (setf (slot-value ;; (range ,sheet-dispatch (col-to-number sym))
			   (cl-win32ole:ole ,sheet-dispatch :range (format nil "~A:~:*~A" (number-to-col ,sym)))
				      :ColumnWidth)
			  (svref ,width ,counter)))
	      (error "列数と指定された幅の数が合いません。")))))
    (symbol
     `(setf (slot-value (range ,sheet-dispatch ,symbol)
			:ColumnWidth)
	    (svref ,width 0)))))

(defmacro set-width (sheet &rest args)
  (optima:match args
    ((list start end width)
     `(setf (slot-value (range ,sheet ,start ,end) :ColumnWidth)
	    ,width))
    ((list start width)
     `(setf (slot-value (range ,sheet ,start) :ColumnWidth)
	    ,width))))

(defmacro set-format (sheet &rest args)
  (optima:match args
    ((list start end formatta)
     `(setf (slot-value (range ,sheet ,start ,end) :NumberFormatLocal)
	    ,formatta))
    ((list start formatta)
     `(setf (slot-value (range ,sheet ,start) :NumberFormatLocal)
	    ,formatta))))

(defmacro set-fontname (sheet &rest args)
  (optima:match args
    ((list start end fontname)
     `(setf (slot-value (slot-value (range ,sheet ,start ,end) :font) :name)
	    ,fontname))
    ((list start fontname)
     `(setf (slot-value (slot-value (range ,sheet ,start) :font) :name)
	    ,fontname))))

;; (defun set-alignment (dispatch type value)
;;   (setf (slot-value (ole dispatch)
;; 		    (case type
;; 		      (:vertical   'verticalAlignment)
;; 		      (:horizontal 'horizontalAlignment)
;; 		      (t (error 'simple-error))))
;; 	value))
(defmacro set-alignment (sheet &rest args)
  (optima:match args
    ((list start end type align)
     `(setf (slot-value (range ,sheet ,start ,end) ,type)
	    ,align))
    ((list start type align)
     `(setf (slot-value (range ,sheet ,start) ,type)
	    ,align))))

(defmacro set-column-alignment (sheet-dispatch column alignment &key (type :horizontalalignment))
  (optima:match column
    ((list start end)
     (let1 columns (column-expand start end)
       (let ((sym     (gensym))
	     (counter (gensym)))
	 `(if (eq ,(length columns) (length ,alignment))
	      (iter (for ,sym :in ',columns)
		    (for ,counter :upfrom 0)
		    (setf (slot-value (cl-win32ole:ole ,sheet-dispatch :range (format nil "~A:~:*~A" (number-to-col ,sym)))
				      ,type)
			  (svref ,alignment ,counter)))
	      (error "列数と指定された幅の数が合いません。")))))
    (symbol
     `(setf (slot-value (range ,sheet-dispatch ,symbol)
			,type)
	    (svref ,alignment 0)))))

(defun set-number-format-local (dispatch value)
  (setf (slot-value dispatch 'NumberFormatLocal)
	value))

(defun borders (sheet-dispatch start-row start-col end-row end-col)
  (warn "excel::borders is obsolete. use excel::border")
  (setf (slot-value (ole sheet-dispatch :range (format nil "~A~A:~A~A" start-row start-col end-row end-col) :borders)
		    'LineStyle)
	1))

(defmacro border (sheet start &optional end)
  `(setf (slot-value (slot-value (range ,sheet ,start ,end) :borders)
		     :linestyle)
	 1))

;; (defparameter x 
;;   (cl-win32ole-sys::with-co-initialize
;;     (let ((m (create-object "InternetExplorer.Application")))
;;       ;; (ole m :Quit)
;;       (setf (slot-value m 'visible) t)
;;       (ole m :navigate "http://10.10.1.102/OCIA/KWlgin/KWlginView.do")
;;       (sleep 3)
;;       (let ((in (ole m :document :getElementById "userID"))
;; 	    (pw (ole m :document :getElementById "pwd"))
;; 	    (bt (ole m :document :getElementById "forward_login")))
;; 	(setf (slot-value in 'value) "ken00263129006")
;; 	(setf (slot-value pw 'value) "kene0002")
;; 	(ole bt :click)
;; 	(sleep 3)
;; 	))))


;; (setf (slot-value (slot-value sh 'pagesetup) 'printarea)
;;       (format nil "A2:S~A" lastrow))
;; (setf (slot-value (slot-value sh 'pagesetup) 'Orientation) ;; 向き xlLandscape
;;       2)
;; (setf (slot-value (slot-value sh 'pagesetup) 'papersize) ;; 用紙 xlpaperb4
;;       12)
;; (setf (slot-value (slot-value sh 'pagesetup) 'PrintTitleRows) "$2:$2")
;; (setf (slot-value (slot-value sh 'pagesetup) 'zoom) nil)
;; (setf (slot-value (slot-value sh 'pagesetup) 'fittopagestall) nil)
;; (setf (slot-value (slot-value sh 'pagesetup) 'FitToPagesWide) 1)
;; (setf (slot-value (slot-value sh 'pagesetup) 'centerfooter)
;;       "&18&\"Palatino Linotype\"&P/&N")
;; (setf (slot-value (slot-value sh 'pagesetup) 'leftheader)
;;       (format nil "&\"HGP明朝B\"&36~A支部" (kensin::long-shibu shibu)))

(defmacro page-setup (sheet-dispatch &rest args)
  `(progn
     ,@(mapcar
	(lambda (list)
	  (destructuring-bind (keyword value) list
	    `(setf (slot-value (slot-value ,sheet-dispatch 'pagesetup)
			       ',(intern (string keyword)))
		   ,value)))
	(util::group args 2))))

(defun true-save-name (pathname type)
  (util::windows-dir-sep
   (namestring
    (make-pathname :defaults pathname
		   :type (string-downcase (format nil "~A" type))))))

(defun sheet-name (sheet name)
  (setf (slot-value (cl-win32ole:ole sheet) :name) name))

(defmacro save-book (book pathname type)
  `(cl-win32ole:ole ,book
		    :SaveAs
		    (true-save-name ,pathname ,type)
		    ,(case type
			   (:xlsx	xlXLSX)
			   (:xlsm	xlXLSM)
			   (:xlsb	xlXLSB)
			   (:xls	xlXLS)
			   (t	xlXLSX))))

(defun decide-range (value &key (start-row 1) (start-column 1))
  (let ((h (length value))
	(w (apply #'max (mapcar #'length value))))
    (format nil "~A~A:~A~A"
	    (excel::number-to-col start-column)
	    start-row
	    (excel::number-to-col (1- (+ w start-column)))
	    (1- (+ h start-row)))))

(defun decide-range-value (sheet value &key (start-row 1) (start-column 1))
  (setf (slot-value (ole sheet :range (decide-range value
						    :start-row start-row
						    :start-column start-column)) :value)
	value))

(defun decide-range-borders (sheet value &key (start-row 1) (start-column 1))
  (setf (slot-value
	 (slot-value (ole sheet :range (decide-range value
						     :start-row start-row
						     :start-column start-column))
		     :borders)
	 :LineStyle)
	1))

(defun excel->lisp (2dl)
  (mapcar (lambda (line)
	    (mapcar (lambda (el)
		      (etypecase el
			(dt:date-time (util::to-string el))
			(float        (truncate el))
			(null	      "")
			(t	      el)))
		    line))
	  2dl))

;; (defmacro PageSetup
;;     (sheet &key
;; 	     (PrintArea		nil)
;; 	     (Orientation	nil)
;; 	     (PaperSize		nil)
;; 	     (PrintTitleRows	nil)
;; 	     (Zoom		nil)
;; 	     (FitToPagesTall	nil)
;; 	     (FitToPagesWide	nil)
;; 	     (LeftHeader	nil)
;; 	     (CenterHeader	nil)
;; 	     (RightHeader	nil)
;; 	     (LeftFooter	nil)
;; 	     (CenterFooter	nil)
;; 	     (RightFooter	nil))
;;   `(progn
;;      ,@(mapcar

;; 	)))
(defmacro PageSetup
    (sheet &rest args)
  `(progn
     ,@(mapcar
	(lambda (xl)
	  (optima:match xl
	    ((LIST sym val)
	     `(setf (slot-value (slot-value ,sheet :PageSetup) ,sym)
		    ,val))))
	(group args 2))))

(defmacro merge-cells (sheet-dispatch &rest range)
  `(setf (slot-value (range ,sheet-dispatch ,@range) :MergeCells) t))
;; (defmacro set-font (sh &rest args)
;;   (optima:match column
;;     ((list start end)
;;      (let1 columns (column-expand start end)
;;        (let ((sym     (gensym))
;; 	     (counter (gensym)))
;; 	 `(if (eq ,(length columns) (length ,alignment))
;; 	      (iter (for ,sym :in ',columns)
;; 		    (for ,counter :upfrom 0)
;; 		    (setf (slot-value (cl-win32ole:ole ,sheet-dispatch :range (format nil "~A:~:*~A" (number-to-col ,sym)))
;; 				      ,type)
;; 			  (svref ,alignment ,counter)))
;; 	      (error "列数と指定された幅の数が合いません。")))))
;;     (symbol
;;      `(setf (slot-value (range ,sheet-dispatch ,symbol)
;; 			,type)
;; 	    (svref ,alignment 0)))))
;; (page-setup 'hoge :foo 2)

;; (require :cl-test-more)


