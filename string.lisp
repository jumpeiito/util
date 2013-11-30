;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   (require :alexandria)
;;   (require :cl-ppcre)
;;   (load-lib "util"))
(in-package :util)

(defmacro tr (&rest args)
  (if args
      `(append ',(if (listp (car args))
		     (iter (for c :from (char-code (first (car args)))
				:to (char-code (second (car args))))
			   (collect (code-char c)))
		     (list (car args)))
	       (tr ,@(cdr args)))
      nil))

(defparameter prealnum  ;"a-zA-Z0-9"
  (tr (#\a #\z) (#\A #\Z) (#\0 #\9)))
(defparameter postalnum ;"ａ-ｚＡ-Ｚ０-９"
  (tr (#\ａ #\ｚ) (#\Ａ #\Ｚ) (#\０ #\９)))
(defparameter prekigou
  (coerce "$<>+@() ･+?:;=!#$%&/_^.､ⅠⅡⅢⅣⅤⅥⅦⅧⅨ~~~-ｰ" 'list))
(defparameter postkigou
  (coerce "＄＜＞＋＠（）　・＋？：；＝！＃＄％＆／＿＾．、１２３４５６７８９〜～ーーー" 'list))
(defparameter preroman  (tr (#\0 #\9)))
(defparameter postroman (tr (#\０ #\９)))
(defparameter prekana
  (coerce "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝｧｨｩｪｫｯｬｭｮ" 'list))
(defparameter postkana
  (coerce "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲンァィゥェォッャュョ" 'list))
;; (defparameter preopt    "-")
;; (defparameter postopt   "－")

(defparameter kanalist
  (alexandria:alist-hash-table
   '(((65438 65398) . 12460)
     ((65438 65399) . 12462)
     ((65438 65400) . 12464)
     ((65438 65401) . 12466)
     ((65438 65402) . 12468)
     ((65438 65403) . 12470)
     ((65438 65404) . 12472)
     ((65438 65405) . 12474)
     ((65438 65406) . 12476)
     ((65438 65407) . 12478)
     ((65438 65408) . 12480)
     ((65438 65409) . 12482)
     ((65438 65410) . 12485)
     ((65438 65411) . 12487)
     ((65438 65412) . 12489)
     ((65438 65418) . 12496)
     ((65438 65419) . 12499)
     ((65438 65420) . 12502)
     ((65438 65421) . 12505)
     ((65438 65422) . 12508)
     ((65439 65418) . 12497)
     ((65439 65419) . 12500)
     ((65439 65420) . 12503)
     ((65439 65421) . 12506)
     ((65439 65422) . 12509)
     ((65438 65395) . 12532)
     ((65392)	    . 12540))
   :test #'equal))

(defparameter kanalist-vice
  (alexandria:alist-hash-table
   '((12460 (65398  65438))
     (12462 (65399 65438))
     (12464 (65400 65438))
     (12466 (65401 65438))
     (12468 (65402 65438))
     (12470 (65403 65438))
     (12472 (65404 65438))
     (12474 (65405 65438))
     (12476 (65406 65438))
     (12478 (65407 65438))
     (12480 (65408 65438))
     (12482 (65409 65438))
     (12485 (65410 65438))
     (12487 (65411 65438))
     (12489 (65412 65438))
     (12496 (65418 65438))
     (12499 (65419 65438))
     (12502 (65420 65438))
     (12505 (65421 65438))
     (12508 (65422 65438))
     (12497 (65418 65439))
     (12500 (65419 65439))
     (12503 (65420 65439))
     (12506 (65421 65439))
     (12509 (65422 65439))
     (12532 (65395 65438))
     (12540 (65392)))
   :test #'equal))

(defun string-join (strlist joiner)
  (declare (string joiner) (optimize (speed 3) (safety 0)))
  (with-output-to-string (s)
    (dolist (str (butlast strlist))
      (princ (str+ str joiner) s))
    (princ (car (last strlist)) s)))

(defun tr-making-hash (pre post)
  (iter (with hash = (make-hash-table :test #'equal))
	(for p :in pre)
	(for counter :upfrom 0)
	(setf (gethash p hash) (nth counter post))
	(finally (return hash))))

(defparameter h2z (tr-making-hash
		   ;; (string-join `(,prealnum ,prekigou ,preroman ,prekana) "")
		   ;; (string-join `(,postalnum ,postkigou ,postroman ,postkana) "")
		   (append prealnum prekigou preroman prekana)
		   (append postalnum postkigou postroman postkana)))

(defparameter z2h (tr-making-hash
		   (append postalnum postkigou postroman postkana)
		   (append prealnum prekigou preroman prekana)))

(defmacro def-zh-core (name hash)
  `(defun ,name (str)
     "第一段階の変換。ハッシュで置き換えられるものを置き換える。"
     (iter (for char :in-vector str)
	   (declare (type character char))
	   (collect (aif (gethash char ,hash nil)
	   		 ;; 置き換えられないものをわかりやすくするために、リストでくくっておく。
	   		 it (list char)))
	   ;; (print char)
	   )))

(def-zh-core to-hankaku-core z2h)
;; (def-zh-core to-zenkaku-core h2z)

(defun to-hankaku-core2 (strlist)
  "第二段階の変換。リストでくくってある濁音・半濁音を置き換える。
濁音・半濁音以外で置き換えられないものは、そのままにしておく。"
  (iter (for char :in strlist)
	(appending (if (listp char)
		       (aif (gethash (char-code (car char)) kanalist-vice)
			    (mapcar #'code-char (car it))
			    char)
		       (list char))
		   :into pot)
	(finally (return (coerce pot 'string)))))

;; (#\KATAKANA_LETTER_HI (#\HALFWIDTH_KATAKANA_SEMI-VOICED_SOUND_MARK)
;; 		      #\KATAKANA_LETTER_N #\KATAKANA_LETTER_HO
;; 		      (#\HALFWIDTH_KATAKANA_SEMI-VOICED_SOUND_MARK) #\KATAKANA_LETTER_N)

(defmacro def-zh (name &key primary-function secondary-function)
  ;; 第一段階 ハッシュで置き換えられるものをすべて置き換える。
  ;; すべて置き換えられたら終了。
  (let ((answer (gensym)))
  `(defun ,name (str)
     (declare (optimize (speed 3) (safety 0) (debug 0))
	      (type string str))
     (let ((,answer (,primary-function str)))
       (if (every #'atom ,answer)
	   (coerce ,answer 'string)
	   ;; 第二段階 第一段階で置き換えられなかったもの(濁音・半濁音)を置き換える。
	   (,secondary-function ,answer))))))

(def-zh to-hankaku
    :primary-function to-hankaku-core :secondary-function to-hankaku-core2)

;; (Defun to-zenkaku-core (str)
;;   (iter (for c :in-string (reverse str))
;; 	))

(defun to-zenkaku (str)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type simple-string str))
  (iter (with pot)
	(for c :in-string str)
	(for p previous c)
	(declare (type character c p)
		 (type list pot))
	(if (or (char-equal c #\HALFWIDTH_KATAKANA_SEMI-VOICED_SOUND_MARK)
		(char-equal c #\HALFWIDTH_KATAKANA_VOICED_SOUND_MARK ))
	    (rplaca pot (code-char (gethash (mapcar #'char-code (list c p)) kanalist)))
	    (setf pot (cons (aif (gethash c h2z nil)
				 it c)
			    pot)))
	(finally (return (coerce (reverse pot) 'string)))))


;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   (require :cl-test-more))

(defmacro testf (func got expected desc)
  `(cl-test-more:is (,func ,got) ,expected (format nil "~A ~A" ',func ,desc)))

(defun to-hankaku-test ()
  (cl-test-more:plan 13)
  (cl-test-more:diag "to-hankaku unit-test")
  (testf to-hankaku "アイスクリーム" "ｱｲｽｸﾘｰﾑ"		"通常テスト1")
  (testf to-hankaku "アドホック" "ｱﾄﾞﾎｯｸ"		"通常テスト2")
  (testf to-hankaku "アーキテクチャ" "ｱｰｷﾃｸﾁｬ"		"通常テスト3")
  (testf to-hankaku "アカウンタビリティ" "ｱｶｳﾝﾀﾋﾞﾘﾃｨ"	"通常テスト4")
  (testf to-hankaku "イデオロギー" "ｲﾃﾞｵﾛｷﾞｰ"		"通常テスト5")
  (testf to-hankaku "ガリットチュウ" "ｶﾞﾘｯﾄﾁｭｳ"		"濁音・拗音テスト")
  (testf to-hankaku "１２３４５６７" "1234567"		"数字テスト1")
  (testf to-hankaku "ホ１ゲ２ン３４" "ﾎ1ｹﾞ2ﾝ34"		"数字テスト2")
  (testf to-hankaku "ほ＠％＆（）！" "ほ@%&()!"		"記号テスト1")
  (testf to-hankaku "＄＝＜＞？＿＋" "$=<>?_+"		"記号テスト2")
  (testf to-hankaku "Ｖ１〜５" "V1~5"			"記号テスト3")
  (testf to-hankaku "" ""				"空白テスト1")
  (testf to-hankaku "　" " "				"空白テスト2"))

(defun to-zenkaku-test ()
  (cl-test-more:plan 10)
  (cl-test-more:diag "to-zenkaku unit-test")
  (testf to-zenkaku "ｱｲｽｸﾘ-ﾑ"	"アイスクリーム"		"通常テスト")
  (testf to-zenkaku "ｶﾞﾘｯﾄﾁｭｳ"	"ガリットチュウ"		"濁音・拗音テスト1")
  (testf to-zenkaku "ﾋﾟﾝﾎﾟﾝ"	"ピンポン"		"濁音・拗音テスト2")
  (testf to-zenkaku "archive"	"ａｒｃｈｉｖｅ"		"アルファベットテスト1")
  (testf to-zenkaku "attend"	"ａｔｔｅｎｄ"		"アルファベットテスト2")
  (testf to-zenkaku "ad hoc"	"ａｄ　ｈｏｃ"		"アルファベットテスト3")
  (testf to-zenkaku "inter-bank" "ｉｎｔｅｒーｂａｎｋ"	"アルファベットテスト4")
  (testf to-zenkaku "1234567"	"１２３４５６７"		"数字テスト1")
  (testf to-zenkaku "ﾎ1ｹﾞ2ﾝ34"	"ホ１ゲ２ン３４"		"数字テスト2")
  (testf to-zenkaku "ほ@%&()!"	"ほ＠％＆（）！"		"記号テスト1")
  (testf to-zenkaku "$=<>?_+"	"＄＝＜＞？＿＋"		"記号テスト2")
  (testf to-zenkaku "V1~5"	"Ｖ１ー５"		"記号テスト3")
  (testf to-zenkaku ""		""			"空白テスト1")
  (testf to-zenkaku " "		"　"			"空白テスト2"))

;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   (to-hankaku-test)
;;   (to-zenkaku-test))

(defmacro str+ (&rest args)
  `(concatenate 'string ,@args))

(defun split-string (str separator)
  (declare (optimize (speed 3) (safety 0)))
  (asdf::split-string str :separator separator))

(defun number->string (num)
  (write-to-string num))

(defun string->number (str)
  (multiple-value-bind (num len) (read-from-string str)
    (declare (ignore len))
    num))

;; (defun substring (str start end)
;;   (coerce (loop for i from start to end collect (aref str i))
;; 	  'string))



;; (defun string-tr-expand (strsym)
;;   ;; (declare (optimize (speed 3) (safety 0)))
;;   (if (ppcre:scan "-" strsym)
;;       (let1 l (mapcar (compose #'char-int (lambda (s) (aref s 0)))
;; 		      (split-string strsym "-"))
;; 	(coerce
;; 	 (mapcar
;; 	  #'code-char
;; 	  (loop for i from (first l) to (second l) collect i))
;; 	 'string))
;;       strsym))

;; (defun char->string-tr-expand (charl)
;;   (coerce
;;    (reverse (string-tr-expand (coerce charl 'string)))
;;    'list))

;; (defun string-tr-core (str)
;;   ;; (declare (string str) (optimize (speed 3) (safety 0)))
;;   (labels ((fn (subl r)
;; 	     (if subl
;; 		 (if (and (char-equal (car subl) #\-) (cdr subl))
;; 		     (funcall #'fn (nthcdr 2 subl)
;; 			      (cons (char->string-tr-expand (list (car r) (car subl) (second subl))) (cdr r)))
;; 		     (funcall #'fn (cdr subl) (cons (car subl) r)))
;; 		 (coerce (reverse (flatten r)) 'string))))
;;     (funcall #'fn (coerce str 'list) '())))



;; (defun string-tr (str hash)
;;   ;; (declare (string str pre post) (optimize (speed 3) (safety 0)))
;;   (labels ((fn (substr)
;; 	     (dotimes (j (length substr))
;; 	       (aif (gethash (aref substr j) hash)
;; 		    (setf (aref substr j) it)))
;; 	     substr))
;;     (fn (copy-seq str))))

;; 拡張可能のベクタ
;; CL-USER> (defparameter a (make-array 0 :adjustable t :fill-pointer t))
;; A
;; CL-USER> a
;; #()
;; CL-USER> (vector-push-extend 'abc a)
;; 0
;; CL-USER> a
;; #(ABC)
;; CL-USER> (vector-push-extend 'xyz a)
;; 1
;; CL-USER> a
;; #(ABC XYZ)
;; (defparameter test01 "ｱﾌﾞﾗｶﾀﾌﾞﾗ")
;; (defparameter test02 "ﾊｯﾌﾟﾝ")

;; (defparameter list01 "ガギグゲゴザジズゼゾダヂヅデドバビブベボパピプペポヴ")
;; (defparameter list02 "ﾊﾋﾌﾍﾎｳ")

;; (defun to/sub-core (str)
;;   (mapcar #'char-int
;; 	  (reverse (coerce str 'list))))

;; (defun to/sub-return (charint-list)
;;   (coerce (mapcar #'code-char charint-list)
;; 	  'string))

;; (defmacro to-zenhan-sub-defun
;;     (name attachfn hash-table true false)
;;   `(defun ,name (str)
;;      (labels ((in (subl pre r)
;; 		(if (null subl)
;; 		    (to/sub-return r)
;; 		    (in (cdr subl)
;; 			(car subl)
;; 			(aif (gethash (funcall ,attachfn (car subl))
;; 				      ,hash-table nil)
;; 			     ,true ,false)))))
;;        (in (to/sub-core str) 0 ()))))

;; ;; (to-zenhan-sub-defun to-zenkaku-sub
;; ;; 		     (lambda (car) (list pre car))
;; ;; 		     kanalist
;; ;; 		     (cons it r)
;; ;; 		     (if (or (eq (car subl) 65438)
;; ;; 			     (eq (car subl) 65439))
;; ;; 			 r
;; ;; 			 (cons (car subl) r)))
;; (defun to-zenkaku-sub (str)
;;   (labels ((in (subl pre r)
;; 	     (if (null subl)
;; 		 (to/sub-return r)
;; 		 (in (cdr subl) (car subl)
;; 		     (aif (or (gethash (list pre (car subl)) kanalist)
;; 			      (gethash (list (car subl)) kanalist))
;; 			  (cons it r)
;; 			  (if (or (eq (car subl) 65438)
;; 				  (eq (car subl) 65439))
;; 			      r
;; 			      (cons (car subl) r)))))))
;;     (in (to/sub-core str) 0 '())))

;; (defun to-zenkaku-sub (str)
;;   (labels ((in (subl pre r)
;; 	     (if (null subl)
;; 		 (to/sub-return r)
;; 		 (aif (gethash (list pre (car subl)) kanalist nil)
;; 		      (in (cdr subl) (car subl) (cons it r))
;; 		      (in (cdr subl) (car subl)
;; 			  (if (or (eq (car subl) 65438)
;; 				  (eq (car subl) 65439))
;; 			      r
;; 			      (cons (car subl) r)))))))
;;     (in (to/sub-core str) 0 '())))

;; (to-zenhan-sub-defun to-hankaku-sub
;; 		     #'identity
;; 		     kanalist-vice
;; 		     (append (car it) r)
;; 		     (cons (car subl) r))

;; ;; (defun to-hankaku-sub (str)
;; ;;   (labels ((in (subl r)
;; ;; 	     (if (null subl)
;; ;; 		 (to/sub-return r)
;; ;; 		 (in (cdr subl)
;; ;; 		     (aif (gethash (car subl) kanalist-vice nil)
;; ;; 			  (append (car it) r)
;; ;; 			  (cons (car subl) r))))))
;; ;;     (in (to/sub-core str) '())))

;; (defun to-zenkaku (str)
;;   (string-tr (to-zenkaku-sub str) h2z))

;; (defun to-hankaku (str)
;;   (string-tr (to-hankaku-sub str) z2h))

(defun number-with-keta (num keta)
  (declare (fixnum keta) (optimize (speed 3) (safety 0)))
  (let1 n (cond
	    ((integerp num) (write-to-string num))
	    ((stringp num) num)
	    (t (error "Argument1 must be string or number.")))
    (format nil "~{~A~^,~}"
	    (reverse (mapcar
		      (lambda (x) (concatenate 'string (reverse x)))
		      (group (reverse (coerce n 'list)) keta))))))

(defun x->string-join (l joiner)
  (declare (list l) (vector joiner) (optimize (speed 3) (safety 0)))
  (the vector
    (string-join (the list (mapcar (lambda (item) (if (stringp item) item (write-to-string item)))
				   (the list l)))
	       joiner)))

(defun regex-replace-alist (target-string alist)
  (labels ((in (subt subl)
	     (if (null subl)
		 subt
		 (in (cl-ppcre:regex-replace-all (caar subl) subt (cdar subl))
		     (cdr subl)))))
    (in target-string alist)))

(defmacro string-case (str &body clause)
  `(cond
     ,@(mapcar
       (lambda (list)
	 (if (listp (first list))
	     `((or ,@(mapcar
		      (lambda (target) `(equal ,str ,target))
		      (first list)))
	       (progn ,@(cdr list)))
	     (if (equal t (first list))
		 `(t (progn ,@(cdr list)))
		 `((equal ,str ,(first list))
		   (progn ,@(cdr list))))))
       clause)))

(defmacro rxmatch-bind (regexp target-string val &rest body)
  (let ((all (gensym)) (part (gensym)))
    `(multiple-value-bind (,all ,part)
	 (cl-ppcre:scan-to-strings ,regexp ,target-string)
       (declare (ignorable ,all))
       (if ,part
	   (progn
	     (destructuring-bind ,val (coerce ,part 'list)
	       ,@body))
	   nil))))

(defun list->write-line (list &optional op)
  (write-line (string-join list ",")
	      (or op *standard-output*)))

(defmacro __ (string &optional op)
  `(write-line ,string ,(or op *standard-output*)))

(defmacro >> (list &optional op)
  `(write-line (x->string-join ,list ",")
	       ,(or op *standard-output*)))

(defun read-line->list (str)
  (ppcre:scan "," str))

(defun string-null (str)
  (equal "" str))

(defun string-not-null (str)
  (not (string-null str)))

(defmacro stdout (formatta &rest args)
  `(format *standard-output* ,formatta ,@args))

(defun string-take (string num &optional start)
  (declare (type string string) (type fixnum num)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (subseq string (or start 0)
	  (if start (min (length string) (+ (the fixnum start) num)) num)))

(defun string-take-right (string num)
  (declare (type string string) (type fixnum num)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (subseq string (- (length string) num)))

;; (string-match-number-expand '(5 2) 10)
;; --> ((0 5) (5 7))
;; (string-match-number-expand '(5 *) 10)
;; --> ((0 5) (5 10))
(defun duo (list)
  (labels ((in (subl r)
	     (cl-match:match subl
	       ((list  x '*)   (reverse (cons (list x '*) r)))
	       ((list* x '* z) (error "* must be on last."))
	       ((list* x y z)  (in (cons (+ x y) z) (cons (list x (+ x y)) r)))
	       ((list x)       (reverse r)))))
    (in list '())))

;; (defun string-match-number-expand
;;     (number-list str &key (from 0))
;;   (mapcar (lambda (l)
;;   	    (cl-match:match l
;;   	      ((list x '*) `(,x (length ,str)))
;;   	      ((list x y)  `(,x ,y))))
;;   	  (duo (cons from number-list))))

;; (string-match "hoge,foo,buz" ((x 5) (y 4))
;;   (list x y))
;;   ==
;; (let ((x (subseq "hoge,foo,buz" 0 5))
;;       (y (subseq "hoge,foo,buz" 5 9)))
;;   (list x y))
;; --> ("hoge," "foo,")
;;
;; (string-match "hoge,foo,buz" ((x 5) (y *))
;;   (list x y))
;; (let ((x (subseq "hoge,foo,buz" 0 5))
;;       (y (subseq "hoge,foo,buz" 5 (length "hoge,foo,buz"))))
;;   (list x y))
;; --> ("hoge," "foo,buz")
;; 
;; (string-match "hoge,foo,buz" ((_ 2) (x 5) (y *))
;;   (list x y))
;; --> ("ge,fo" "o,buz")
;; (defmacro string-match (str pattern &rest body)
;;   (cl-match:match str
;;     ((type string)
;;      `(let ,(mapcar
;; 	     (lambda (sym pair) `(,sym (subseq ,str ,@pair)))
;; 	     (mapcar #'first pattern)
;; 	     (string-match-number-expand (mapcar #'second pattern) (length str)))
;; 	,@body))
;;     ))
;; (defmacro string-match (str pattern &rest body)
;;   `(let ,(remove-if-not
;; 	  #'identity
;; 	  (mapcar
;; 	   (lambda (sym pair)
;; 	     (if (equal '* sym) nil `(,sym (subseq ,str ,@pair))))
;; 	   (mapcar #'first pattern)
;; 	   ;; (string-match-number-expand (mapcar #'second pattern) str)
;; 	   ))
;;      ,@body))


;; (defun string-match-test ()
;;   (cl-test-more:is (string-match "hoge,foo,buz" ((x 5) (y 4)) (list x y))
;; 		   `("hoge," "foo,"))
;;   (cl-test-more:is (string-match "hoge,foo,buz" ((x 5) (y 2) (z *)) (list x y z))
;; 		   `("hoge," "fo" "o,buz"))
;;   (cl-test-more:is (string-match "hoge,foo,buz" ((* 2) (x 5) (y *)) (list x y))
;; 		   `("ge,fo" "o,buz")))

(defun explode (string)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type simple-string string))
  (iter (for s :in-string string)
	(declare (type character s))
	(collect (string s))))

(defun zero-padding (string length)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type simple-string string)
	   (type fixnum length))
  (format nil "~A~A"
	  (make-string (- length (length string)) :initial-element #\0)
	  string))

(defun ruby-scan (regexp string)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type simple-string regexp string))
  (labels ((inner (subst r)
	     (declare (type simple-string subst))
	     (multiple-value-bind (st end stv endv)
		 (ppcre:scan regexp subst)
	       (declare (ignore stv endv))
	       (if st
		   (inner (subseq subst end) (cons (subseq subst st end) r))
		   (reverse r)))))
    (inner string nil)))

(in-package :cl-user)
