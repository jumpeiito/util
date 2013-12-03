(in-package :util)

(defun flatten (tree &key (filter #'identity) (function #'identity))
  (iter (for leaf :in tree)
	(cond
	  ((not leaf) nil)
	  ((and (atom leaf) (funcall filter leaf))
	   (collect (funcall function leaf) :into ret))
	  (t           (appending (flatten leaf :filter filter :function function)
				  :into ret)))
	(finally (return ret))))

(defun uniq (list)
  (iter (for el :in list)
	(unless (member el ret :test #'equal)
	  (collect el :into ret))
	(finally (return ret))))

(defun for-each (function list)
  (iter (for el :in list)
	(funcall function el)))

(defun for-each-with-index (function list &key (counter-start 0))
  (iter (for el :in list)
	(for counter :upfrom counter-start)
	(funcall function el counter)))

;; (defmacro for-each-with-index (fn l)
;;   (declare (list l) (optimize (speed 3) (safety 0)))
;;   `(let ((fewilist ,l))
;;      (dotimes (i (length fewilist))
;;        (funcall ,fn i (nth i fewilist)))))

;; (defun nthcar (nth l)
;;   (iter (for el :in l)
;; 	(for i :upfrom 1)
;; 	(if ())))

(defun nthcar (nth l)
  (declare (fixnum nth) (optimize (speed 3) (safety 0)))
  (loop for i from 0 to nth collect (nth i l)))

(defun group (l num)
  (declare (optimize (speed 3) (safety 0)))
  (iter (for el :in l)
	(for counter :upfrom 1)
	(declare (type fixnum counter)
		 (type list l))
	(if (eq 0 (mod counter num))
	    (progn
	      (collect (append small (list el)) :into ret)
	      (setq small nil))
	    (collect el :into small))
	(finally (return (append ret (if small (list small) nil))))))

(defun iota (&key (to 0) (from 0) (step 1))
  (declare (type fixnum to from step) (optimize (speed 3) (safety 0)))
  (iter (for i :from from :to to :by step)
	(collect i)))

(defun take (l n &key (from 0))
  (declare (type list l) (type integer n from) (optimize (speed 3) (safety 0)))
  (iter (for el :in (nthcdr from l))
	(for counter :upfrom 0)
	(declare (type fixnum counter))
	(if (> n counter)
	    (collect el))))

(defun take-right (l n &key (from 0))
  (reverse (take (reverse l) n)))

(defun drop (l n)
  (declare (list l) (integer n) (optimize (speed 3) (safety 0)))
  (the list
    (loop for i from n to (- (length l) 1) collect (nth i l))))

(defun mapcar-with-index (fn l)
  (declare (list l) (optimize (speed 3) (safety 0)))
  (labels ((inner (subl c r)
	     (if (null subl)
		 (reverse r)
		 (inner (cdr subl) (+ 1 c)
			(cons (funcall fn c (car subl)) r)))))
    (the list (funcall #'inner l 0 '()))))

;; (defmacro multiple-sort (l &rest clause)
;;   (labels ((subfn (expansion subc)
;; 	     (if (null subc)
;; 		 expansion
;; 		 (subfn `(the list (sort (the list ,expansion)
;; 					 (lambda (x y) ,(car subc)))) (cdr subc)))))
;;     (subfn (copy-list l) clause)))
(defmacro multiple-sort (l &rest clause)
  (if (null clause)
      l
      `(sort2 ,(if (cdr clause)
		   `(multiple-sort ,l ,@(cdr clause)) l)
	      ,(first (car clause)) ,(second (car clause)))))

(defun list->array (l)
  (declare (list l) (optimize (speed 3) (safety 0)))
  (labels ((fn (subl v)
	     (if (null subl)
		 v
		 (fn (cdr subl) (progn (vector-push-extend (car subl) v) v)))))
    (the array (funcall #'fn l (make-array 0 :adjustable t :fill-pointer t)))))

(defmacro cell (2dl ary)
  ;; `(nth ,(cdr ary) (nth ,(car ary) ,2dl))
  `(destructuring-bind (y x) ',ary
     (nth (1- x) (nth (1- y) ,2dl))))

(defun combinate% (l1 l2)
  (labels ((inner (sl1 sl2 r)
	     (if (null sl2)
		 (reverse r)
		 (if (null sl1)
		     (inner l1 (cdr sl2) r)
		     (inner (cdr sl1) sl2
			    (cons (if (listp (car sl1))
				      (append (car sl1) (list (car sl2)))
				      (list (car sl1) (car sl2))) r))))))
    (if (or (null l2) (null l1))
	(or l1 l2)
	(inner l1 l2 '()))))

(defun combinate (&rest lofl)
  (labels ((inner (subl r)
	     (if (null subl)
		 r
		 (inner (cdr subl)
			(combinate% r (car subl))))))
    (inner lofl '())))

(defun last1 (list) (car (last list)))

(defun filter (fn list)
  (labels ((in (subl r)
	     (if (null subl)
		 (reverse r)
		 (in (cdr list)
		     (aif (funcall fn (car subl)) (cons it r) r)))))
    (in list '())))

(defun index-base (target list function)
  (labels ((in (subl c)
	     (if (null subl)
		 nil
		 (if (funcall function target (car subl))
		     c
		     (in (cdr subl) (1+ c))))))
    (in list 0)))

(defun index (target list &key (test #'equal))
  (index-base target list test))

(defun index-regexp (regexp list)
  (index-base regexp list #'ppcre:scan))

(defmacro sort2 (list sort-fn attach-fn)
  `(sort (copy-list ,list)
	 (lambda (x y) (,sort-fn (,attach-fn x) (,attach-fn y)))))

(defun 2dl-map (fn 2dl)
  (mapcar
   (lambda (x2dl)
     (mapcar
      (lambda (line) (funcall fn line))
      x2dl))
   2dl))

(defun string-2dl-sum (2dl &key (string nil))
  (let1 l (reduce
	   (lambda (x y) (mapcar #'+ x y))
	   (2dl-map #'read-from-string 2dl))
    (if string (mapcar #'write-to-string l) l)))

(defun singlep (obj)
  (if (and (car obj) (not (cdr obj)))
      obj))

(defun compact (list)
  (remove-if-not #'identity list))

;; (defun classify-multiple-value (testfn list)
;;   (iter (for el in list)
;;   	(if (funcall testfn el)
;;   	    (collect el :into succeed)
;;   	    (collect el :into failure))
;;   	(finally (return (values succeed failure))))
;;   )

(defun find-if->index (pred list)
  (labels ((inner (subl counter)
	     (cond
	       ((null subl) nil)
	       ((funcall pred (car subl)) counter)
	       (t (inner (cdr subl) (1+ counter))))))
    (inner list 0)))

(defun find-if-regexp (regexp list)
  (find-if (lambda (s) (and (stringp s) (ppcre:scan regexp s)))
	   list))

(defun find-if-regexp->index (regexp list)
 (find-if->index (lambda (s) (and (stringp s) (ppcre:scan regexp s)))
		 list))

;; (append-total '((1 2 3) (4 5 6))) => '((1 2 3) (4 5 6) (5 7 9))
(defun append-total (list &key (from 0) (initial-element ""))
  (iter (with total = (nthcdr from (car list)))
	(for cell :in list)
	(if (first-time-p)
	    (collect cell :into pot)
	    (progn
	      (setq total (mapcar #'+
				  total
				  (nthcdr from cell)))
	      (collect cell :into pot)))
	(finally (return `(,@pot (,@(make-list from :initial-element initial-element)
				  ,@total))))))

;; (append-group-total 2 '((0 0 0) (1 1 1) (2 2 2) (3 3 3)))
;; => '((0 0 0) (1 1 1) (2 2 2) (3 3 3) (2 2 2) (4 4 4))
(defun append-group-total (n list)
  (let ((l (group list n)))
    (reduce (lambda (x y)
	      )
	    (cdr l)
	    :initial-value (car l)
	    :from-end t)))

(defun repeated-list (times elements)
  (iter (for i :from 0 :to (1- times))
	(appending elements)))

(in-package :cl-user)
