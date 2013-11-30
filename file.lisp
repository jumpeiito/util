;; (require :asdf)
;; (load-lib "util")
(in-package :util)

(unless (fboundp 'flatten)
  (defun flatten (tree &key (filter #'identity) (function #'identity))
    (labels ((%in (subt r)
	       (if (null subt)
		   (reverse r)
		   (if (atom (car subt))
		       (%in (cdr subt)
			    (if (funcall filter (car subt))
				(cons (funcall function (car subt)) r)
				r))
		       (%in (cdr subt)
			    (concatenate 'list
					 (reverse (flatten (car subt) :filter filter :function function))
					 r))))))
      (%in tree '()))))

(defmacro directory-list (dirname &key (type nil) (regexp nil))
  `(remove-if-not
    (lambda (path)
      (and ,(if type
		`(and path
		      (pathname-type path)
		      (or (equal (pathname-type path) (string-downcase ,type))
			  (equal (pathname-type path) (string-upcase ,type))))
		t)
	   ,(if regexp
		`(and path
		      (pathname-name path)
		      (ppcre:scan ,regexp (pathname-name path)))
		t)))
    (asdf::directory-files ,dirname)))

(defun alld (dir)
  (declare (optimize (speed 3) (safety 0)))
  (labels ((a (pathlist ret)
	     (if (null pathlist)
		 (flatten (the list (cons dir (the list ret))))
		 (a (the list (cdr pathlist))
		    (cons (if (null (asdf::subdirectories (car pathlist)))
			      (car pathlist)
			      (alld (car pathlist))) ret)))))
    (a (the list (asdf::subdirectories dir)) '())))

(defun subfiles (dir)
  (declare (optimize (speed 3) (safety 0)))
  (remove-if #'asdf::directory-pathname-p
	     (asdf::directory-files dir)))

(defun directory-compare (parent? children?)
  (labels ((in (p c)
	     (if (null p)
		 t
		 (if (equal (car p) (car c))
		     (in (cdr p) (cdr c))
		     nil))))
    (in parent? children?)))

(defun directory-children-p (parent? children?)
  (if (equal (pathname-device parent?)
	     (pathname-device children?))
      (if (null (pathname-directory parent?))
	  t
	  (directory-compare (pathname-directory parent?)
			     (pathname-directory children?)))
      nil))


(defun allf-cut-prune (directory-list prune)
  (labels ((in (subp r)
	     (if (null subp)
		 r
		 (in (cdr subp)
		     (remove-if
		      (lambda (file) (directory-children-p (car subp) file))
		      r)))))
    (in prune directory-list)))

(defun allf (dir &key (type nil) (prune nil) (regexp nil))
  (declare (optimize (speed 3) (safety 0)))
  (funcall
   ;; (if type
   ;;     (lambda (list)
   ;; 	 (remove-if-not
   ;; 	  (lambda (file) (and (equal type (pathname-type file))
   ;; 			      (if regexp (cl-ppcre:scan regexp (pathname-name file)) t)))
   ;; 	  list))
   ;;     #'identity)
   (lambda (list)
     (remove-if-not
      (lambda (file) (and (if TYPE (equal type (pathname-type file)) T)
			  (if REGEXP (cl-ppcre:scan regexp (pathname-name file)) T)))
      list))
   (mapcan #'subfiles
	   (allf-cut-prune (alld dir) prune))))

(defun filename-basename (pathname)
  ;; (declare (pathname pathname) (optimize (speed 3) (safety 1)))
  (if (pathnamep pathname)
      (the string (str+ (pathname-name pathname)
			"."
			(pathname-type pathname)))
      (car (last (ppcre:split "/" pathname)))))

(defun call-with-output-file (filename func &key (code :UTF-8) (if-exists :supersede))
  (let1 op (gensym)
    (with-open-file (op filename :direction :output
			:external-format `(,code)
			:if-exists if-exists)
      (funcall func op))))

(defun call-with-output-file2 (filename func &key (code :UTF-8) (if-exists :supersede))
  (let1 op (gensym)
    (with-open-file (op filename :direction :output
			:external-format code
			:if-exists if-exists)
      (funcall func op))))

(defun call-with-input-file (filename func &key (code :UTF-8))
  (let1 ip (gensym)
    (with-open-file (ip filename :external-format `(,code))
      (funcall func ip))))

(defun call-with-input-file2 (filename func &key (code :UTF-8))
  (let1 ip (gensym)
    (with-open-file (ip filename :external-format code)
      (funcall func ip))))

(defun windows-dir-sep (string)
  (ppcre:regex-replace-all "/" string "\\"))

(defun make-file-next-version (file counter)
  (let1 gen (make-pathname :defaults file)
    (make-pathname :defaults gen
		   :name (format nil "~A-version~A"
				 (pathname-name gen)
				 counter))))

(defun file-next-version (file)
  (if (cl-fad:file-exists-p file)
      (labels ((in (counter)
		 (let1 newname (make-file-next-version file counter)
		   (if (cl-fad:file-exists-p newname)
		       (in (1+ counter))
		       newname))))
	(in 1))
      (make-pathname :defaults file)))

(defun move-file-copy-version (source destination)
  (let1 next (file-next-version destination)
    (cl-fad:copy-file source next
		      :overwrite nil)
    next))

(defun move-file-copy (source destination if-exists)
  (if (eq if-exists :version)
      (move-file-copy-version source destination)
      (cl-fad:copy-file source destination
			:overwrite
			(cond
			  ((eq if-exists :overwrite) t)
			  ((eq if-exists :none) nil)))))

(defun move-file (source destination &key (type :copy) (if-exists :none))
  (case type
    (:copy (move-file-copy source destination if-exists))
    (:move (move-file-move source destination if-exists))))

(defun make-directory (dirname)
  #+sbcl (let ((sb-alien::*default-c-string-external-format* :SJIS))
    (unless (cl-fad:file-exists-p dirname)
      (sb-posix:mkdir dirname 0777))))

(defun ctime (path)
  (let ((sb-alien::*default-c-string-external-format* :SJIS))
    (sb-posix:stat-ctime (sb-posix:stat path))))

(defun atime (path)
  (let ((sb-alien::*default-c-string-external-format* :SJIS))
    (sb-posix:stat-atime (sb-posix:stat path))))

(defun mtime (path)
  (let ((sb-alien::*default-c-string-external-format* :SJIS))
    (sb-posix:stat-mtime (sb-posix:stat path))))

(defmacro defun-comp-ctime (name fn)
  #+sbcl `(defun ,name (path1 path2)
     (let ((sb-alien::*default-c-string-external-format* :SJIS))
       (,fn (sb-posix:stat-ctime (sb-posix:stat path1))
	    (sb-posix:stat-ctime (sb-posix:stat path2))))))

(defun delete-file-if-exists (pathname)
  (if (cl-fad:file-exists-p pathname)
      (delete-file pathname)))

(defun-comp-ctime ctime> >)
(defun-comp-ctime ctime< <)
(defun-comp-ctime ctime= eq)

(defun path+ (parent child)
  (if (cl-fad:directory-pathname-p child)
      (make-pathname :defaults parent
		     :directory (append (pathname-directory parent)
					(cdr (pathname-directory child))))
      (merge-pathnames parent child)))

(defun exists (dirlist)
  (find-if #'cl-fad:file-exists-p dirlist))

;; #P"f:/20130628/kmove/hoge.xls" -> #P"f:/20130628/kmove/"
(defun base-directory (pathname)
  (make-pathname :defaults pathname
		 :name nil :type nil))

(in-package :cl-user)
