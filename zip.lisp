(in-package :util)

(defun zipfile-entry (zip)
  (zip:with-zipfile (zip-instance zip)
   (hash-table-alist
    (zip:zipfile-entries zip-instance))))

(defun zipfile-find (predicate attach-fn pathname)
  (zip:with-zipfile (zip-inst pathname)
    (let1 n nil
      (dolist (i (zipfile-entry pathname))
	(if (funcall predicate (car i) (cdr i))
	    (push (funcall attach-fn i) n)))
      n)))

(defun zipfile-contents (entry)
  (babel:octets-to-string
   (zip:zipfile-entry-contents entry)))

(defun zip->file-name (zipfile)
  (remove-if
   (lambda (f) (scan "/$" f))
   (mapcar #'car (zipfile-entry zipfile))))

(defun call-with-zipfile (zipfile f)
  (zip:with-zipfile (z zipfile)
    (funcall f z)))

(defun map-zipfile (f zipfile)
  (call-with-zipfile zipfile
    (lambda (zip)
      (iter (for (name entry) :in-hashtable (zip:zipfile-entries zip))
	    (collect (funcall f name entry))))))

(defun filter-zipfile (zipfile pred)
  (call-with-zipfile zipfile
    (lambda (zip)
      (iter (for (name entry) :in-hashtable (zip:zipfile-entries zip))
	    (if (funcall pred name entry)
		(collect (list name entry)))))))

(in-package :cl-user)
