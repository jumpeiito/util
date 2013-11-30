
(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :util)
  (require :cl-pdf)
  (use-package :cl-pdf))

(define-foreign-library xdoc
  (t (:default "xd2txlib")))

(use-foreign-library xdoc)

;; "y:/47伊東/B_健診/A_集合契約_受診者/data/20131107/受診者別支払一覧表（特定健診分）.pdf"
(defparameter *foreign-string*
  (foreign-alloc :char))

(defcfun ("ExtractText" xdoc2txt) :string
  (filename   :string)
  (boolean    :boolean)
  (returntext :pointer))

(defun xdoc (pathname)
  (let ((mem  (foreign-alloc :char)))
    (xdoc2txt pathname nil mem)
    (prog1
	(sb-ext:octets-to-string
	 (coerce
	  (loop :for i :from 0 :to 20000 :collect (mem-aref mem :char i))
	  'vector)
	 :external-format :UTF-8)
      (foreign-free mem))))


(with-existing-document (#P"f:/util/受診者別支払一覧表（特定健診分）.pdf")
  (with-existing-page (0)
    (pdf:move-text 1 1))
  (write-document #P"hoge.pdf"))
