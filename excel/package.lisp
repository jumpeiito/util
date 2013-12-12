(defpackage :excel
  (:nicknames :ex)
  (:use :cl :iterate :util :cl-win32ole)
  (:export #:with-excel
	   #:with-excel-book
	   #:ole-value
	   #:lastrow
	   #:lastcol
	   #:range
	   #:cells
	   #:cell-value!
	   #:get-color
	   #:set-color
	   #:set-colorindex
	   #:set-column-width
	   #:set-alignment
	   #:set-number-format-local
	   #:set-fontname
	   #:borders
	   #:border
	   #:page-setup
	   #:value
	   #:set-colorindex
	   #:set-color
	   #:set-width
	   #:set-format
	   #:decide-range
	   #:decide-range-value
	   #:decide-range-borders
	   #:merge-cells
	   #:page-setup))
