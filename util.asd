(asdf:defsystem :util
  :version "0"
  :depends-on
  (:iterate :cl-ppcre :alexandria :asdf :parse-number
	    :local-time :cl-fad :cxml :cxml-stp :zip :cl-irregsexp :closer-mop
	    :cl-win32ole :simple-date-time :cl-test-more :optima)
  :components
  ((:file "package")
   (:file "utilb"	:depends-on ("package" ))
   (:file "list"	:depends-on ("package" "utilb"))
   (:file "iterate"	:depends-on ("package" "csv"))
   (:file "csv"		:depends-on ("package"))
   (:file "string"	:depends-on ("package" "utilb"))
   (:file "date"	:depends-on ("package" "utilb" "iterate" "list"))
   (:file "argv"	:depends-on ("package" "list"))
   (:file "hash"	:depends-on ("package" "utilb"))
   (:file "clojure"	:depends-on ("package"))
   (:file "file"	:depends-on ("package" "list"))
   (:file "tree-vector"	:depends-on ("utilb"))
   ;; (:file "excel"       :depends-on ("package"))
   (:module :excel
   	    :components ((:file "package")
   			 (:file "excel" :depends-on ("package"))))
   (:file "zip")))
