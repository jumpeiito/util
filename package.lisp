(defpackage :util.jump
  (:nicknames :util)
  (:use :cl :iterate :cl-win32ole)
  (:import-from #:alexandria
  		#:hash-table-alist
  		#:alist-hash-table
  		#:read-file-into-string)
  (:import-from #:iterate
  		#:iter
  		#:for
  		#:repeat
  		#:collect
  		#:counting
  		#:maximize
  		#:generating
  		#:next
  		#:sum
  		#:with
  		#:finally
  		#:appending
  		#:first-time-p
  		#:defmacro-clause
  		#:defmacro-driver)
  (:import-from #:cl-ppcre
  		#:scan
  		#:regex-replace-all
  		#:scan-to-strings
  		#:split)
  #+sbcl (:import-from #:sb-impl #:fd-stream-file)
  (:import-from #:cxml-stp #:make-builder)
  (:import-from #:cl-fad #:file-exists-p)
  (:export #:dbind
  	   #:mvbind
  	   #:let1
  	   #:rlet1
  	   #:rvlet1
  	   #:os-expand
  	   #:compose
  	   #:fn
	   #:while
  	   #:^
  	   #:cut
  	   #:aif
  	   #:awhen
  	   #:it
  	   #:acond
  	   #:aand
  	   #:&
  	   #:any
  	   #:flatten
  	   #:uniq
  	   #:for-each
  	   #:for-each-with-index
  	   #:nthcar
  	   #:group
  	   #:iota
  	   #:take
  	   #:take-right
  	   #:drop
  	   #:mapcar-with-index
  	   #:multiple-sort
  	   #:list->array
  	   #:cell
  	   #:combinate
  	   #:last1
  	   #:filter
  	   #:index
  	   #:index-regexp
  	   #:sort2
  	   #:2dl-map
  	   #:string-2dl-sum
  	   #:singlep
  	   #:compact
  	   #:classify-multiple-value
  	   #:str+
  	   #:number->string
  	   #:string->number
  	   #:string-join
  	   #:to-zenkaku
  	   #:to-hankaku
  	   #:number-with-keta
  	   #:x->string-join
  	   #:regex-replace-alist
  	   #:string-case
  	   #:rxmatch-bind
  	   #:directory-list
  	   #:alld
  	   #:allf
  	   #:filename-basename
  	   #:call-with-output-file2
  	   #:call-with-input-file2
  	   ;; #:make-directory
  	   #:delete-file-if-exists
  	   #:base-directory
  	   #:normal-date->string
  	   #:date-string-normalize
  	   #:date->japanese-string
  	   #:date->japanese-month
  	   #:rxmatch-case
  	   #:rxmatch-case2
  	   #:english-month-name
  	   #:string->date
  	   #:timestamp-p
  	   #:how-old
  	   #:nendo
  	   #:nendo-year
  	   #:nendo-year-string
  	   #:date>
  	   #:date<
  	   #:date>=
  	   #:date<=
  	   #:date=
  	   #:date-year
  	   #:date-month
  	   #:date-day
  	   #:nendo-month-list
  	   #:optcase
  	   #:push-hash
  	   #:sethash
  	   #:simple-hash-table
  	   #:count-hash-table
  	   #:push-hash-table
  	   ;; #:kx/parse
  	   ;; #:hs/parse-zip-main
  	   ;; #:rengokai-file-p
  	   ;; #:hospital-hash
  	   ;; #:hospital-short-hash
  	   ;; #:dock-hash
  	   ;; #:dock?
  	   ;; #:hospital-shibu-hash
  	   ;; #:shibu-kensin?
  	   ;; #:code->hospital
  	   ;; #:kgbg-regexp
  	   ;; #:172data
  	   ;; #:172-hash
  	   ;; #:172-complex-hash
  	   ;; #:172-hash2
  	   ;; #:short-vice-shibu-hash
  	   ;; #:long-vice-shibu-hash
  	   ;; #:short-shibu
  	   ;; #:long-shibu
  	   ;; #:kgbg
  	   ;; #:shibu>
  	   ;; #:shibu<
  	   ;; #:bunkai-hash
  	   #:tree-map
  	   #:tree-map-with-index
  	   #:tree-for-each
  	   #:vector-find-if
  	   #:csv-read-to-list
  	   #:csv-read
  	   #:phash
  	   #:chash
  	   #:shash
  	   #:string-null
  	   #:string-not-null
  	   #:nendo-end
  	   #:nendo-month-list
  	   #:strdt
  	   #:path+
  	   #:exists
  	   #:def-clojure
  	   #:string-take
  	   #:string-take-right
	   #:find-if->index
	   #:find-if-regexp
	   #:find-if-regexp->index
	   #:string-match
	   #:explode
	   #:aref-1+
	   #:aref-cons
	   #:compact-str
	   #:call-with-zipfile
	   #:map-zipfile
	   #:filter-zipfile
	   #:zero-padding
	   #:append-total
	   #:ruby-scan))
