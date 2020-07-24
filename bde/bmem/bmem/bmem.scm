;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/bde/bmem/bmem/bmem.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Apr 15 09:59:09 2003                          */
;*    Last change :  Sun Jun  9 09:06:30 2019 (serrano)                */
;*    Copyright   :  2003-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Allocation profiler visualizer. This tool generates an HTML file */
;*    from a monitor file.                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmem
   (include "html.sch")
   (import  html
	    bmem_param
	    bmem_tools
	    bmem_function
	    bmem_gc
	    bmem_type
	    bmem_thread)
   (main    main)
   (export  (dynamic-css gcmon function types)
	    (function-ref ::obj)
	    (function-ident-pp ::obj)
	    (type-ref ::obj)))

;*---------------------------------------------------------------------*/
;*    bigloo-fildir ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (bigloo-fildir)
   (make-file-name (bigloo-config 'library-directory) "bmem"))

;*---------------------------------------------------------------------*/
;*    plugin-exit ...                                                  */
;*---------------------------------------------------------------------*/
(define (plugin-exit val)
   (exit -1))

;*---------------------------------------------------------------------*/
;*    http-request? ...                                                */
;*---------------------------------------------------------------------*/
(define (http-request?)
   (and (string? *hostname*) (string? *port*)))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage proc)
   (if (http-request?)
       (print-html
	(html-plugin-usage #f
			   "bglmem"
			   "bglmem [options]"
			   (html-usage :inline*
				       (html-string
					(with-output-to-string
					   (lambda ()
					      (proc #f)))))))
       (begin
	  (print "usage: bglmem [options] name.bmem")
	  (proc #f))))

;*---------------------------------------------------------------------*/
;*    parse-cmdline! ...                                               */
;*---------------------------------------------------------------------*/
(define (parse-cmdline! argv)
   ;; pre setttings
   (set! *uri* (getenv "QUERY_STRING"))
   (set! *hostname* (getenv "SERVER_NAME"))
   (set! *port* (getenv "SERVER_PORT"))
   ;; argument parsing
   (args-parse (cdr argv)
      (("--help" (help "This help message"))
       (usage args-parse-usage)
       (plugin-exit 400))
      ((("-v" "--version") (help "Version number"))
       (if (http-request?)
	   (print-html (html-plugin-version #f *bmem* *bmem-version*))
	   (fprint (current-error-port) *bmem* ": " *bmem-version*))
       (plugin-exit 200))
      ((("-o" "--output") ?fout (help "Output file"))
       (set! *fout* fout))
      ((("-f" "--function") ?function (help "Show the function information"))
       (set! *function* function))
      ((("-t" "--type") ?type (help "Show the type information"))
       (set! *type* type))
      ((("-h" "--hostname") ?h (help "Hostname of the request"))
       (set! *hostname* h))
      ((("-p" "--port") ?p (help "Port number of the request"))
       (set! *port* p))
      (()
       (if (and (not (string? *uri*))
		(string? *hostname*)
		(string? *port*))
	   (begin
	      (usage args-parse-usage)
	      (plugin-exit 400))))
      (else
       (set! *uri* else))))

;*---------------------------------------------------------------------*/
;*    parse-uri! ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-uri! uri)
   (let ((p (pregexp-match "([^?]+)[?]fun=(.+)" uri)))
      (if (pair? p)
	  (begin 
	     (set! *file* (cadr p))
	     (set! *function* (caddr p)))
	  (let ((p (pregexp-match "([^?]+)[?]type=(.+)" uri)))
	     (if (pair? p)
		 (begin
		    (set! *file* (cadr p))
		    (set! *type* (caddr p)))
		 (set! *file* uri))))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (with-exception-handler
      (lambda (e)
	 (if (isa? e &error)
	     (with-access::&error e (obj proc msg)
		(error-notify e)
		(if (http-request?)
		    (print-html
		     (html-plugin-error #f *bmem* obj proc msg)))
		(plugin-exit 500)
		#f))
	 (raise e))
      (lambda ()
	 (parse-cmdline! argv)
	 (unless (string? *uri*)
	    (usage (lambda (_) (exit 0))))
	 (parse-uri! *uri*)
	 (bmem-request))))

;*---------------------------------------------------------------------*/
;*    bmem-request ...                                                 */
;*---------------------------------------------------------------------*/
(define (bmem-request)
   (let* ((mon (if (string? *file*)
		   (with-input-from-file *file* read)
		   (read)))
	  (info (find-monitor mon 'info))
	  (gcmon (find-monitor mon 'gc))
	  (funmon (find-monitor mon 'function))
	  (types (find-monitor mon 'type))
	  (thread (find-monitor mon 'thread))
	  (css (let ((p (append '(".") *css-path* `(,(bigloo-fildir)))))
		  (let ((f (find-file/path "bmem.css" p)))
		     (if (string? f)
			 f
			 #f)))))
      ;; initial check
      (if (or (null? gcmon) (null? (cdr gcmon)))
	  (begin
	     (print-html
	      (html-plugin-error #f
				 "bglmem"
				 'bglmem
				 "Illegal monitor file (empty gc section)"
				 '()))
	     (plugin-exit 500)))
      ;; fetch the word size
      (let ((c (assq 'sizeof-word (cdr info))))
	 (if (pair? c)
	     (set! *sizeof-word* (fixnum->llong (cadr c)))))
      ;; store all the functions
      (for-each add-function! (cdr funmon))
      (let ((doc (cond
		    (*function*
		     (or (bmem-function *function* css info gcmon funmon types)
			 (html-plugin-error #f
					    "bglmem"
					    'bglmem
					    "Can't find function"
					    *function*)))
		    (*type*
		     (or (bmem-type *type* css info gcmon funmon types)
			 (html-plugin-error #f
					    "bglmem"
					    'bglmem
					    "Can't find type"
					    *type*)))
		    (else
		     (bmem-all css info gcmon funmon types thread)))))
	 (if (string? *fout*)
	     (with-output-to-file *fout* (lambda () (print-html doc)))
	     (print-html doc)))))

;*---------------------------------------------------------------------*/
;*    pregexp-case ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (pregexp-case l . clauses)
   (let ((tmp1 (gensym 'pregexp-tmp))
         (tmp2 (gensym 'pregexp-tmp)))
      `(let* ((,tmp1 ,l)
              (,tmp2 '())
              (match (lambda (num)
                        (list-ref ,tmp2 (+fx 1 num)))))
          ,(let loop ((clauses clauses))
              (cond
                 ((null? clauses)
                  #f)
                 ((eq? (caar clauses) 'else)
                  `(begin ,@(cdar clauses)))
                 (else
                  `(begin
                      (set! ,tmp2 (pregexp-match ,(caar clauses) ,tmp1))
                      (if ,tmp2
                          (begin ,@(cdar clauses))
                          ,(loop (cdr clauses))))))))))

;*---------------------------------------------------------------------*/
;*    function-ident-pp ...                                            */
;*---------------------------------------------------------------------*/
(define (function-ident-pp ident)
   (let* ((i (if (symbol? ident) (symbol->string ident) ident)))
      (pregexp-case i
         ;;;		    
	 ("<anonymous:[^:]+:([^:]+):([0-9]+)>"
	  (string-append "&#955; (" (match 0) ":" (match 1) ")"))
	 ("<anonymous:([^>]+)>"
	  (string-append "&#955; (" (match 0) ")"))
	 ("<exit:[^:]+:([^:]+):([0-9]+)>"
	  (string-append "&#923; (" (match 0) ":" (match 1) ")"))
	 ("<exit:([^>]+)>"
	  (string-append "&#923; (" (match 0) ")"))
	 (else
	  (html-string i)))))

;*---------------------------------------------------------------------*/
;*    function-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (function-ref ident)
   (let* ((i (if (symbol? ident) (symbol->string ident) ident))
	  (id (pregexp-case i
                 ;;;		    
		 ("<anonymous:[^:]+:([^:]+):([0-9]+)>"
		    (list "&#955;"
		       (html-span :class "function-index"
			  (list (match 0) ":" (match 1)))))
		 ("<anonymous:([^>]+)>"
		    (list "&#955;"
		       (html-span :class "function-index" (match 0))))
		 ("<exit:[^:]+:([^:]+):([0-9]+)>"
		    (list "&#923;"
		       (html-span :class "function-index"
			  (list (match 0) ":" (match 1)))))
		 ("<exit:([^>]+)>"
		    (list "&#923;"
		       (html-span :class "function-index" (match 0))))
		 (else
		  (html-string i))))
	  (hid (html-div :class "function-ident" :title i id)))
      (if (not (http-request?))
	  hid
	  (let ((url (format "http://~a:~a/~a?fun=~a"
			*hostname*
			*port*
			*file*
			ident)))
	     (html-a :class "function-ref" :href url hid)))))

;*---------------------------------------------------------------------*/
;*    type-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define (type-ref ident)
   (let* ((ident (html-string ident))
	  (hid (html-div :class "type-ident" :title ident ident)))
      (if (not (http-request?))
	  hid
	  (let ((url (format "http://~a:~a/~a?type=~a"
			*hostname*
			*port*
			*file*
			ident)))
	     (html-a :class "type-ref" :href url
		hid)))))

;*---------------------------------------------------------------------*/
;*    bmem-all ...                                                     */
;*---------------------------------------------------------------------*/
(define (bmem-all css info gcmon funmon types thread)
   (let* ((functions (get-functions))
	  (gctable (if (pair? gcmon)
		       (make-gc-tables gcmon functions types)
		       (error 'bmem
			      "Can't find gc monitor"
			      #f)))
	  (funtable (if (pair? funmon)
			(make-function-tables gcmon functions types)
			(error 'bmem
			       "Can't find function monitor"
			       #f)))
	  (typetable (if (pair? types)
			 (make-type-tables gcmon functions types)
			 (error 'bmem
				"Can't find type monitor"
				#f)))
	  (threadtable (if (pair? thread)
			 (make-thread-table thread)
			 ""))
	  (dcss (dynamic-css gcmon functions types))
	  (margin (list (html-h1 (string-append "Bglmem" *bmem-version*))
			(html-h2 "Contents")
			(html-div :class "contents"
				  (html-vbox
				   (html-a :href "#functions" "Functions")
				   (html-a :href "#types" "Types")
				   (html-a :href "#threads" "Threads")
				   (html-a :href "#gcs" "Garbage Collections")))
			(html-h2 "Legends")
			(html-legend
			 1 "90%"
			 (map (lambda (f)
				 (with-access::funinfo f (ident num)
				    (list (format "function~a" num)
					  (function-ref ident))))
			      (sort (filter (lambda (f)
					       (with-access::funinfo f (use)
						  use))
				       functions)
				    (lambda (f1 f2)
				       (with-access::funinfo f1 ((id1 ident))
					  (with-access::funinfo f1 ((id2 ident))
					     (string<?
						(symbol->string id1)
						(symbol->string id2)))))))
			 "Functions" "function-legend")
			(html-br)
			(html-legend
			 1 "90%"
			 (map (lambda (t)
				 (list (format "type~a" (car t))
				       (type-ref (cadr t))))
			      (sort (cdr types)
				    (lambda (t1 t2)
				       (string<? (cadr t1) (cadr t2)))))
			 "Types" "type-legend")
			(html-br)
			(html-legend
			 1 "90%"
			 (map (lambda (t)
				 (list (format "gc~a" (+fx 1 (car t)))
				       (format "gc #~a" (+fx 1 (car t)))))
			      (cdr gcmon))
			 "Gcs" "gc-legend")))
	  (title (if (pair? info)
		     (let ((c (assq 'exec (cdr info))))
			(if (pair? c)
			    (cadr c)
			    "bglmem"))
		     "bglmem"))
	  (page (list (html-h1 '("Statistics"))
		   (make-gc-summary gcmon)
		   (html-br)
		   (html-h1 '("Functions")) (html-a :name "functions")
		   funtable (html-br)
		   (html-h1 '("Types")) (html-a :name "types")
		   typetable (html-br)
		   (html-h1 '("Threads information")) (html-a :name "threads")
		   threadtable (html-br)
		   (html-h1 '("Garbage collections")) (html-a :name "gcs")
		   gctable)))
      (html-document :title (if (string=? title "bglmem")
				"bglmem"
				(string-append "bglmem: " title))
		     :style (list css dcss)
		     :class "hop"
		     (list (html-div :class "bglmem"
				     (list (html-twocolumns
					    :width 20
					    :scroll #t
					    :margin margin
					    :title title
					    :page page)))))))

;*---------------------------------------------------------------------*/
;*    dynamic-css ...                                                  */
;*---------------------------------------------------------------------*/
(define (dynamic-css gcmon function types)
   (define (gc-css gcmon)
      (let loop ((i (length gcmon))
		 (res '()))
	 (if (>=fx i -1)
	     (let* ((selector (format "div.profile .gc~a" i))
		    (color (case i
			      ((-1) "#ccc")
			      ((0) "#ddf")
			      (else (css-color i 117 128 162))))
		    (entry `(,selector background: ,color cursor: help)))
		(loop (-fx i 1) (cons entry res)))
	     (reverse! res))))
   (define (function-css fun)
      (cons `("div.profile .function-1" background: "#ccc" cursor: help)
	 (map (lambda (f)
		 (with-access::funinfo f (num)
		    (let* ((selector (format "div.profile .function~a" num))
			   (color (css-color num 143 255 128)))
		       `(,selector background: ,color cursor: help))))
	    fun)))
   (define (type-css types)
      (cons `("div.profile .type-1" background: "#ccc" cursor: help)
	 (map (lambda (t)
		 (let* ((i (car t))
			(selector (format "div.profile .type~a" i))
			(color (if (string=? (cadr t) "byte")
				   "rgb( 60, 60, 60 )"
				   (css-color i 200 40 80))))
		    `(,selector background: ,color cursor: help)))
	    types)))
   (list->css
    (append (gc-css (cdr gcmon))
	    (function-css function)
	    (type-css (cdr types)))))

;*---------------------------------------------------------------------*/
;*    find-monitor ...                                                 */
;*---------------------------------------------------------------------*/
(define (find-monitor mon key)
   (let loop ((mon mon))
      (cond
	 ((null? mon)
	  #f)
	 ((and (pair? (car mon)) (eq? (car (car mon)) key))
	  (car mon))
	 (else
	  (loop (cdr mon))))))
