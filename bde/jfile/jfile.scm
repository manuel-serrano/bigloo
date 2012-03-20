;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/jfile/jfile.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 10:49:15 1993                          */
;*    Last change :  Tue Mar 20 11:49:24 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Package access file generator.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module jfile (main main))

;*---------------------------------------------------------------------*/
;*    Global variables                                                 */
;*---------------------------------------------------------------------*/
(define *verbose* #f)
(define *suffixes* '("scm" "sch" "bgl"))
(define *gui-suffix* "bld")
(define *package-base* #unspecified)
(define *module-keywords* '(module))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (if (or (null?    (cdr argv))
	   (string=? (cadr argv) "-help"))
       (usage)
       (let loop ((files        (cdr argv))
		  (access-list '())
		  (add-list    '())
		  (output-file '())
		  (path        '(".")))
	  (cond
	     ((null? files)
	      (output access-list add-list output-file path))
	     ((string=? (car files) "-v")
	      (set! *verbose* #t)
	      (loop (cdr files) 
		    access-list
		    add-list
		    output-file
		    path))
	     ((string=? (car files) "-o")
	      (if (null? (cdr files))
		  (usage)
		  (loop (cddr files)
			access-list
			add-list
			(cadr files)
			path)))
	     ((string=? (car files) "-I")
	      (if (null? (cdr files))
		  (usage)
		  (loop (cddr files)
			access-list
			add-list
			output-file
			(cons (cadr files) path))))
	     ((string=? (car files) "-padd")
	      (if (or (null? (cdr files))
		      (null? (cddr files)))
		  (usage)
		  (loop (cdddr files)
			access-list
			(cons (list (cadr files) (caddr files)) add-list)
			output-file
			path)))
	     ((string=? (car files) "-pbase")
	      (if (null? (cdr files))
		  (usage)
		  (begin
		     (set! *package-base* (cadr files))
		     (loop (cddr files)
			   access-list
			   add-list
			   output-file
			   path))))
	     ((or (string=? (car files) "-suffix")
		  (string=? (car files) "--suffix"))
	      (if (null? (cdr files))
		  (usage)
		  (begin
		     (set! *suffixes* (cons (cadr files) *suffixes*))
		     (loop (cddr files)
			   access-list
			   add-list
			   output-file
			   path))))
	     ((string=? (car files) "-gui-suffix")
	      (set! *gui-suffix* (car files))
	      (loop (cddr files)
		    access-list
		    add-list
		    output-file
		    path))
	     ((or (string=? (car files) "-m")
		  (string=? (car files) "--module-keyword"))
	      (if (null? (cdr files))
		  (usage)
		  (begin
		     (set! *module-keywords*
			   (cons (string->symbol (cadr files))
				 *module-keywords*))
		     (loop (cddr files)
			   access-list
			   add-list
			   output-file
			   path))))
	     (else
	      (loop (cdr files)
		    (cons (car files) access-list)
		    add-list
		    output-file
		    path))))))

;*---------------------------------------------------------------------*/
;*    my-open-input-file ...                                           */
;*---------------------------------------------------------------------*/
(define (my-open-input-file file-name)
   (if *verbose*
       (print file-name ":"))
   (open-input-file file-name))

;*---------------------------------------------------------------------*/
;*    make-package-name ...                                            */
;*---------------------------------------------------------------------*/
(define (make-package-name name)
   (let ((name (prefix name)))
      (let loop ((i (-fx (string-length name) 1)))
	 (cond
	    ((=fx i -1)
	     (if (string? *package-base*)
		 (string-append *package-base* "." name)
		 name))
	    ((char=? (string-ref name i) #\/)
	     (string-set! name i #\.)
	     (loop (-fx i 1)))
	    (else
	     (loop (-fx i 1)))))))
	      
;*---------------------------------------------------------------------*/
;*    output ...                                                       */
;*---------------------------------------------------------------------*/
(define (output access-list add-list output-file path)
   (let ((port (if (string? output-file)
		   (begin
		      (if (file-exists? output-file)
			  (rename-file output-file
				       (string-append output-file "~")))
		      (open-output-file output-file))
		   (current-output-port))))
      (fprint port
	      ";; " (pwd) #\Newline
	      ";; " (date) #\Newline #\Newline
	      #\()
      (for-each (lambda (cell)
		   (fprint port "  (" (car cell) " " "\"" (cadr cell) "\")"))
		add-list)
      (let loop ((access-list access-list))
	 (if (null? access-list)
	     (begin
		(fprint port #\) #\Newline)
		(unless (eq? port (current-output-port))
		   (close-output-port port)))
	     (let ((suf (suffix (car access-list))))
		(cond
		   ((member suf *suffixes*)
		    (let ((n (find-module-name (car access-list) path)))
		       (when (symbol? n)
			  (fprint port
			     "  ("
			     n
			     " "
			     #\" (make-package-name (car access-list)) #\" #\)))))
		   ((string=? suf *gui-suffix*)
		    (fprint port
			    "  ("
			    (find-gui-module-name (car access-list) path)
			    " "
			    #\" (make-package-name
				 (string-append (prefix (car access-list))))
			    #\" #\))))
		(loop (cdr access-list)))))))
	  
;*---------------------------------------------------------------------*/
;*    module? ...                                                      */
;*---------------------------------------------------------------------*/
(define (module? s)
   (and (symbol? s) (memq s *module-keywords*)))

;*---------------------------------------------------------------------*/
;*    find-module-name ...                                             */
;*---------------------------------------------------------------------*/
(define (find-module-name fname path)
   (let ((file (find-file/path fname path)))
      (if (or (not (string? file)) (not (file-exists? file)))
	  (begin
	     (fprint (current-error-port) "*** ERROR:bgljfile:"
		     #\Newline
		     "Can't find file -- " fname)
	     'no-such-module)
	  (let ((port (my-open-input-file file)))
	     (if (not (input-port? port))
		 (begin
		    (fprint (current-error-port) "*** ERROR:bgljfile:"
			    #\Newline
			    "Can't open file -- " fname)
		    'no-such-module)
		 (let ((exp (read port)))
		    (match-case exp
		       (((? module?) ?module-name . ?-)
			(close-input-port port)
			module-name)
		       ((directives . ?-)
			(close-input-port port)
			#f)
		       (else
			(close-input-port port)
			(fprint (current-error-port) "*** ERROR:bglfile:"
				#\Newline
				"Illegal file format -- " file)
			#f))))))))

;*---------------------------------------------------------------------*/
;*    find-gui-module-name ...                                         */
;*---------------------------------------------------------------------*/
(define (find-gui-module-name file path)
   (let ((file (find-file/path file path)))
      (if (or (not (string? file)) (not (file-exists? file)))
	  (begin
	     (fprint (current-error-port) "*** ERROR:bglfile:"
		     #\Newline
		     "Can't find file -- " file)
	     'no-such-module)
	  (with-input-from-file file
	     (lambda ()
		(read))))))
   
;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage)
   (print "usage: bgljfile [-I path] [-o output] [-pbase pbase] [-suffix suf] [-gui-suffix suf] file ...")
   (exit -1))
