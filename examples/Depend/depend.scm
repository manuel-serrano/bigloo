;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/examples/Depend/depend.scm           */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 10:49:15 1993                          */
;*    Last change :  Thu Sep 26 09:47:28 1996 (serrano)                */
;*                                                                     */
;*    On genere des dependances (d'apres les includes).                */
;*---------------------------------------------------------------------*/
 
;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module afile (main main))

;*---------------------------------------------------------------------*/
;*    *print-dependence* ...                                           */
;*---------------------------------------------------------------------*/
(define *print-dependence* print-make-dependence)

;*---------------------------------------------------------------------*/
;*    *prefix-dir*                                                     */
;*---------------------------------------------------------------------*/
(define *prefix-dir* "")

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (if (or (null?    (cdr argv))
	   (string=? (cadr argv) "-help"))
       (usage)
       (let loop ((files        (cdr argv))
		  (files-list  '())
		  (output-file '()))
	  (cond
	     ((null? files)
	      (output files-list output-file))
	     ((string=? (car files) "-o")
	      (if (null? (cdr files))
		  (usage)
		  (loop (cddr files)
			files-list
			(cadr files))))
	     ((string=? (car files) "-rmake")
	      (set! *print-dependence* print-rmake-dependence)
	      (loop (cdr files)
		    files-list
		    output-file))
	     ((string=? (car files) "-make")
	      (set! *print-dependence* print-make-dependence)
	      (loop (cdr files)
		    files-list
		    output-file))
	     ((string=? (car files) "-dir")
	      (if (null? (cdr files))
		  (usage)
		  (let ((dir (cadr files)))
		     (if (not (char=? (string-ref dir (-fx (string-length dir)
							   1))
				      #\/))
			 (set! *prefix-dir* (string-append dir "/"))
			 (set! *prefix-dir* dir))
		     (loop (cddr files)
			   files-list
			   output-file))))
	     (else
	      (loop (cdr files)
		    (cons (car files) files-list)
		    output-file))))))

;*---------------------------------------------------------------------*/
;*    output ...                                                       */
;*---------------------------------------------------------------------*/
(define (output files-list output-file)
   (let ((port (if (string? output-file)
		   (begin
		      (if (file-exists? output-file)
			  (rename-file output-file
				       (string-append output-file "~")))
		      (open-output-file output-file))
		   (current-output-port))))
      (let loop ((files-list files-list))
	 (if (null? files-list)
	     (newline port)
	     (let* ((file-name (car files-list))
		    (key       (gensym))
		    (includes  (find-includes key file-name #t)))
		(if (not (null? includes))
		    (*print-dependence* port file-name includes))
		(loop (cdr files-list)))))))
	  
;*---------------------------------------------------------------------*/
;*    print-make-dependence ...                                        */
;*---------------------------------------------------------------------*/
(define (print-make-dependence port file-name includes)
   (display (string-append (remove-extansion file-name) ".o") port)
   (display #\: port)
   (for-each (lambda (i)
		(display #\space port)
		(display (string-append *prefix-dir* i) port))
	     includes)
   (newline port))

;*---------------------------------------------------------------------*/
;*    print-rmake-dependence ...                                       */
;*---------------------------------------------------------------------*/
(define (print-rmake-dependence port file-name includes)
   (fprint port ";; " file-name)
   (display "(set-depend! " port)
   (write file-name port)
   (display " '" port)
   (write (map (lambda (f) (string-append *prefix-dir* f)) includes) port)
   (fprint port ")"))

;*---------------------------------------------------------------------*/
;*    find-includes ...                                                */
;*---------------------------------------------------------------------*/
(define (find-includes key file w/error)
   (cond
      ((not (file-exists? file))
       (fprint (current-error-port) "*** ERROR:depend:" #\Newline
	       "Can't find file -- " file)
       '())
      ((eq? (getprop (string->symbol file) 'include) key)
       '())
      (else
       ;; on marque que le fichier a ete examine
       (putprop! (string->symbol file) 'include key)
       (let ((port (open-input-file file)))
	  (if (not (input-port? port))
	      (begin
		 (fprint (current-error-port) "*** ERROR:depend:" #\Newline
			 "Can't open file -- " file)
		 '())
	      (let ((exp (read port)))
		 (match-case exp
		    ((or (module ?- . ?clauses)
			 (directives . ?clauses))
		     (let loop ((clauses  clauses)
				(includes '()))
			(if (null? clauses)
			    (begin
			       (close-input-port port)
			       includes)
			    (if (eq? (car (car clauses)) 'include)
				(loop (cdr clauses)
				      (append (cdr (car clauses))
					      (apply append
						     (map
						      (lambda (f)
							 (find-includes key
									f
									#f))
						      (cdr (car clauses))))
					      includes))
				(loop (cdr clauses)
				      includes)))))
		    (else
		     (close-input-port port)
		     (if w/error
			 (begin
			    (fprint (current-error-port) "*** ERROR:depend:"
				    #\Newline
				    "Illegal file format -- " file)
			    '())
			 '())))))))))
	     
;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage)
   (print "usage: depend [-rmake/-make] [-o output] <file1> <file2> ... <filen>")
   (exit -1))

;*---------------------------------------------------------------------*/
;*    remove-extansion ...                                             */
;*---------------------------------------------------------------------*/
(define (remove-extansion string)
   (let ((len (-fx (string-length string) 1)))
      (let loop ((e len)
                 (s len))
         (cond
            ((=fx s 0)
             (substring string 0 (+fx 1 e)))
            (else
             (if (and (eq? (string-ref string s) #\.)
                      (=fx e len))
                 (loop (-fx s 1) (- s 1))
                 (loop e (-fx s 1))))))))
