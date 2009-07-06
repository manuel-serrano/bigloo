;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Sqltiny/sqltiny.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 28 09:15:45 2007                          */
;*    Last change :  Wed Dec 12 17:32:45 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    REPL for sqltiny                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sqltiny
   (library sqlite)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((db (if (pair? (cdr argv))
		 (instantiate::sqltiny (path (cadr argv)))
		 (instantiate::sqltiny))))
      (let loop ((pending ""))
	 (display "sqltiny> ")
	 (let ((line (read-line)))
	    (unless (eof-object? line)
	       (let ((exp (string-append pending line "\n")))
		  (loop (with-handler
			   (lambda (e)
			      (if (and (&io-parse-error? e)
				       (eof-object? (&io-parse-error-obj e)))
				  exp
				  (begin
				     (exception-notify e)
				     "")))
			   (begin
			      (if (string-prefix? ".dump" exp)
				  (dump exp db)
				  (sqlite-map db
				     (lambda l (write l) (newline))
				     exp))
			      "")))))))))

;*---------------------------------------------------------------------*/
;*    dump ...                                                         */
;*---------------------------------------------------------------------*/
(define (dump exp db)
   (if (string=? exp ".dump\n")
       (sqlite-dump db (current-output-port))
       (let* ((s (or (string-index-right exp #\space) -1))
	      (t (if (char=? (string-ref exp (-fx (string-length exp) 2)) #\;)
		     (substring exp (+fx 1 s) (-fx (string-length exp) 2))
		     (substring exp (+fx 1 s) (-fx (string-length exp) 1)))))
	  (sqlite-dump-table db t (current-output-port)))))
