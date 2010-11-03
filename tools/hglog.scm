;*=====================================================================*/
;*    serrano/prgm/project/bigloo/tools/hglog.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  3 07:22:22 2010                          */
;*    Last change :  Wed Nov  3 11:31:29 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generate a changelog with HG                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hglog
   (main main))

;*---------------------------------------------------------------------*/
;*    hg-pattern ...                                                   */
;*---------------------------------------------------------------------*/
(define hg-pattern
   "(\"{date|shortdate}\" \"{author}\" ({files}) \"{desc|fill68}\" \"{node|short}\" \"{branches}\")")

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((opts '(output: pipe:))
	 (stdin #f))
      (args-parse (cdr argv)
	 ((("-b" "--branch") ?b (help "select branch (cumulative)"))
	  (set! opts (cons* "-b" b opts)))
	 ((("-r" "--rev") ?r (help "Select a revision"))
	  (set! opts (cons* "-r" r opts)))
	 ((("-" "--stdin") (help "Read from stdin"))
	  (set! opts (cons* "-r" r opts)))
	 (else
	  (print "Illegal argument `" else "'. Usage:")
	  (args-parse-usage #f)))
      (let ((op (if stdin
		    (current-input-port)
		    (let ((p (apply run-process "hg" "log" "--template" hg-pattern opts)))
		       (process-output-port p)))))
	 (let loop ((exp (read op))
		    (dt ""))
	    (unless (eof-object? exp)
	       (match-case exp
		  ((?date ?author ?files ?desc ?node ?branch)
		   (unless (skip-entry? desc)
		      (unless (string=? date dt)
			 (display date)
			 (display "  ")
			 (display author)
			 (newline)
			 (newline))
		      (display "\t*")
		      (display-list files)
		      (newline)
		      (for-each (lambda (s)
				   (print "\t" s))
				(string-split desc "\n"))
		      (print "\t[" node "] <" branch ">\n"))
		   (loop (read op) date))
		  (else
		   (loop (read op) dt))))))))

;*---------------------------------------------------------------------*/
;*    display-list ...                                                 */
;*---------------------------------------------------------------------*/
(define (display-list l)
   (when (pair? l)
      (let loop ((l l)
		 (c 0))
	 (let ((sl (string-length (symbol->string (car l)))))
	    (when (>fx (+fx c sl) 68)
	       (display "\n\t")
	       (set! c 0))
	    (display " ")
	    (display (car l))
	    (if (pair? (cdr l))
		(begin
		   (display ",")
		   (loop (cdr l) (+ c sl 2)))
		(display ":"))))))

;*---------------------------------------------------------------------*/
;*    skip-entry? ...                                                  */
;*---------------------------------------------------------------------*/
(define (skip-entry? desc)
   (or (string=? desc ".")
       (string=? desc ". (bootstrap)")
       (string=? desc "bootstrap")
       (string=? desc "")
       (string=? desc " ")))

