;*=====================================================================*/
;*    serrano/prgm/project/bigloo/tools/hglog.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  3 07:22:22 2010                          */
;*    Last change :  Fri Oct 12 14:48:06 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
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
   "(\"{date|shortdate}\" \"{author}\" \"{files}\" \"{desc|urlescape|fill68}\" \"{node|short}\" \"{branches}\")")

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
	 (("--stdin" (help "Read from stdin"))
	  (set! stdin #t))
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
		   (if (skip-entry? desc)
		       (loop (read op) dt)
		       (begin
			  (unless (string=? date dt)
			     (display date)
			     (display "  ")
			     (display author)
;* 			     (newline)                                 */
			     (newline))
;* 			  (display "\t*")                              */
;* 			  (display-list files)                         */
;* 			  (newline)                                    */
			  (for-each (lambda (s)
				       (print "\t" s))
				    (string-split (url-decode desc) "\n"))
			  (display "\t")
			  (unless (string=? branch "")
			     (display* "<" branch "> "))
			  (display* "[" node "]")
			  (newline)
			  (newline)
			  (loop (read op) date))))
		  (else
		   (loop (read op) dt))))))))

;*---------------------------------------------------------------------*/
;*    display-list ...                                                 */
;*---------------------------------------------------------------------*/
(define (display-list files)
   (let ((l (string-split files " ")))
      (when (pair? l)
	 (let loop ((l l)
		    (c 0))
	    (let ((sl (string-length (car l))))
	       (if (>fx (+fx c sl) 68)
		   (begin
		      (display "\n\t")
		      (set! c 0))
		   (display " "))
	       (display (car l))
	       (if (pair? (cdr l))
		   (begin
		      (display ",")
		      (loop (cdr l) (+ c sl 2)))
		   (display ":")))))))

;*---------------------------------------------------------------------*/
;*    skip-entry? ...                                                  */
;*---------------------------------------------------------------------*/
(define (skip-entry? desc)
   (or (string=? desc ".")
       (string=? desc ". (bootstrap)")
       (string=? desc ".%20%28bootstrap%29")
       (string=? desc "bootstrap")
       (string=? desc "")
       (string=? desc " ")
       (string-prefix? "Full%20Bootstrap%20at" desc)))

