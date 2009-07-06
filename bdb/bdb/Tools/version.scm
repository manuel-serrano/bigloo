;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Tools/version.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 12 14:03:51 1995                          */
;*    Last change :  Fri Oct 29 08:50:43 1999 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Bdb version display                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module tools_version 
   (import engine_param
	   tools_speek
	   tools_io)
   (export (version)
	   (short-version)))

;*---------------------------------------------------------------------*/
;*    console-print ...                                                */
;*---------------------------------------------------------------------*/
(define (console-print . args)
   (for-each console-echo args)
   (console-newline))

;*---------------------------------------------------------------------*/
;*    short-version ...                                                */
;*---------------------------------------------------------------------*/
(define (short-version)
   (console-print *bdb-name* (if (char? *bdb-level*)
				 (let ((s (string-copy " (level 0)")))
				    (string-set! s 8 *bdb-level*)
				    s)
				 "")))

;*---------------------------------------------------------------------*/
;*    version ...                                                      */
;*---------------------------------------------------------------------*/
(define (version)
   (display-to-column "" 79 #\-)
   (newline)
   (cat (string-append *bdb-name* (if (char? *bdb-level*)
				      (let ((s (string-copy " (level 0)")))
					 (string-set! s 8 *bdb-level*)
					 s)
				      ""))
	"the Bigloo debugger"
	(if (char=? (string-ref *bdb-date* 0) #\space)
	    (substring *bdb-date* 1 (string-length *bdb-date*))
	    *bdb-date*)
	*bdb-author*
	"email:"
	*bdb-email*)
   (display-to-column "" 79 #\-)
   (newline)
   (print "                         *** THIS IS AN ALPHA RELEASE ***")
   (print "If it fails, if it does not show accurate informations, please, be tolerant.")
   (display-to-column "" 79 #\-)
   (newline)
   (newline))

;*---------------------------------------------------------------------*/
;*    cat ...                                                          */
;*---------------------------------------------------------------------*/
(define (cat . l)
   (let loop ((l     l)
	      (cat '("   (\"`-/\")_.-'\"``-._"
		     "    . . `; -._    )-;-,_`)"
		     "    v   '  _  )`-.\\  ``-'"
		     "  _.- _..-_/ / ((.'"
		     "((,.-'   ((,/"
		     "")))
      (cond
	 ((null? l)
	  (if (null? cat)
	      'done
	      (begin
		 (display-to-column "" 53 #\space)
		 (print (car cat))
		 (loop '() (cdr cat)))))
	 ((null? cat)
	  (print (car l))
	  (loop (cdr l) '()))
	 (else
	  (display-to-column (car l) 53 #\space)
	  (print (car cat))
	  (loop (cdr l) (cdr cat))))))

;*---------------------------------------------------------------------*/
;*    display-to-column ...                                            */
;*---------------------------------------------------------------------*/
(define (display-to-column string column char)
   (display string)
   (let loop ((l (+fx 1 (string-length string))))
      (if (=fx l column)
	  'done
	  (begin
	     (write-char char)
	     (loop (+fx l 1))))))   
      
