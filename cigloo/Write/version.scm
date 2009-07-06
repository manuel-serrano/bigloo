;*=====================================================================*/
;*    serrano/prgm/project/cigloo/Write/version.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 12 14:03:51 1995                          */
;*    Last change :  Fri Dec  1 10:11:31 1995 (serrano)                */
;*    -------------------------------------------------------------    */
;*    La version de Bigloo.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module write_version 
   (import engine_param
	   tools_speek)
   (export (version)
	   (short-version)))

;*---------------------------------------------------------------------*/
;*    short-version ...                                                */
;*---------------------------------------------------------------------*/
(define (short-version)
   (print *cigloo-name* (if (char? *cigloo-level*)
			    (let ((s " (level 0)"))
			       (string-set! s 8 *cigloo-level*)
			       s)
			    "")))

;*---------------------------------------------------------------------*/
;*    version ...                                                      */
;*---------------------------------------------------------------------*/
(define (version)
   (display-to-column "" 79 #\-)
   (newline)
   (cat (string-append *cigloo-name* (if (char? *cigloo-level*)
					(let ((s " (level 0)"))
					   (string-set! s 8 *cigloo-level*)
					   s)
					""))
	"a `C->Bigloo' translator"
	(if (char=? (string-ref *cigloo-date* 0) #\space)
	    (substring *cigloo-date* 1 (string-length *cigloo-date*))
	    *cigloo-date*)
	*cigloo-author*
	"email:"
	*cigloo-email*)
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
      
