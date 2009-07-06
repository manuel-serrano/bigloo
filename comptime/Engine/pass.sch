;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/pass.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 25 10:29:06 1994                          */
;*    Last change :  Mon May 15 07:43:19 2000 (serrano)                */
;*    Copyright   :  1994-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The pass prelude                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives clause                                            */
;*---------------------------------------------------------------------*/
(directives
   (import  tools_speek
	    (*nb-error-on-pass* tools_error)
	    engine_pass))
 
;*---------------------------------------------------------------------*/
;*    pass-prelude ...                                                 */
;*    -------------------------------------------------------------    */
;*    The pass prelude.                                                */
;*---------------------------------------------------------------------*/
(define-macro (pass-prelude name . hooks)
   `(begin
       (verbose 1 "   . " ,name #\newline)
       (set! *nb-error-on-pass* 0)
       (set! *current-pass*  ,name)
       (let loop ((hooks ,(cons 'list hooks))
		  (hnames ',hooks))
	  (cond
	     ((null? hooks)
	      'pass-started)
	     (((car hooks))
	      (loop (cdr hooks) (cdr hnames)))
	     (else
	      (internal-error ,name
			      "failure during prelude hook"
			      (car hnames)))))))

;*---------------------------------------------------------------------*/
;*    pass-postlude ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (pass-postlude value . hooks)
   `(let ((value ,value))
       (if (>fx *nb-error-on-pass* 0)
	   (begin
	      (fprint (current-error-port)
		      *nb-error-on-pass*
		      " error"
		      (if (and (integer? *nb-error-on-pass*)
			       (> *nb-error-on-pass* 1))
			  "s"
			  "")
		      " occured, ending ...")
	      (exit -1))
	   (let loop ((hooks  ,(cons 'list hooks))
		      (hnames ',hooks))
	      (cond
		 ((null? hooks)
		  value)
		 (((car hooks))
		  (loop (cdr hooks) (cdr hnames)))
		 (else
		  (internal-error *current-pass*
				  "failure during postlude hook"
				  (car hnames))))))))
	      
	       

	   
