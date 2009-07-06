;*=====================================================================*/
;*    serrano/prgm/project/bigloo/snowman/param.sch                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Oct  1 05:02:42 2005                          */
;*    Last change :  Wed Dec 13 13:37:55 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The definition of the param macros                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-parameter ...                                             */
;*---------------------------------------------------------------------*/
(define-expander define-parameter
   (lambda (x e)
      (define (expand-define-parameter id default setter)
	 (let ((vid (symbol-append '* id '*))
	       (set (symbol-append id '-set!)))
	    `(begin
		(define ,vid ,default)
		(define (,id) ,vid)
		(define (,set v)
		   ,(if (pair? setter)
			`(set! ,vid (,(car setter) v))
			`(set! ,vid v))
		   v)
		(,set (,id)))))
      (match-case x
	 ((?- ?id ?default . ?setter)
	  (e (evepairify (expand-define-parameter id default setter) x) e))
	 (else
	  (error 'define-expander "Illegal form" x)))))

;*---------------------------------------------------------------------*/
;*    print-parameters                                                 */
;*---------------------------------------------------------------------*/
(define-expander print-parameters
   (lambda (x e)
      (match-case x
	 ((?- . ?params)
	  (let* ((names (map symbol->string params))
		 (len (apply max (map string-length names))))
	     `(begin
		 ,@(map (lambda (n p)
			   `(begin
			       (display ,n)
			       (display ,(make-string (- len (string-length n)) #\space))
			       (display ": ")
			       (display (,p))
			       (newline)))
			names params))))
	 (else
	  (error 'print-parameters "Illegal form" x)))))


