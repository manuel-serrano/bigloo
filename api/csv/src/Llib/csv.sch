;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/csv/src/Llib/cvs.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Joseph Donaldson (donaldsonjw@yahoo.com)          */
;*    Creation    :  Fri Feb 24 07:12:29 2012                          */
;*    Last change :  Fri Feb 24 07:15:29 2012 (serrano)                */
;*    Copyright   :  2011-12 Joseph Donaldson                          */
;*    -------------------------------------------------------------    */
;*    This file is part of bigloo-csv.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    make-csv-lexer                                                   */
;*---------------------------------------------------------------------*/
(define-macro (make-csv-lexer sep quot)
   (if (and (char? sep) (char? quot))
       `(lambda (in-quote?)
	   (regular-grammar ((quote ,quot)
			     (separator ,sep))
	      ((when in-quote?
		  (: quote quote))
	       (cons '2quote (string ,quot)))
	      (quote 
		 (begin
		    (set! in-quote? (not in-quote?))
		    (cons 'kwote (the-string))))
	      ,(cond ((and (or (char=? sep #\space)
			      (char=? quot #\space))
			  (or (char=? sep #\tab)
			      (char=? quot #\tab))) 
		     `(define ,(gensym 'dummy) #unspecified))
	       ((or (char=? sep #\space)
		    (char=? quot #\space))
		'((when (not in-quote?) (+ #\tab))
		  (cons 'space (the-string))))
	       ((or (char=? sep #\tab)
		    (char=? quot #\tab))
		'((when (not in-quote?) (+ #\space))
		  (cons 'space (the-string))))
	       (else
		'((when (not in-quote?) (+ (or #\space #\tab)))
		  (cons 'space (the-string)))))
	      (separator
		 'separator)
	      ((or (: #\return #\newline)
		   #\newline)
	       'newline)
	      ((when (not in-quote?)
		  (+ (out quote separator #\return #\newline)))
	       (cons 'text (the-string)))
	      ((when in-quote?
		  (+ (out quote)))
	       (cons 'text (the-string)))
	      (else 
	       (let ((c (the-failure)))
		  (set! in-quote? #f)
		  (if (eof-object? c)
		      c
		      (error "csv-lexer" "Illegal character" c))))))
       (error "csv-lexer"
	  "separator and quote must be a single character"
	  (list sep quot))))
