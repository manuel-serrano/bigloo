;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/html-private.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb  8 08:57:06 2003                          */
;*    Last change :  Wed May 14 14:31:03 2003 (serrano)                */
;*    Copyright   :  2003 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Convenience macros only used in hop_html.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    with-markup ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (with-markup markup attr* margin . body)
   (match-case body
      ((? string?)
       (let ((tmp (gensym))
	     (att (gensym)))
	  `(let ((,tmp ,markup)
		 (,att ,attr*))
	      (newline)
	      (display (make-margin ,margin))
	      (out-omarkup ,tmp ,att)
	      ,@(if (pair? body) body '())
	      (out-cmarkup ,tmp))))
      (()
       (let ((tmp (gensym))
	     (att (gensym)))
	  `(let ((,tmp ,markup)
		 (,att ,attr*))
	      (newline)
	      (display (make-margin ,margin))
	      (out-omarkup ,tmp ,att)
	      (out-cmarkup ,tmp))))
      (else
       (let ((tmp (gensym))
	     (att (gensym))
	     (mar (gensym)))
	  `(let ((,tmp ,markup)
		 (,att ,attr*)
		 (,mar (make-margin ,margin)))
	      (newline)
	      (display ,mar)
	      (out-omarkup ,tmp ,att)
	      ,@body
	      (newline)
	      (display ,mar)
	      (out-cmarkup ,tmp))))))

;*---------------------------------------------------------------------*/
;*    with-oneline-markup ...                                          */
;*---------------------------------------------------------------------*/
(define-macro (with-oneline-markup markup attr* margin . body)
   (let ((tmp (gensym))
	 (att (gensym)))
      `(let ((,tmp ,markup)
	     (,att ,attr*))
	  (newline)
	  (display (make-margin ,margin))
	  (out-omarkup ,tmp ,att)
	  ,@(if (pair? body) body '())
	  (out-cmarkup ,tmp))))
		 
;*---------------------------------------------------------------------*/
;*    with-online-markup ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (with-online-markup markup attr* margin . body)
   (let ((tmp (gensym))
	 (att (gensym)))
      `(let ((,tmp ,markup)
	     (,att ,attr*))
	  (out-omarkup ,tmp ,att)
	  ,@(if (pair? body) body '())
	  (out-cmarkup ,tmp))))
		 
;*---------------------------------------------------------------------*/
;*    make-attributes ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (make-attributes . clause*)
   (let ((tmp (gensym 'attr))
	 (val (gensym 'val))
	 (loop (gensym 'loop)))
      `(let ((,tmp '()))
	  ,@(map (lambda (c)
		    `(if ,(cadr c)
			 (let ((,val ,(caddr c)))
			    (set! ,tmp
				  (cons (string-append
					 ,(car c)
					 "=\""
					 (cond
					    ((string? ,val)
					     ,val)
					    ((number? ,val)
					     (number->string ,val))
					    (else
					     (error "make-attributes"
						    "Illegal attribute value"
						    ,val)))
					 "\"")
					,tmp)))))
		 clause*)
	  ,tmp)))

