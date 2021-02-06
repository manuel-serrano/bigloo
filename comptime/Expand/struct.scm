;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/struct.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  4 10:54:57 1995                          */
;*    Last change :  Wed Mar 30 08:32:18 2011 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `define-struct' expansion                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_struct
   (import engine_param
	   tools_misc)
   (export (expand-struct ::obj ::procedure)))
 
;*---------------------------------------------------------------------*/
;*    expand-struct ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-struct x e)
   (match-case x
      ((?- ?name . ?slots)
       (match-define-structure! x)
       (let* ((len        (length slots))
	      (slots-name (map (lambda (s)
				  (match-case s
				     ((?name ?dv)
				      name)
				     ((? symbol?)
				      s)
				     (else
				      (error #f
					     "Illegal `define-struct' form"
					     x))))
			       slots))
	      (slots-val?  #f)
	      (slots-val   (map (lambda (s)
				   (match-case s
				      ((?- ?dv)
				       (set! slots-val? #t)
				       dv)
				      ((? symbol?)
				       ''())
				      (else
				       (error #f
					      "Illegal `define-struct' form"
					      x))))
				slots)))
          (cons
           'begin
           (cons
	    ;; on genere la fonction de create
            (e (epairify
		`(define-inline (,(symbol-append 'make- name) . init)
		    ,(if slots-val?
			 `(if (pair? init)
			      (if (not (null? (cdr init)))
				  (apply ,name init)
				  ((@ make-struct __structure)
				   ',name ,len (car init)))
			      (,name ,@slots-val))
			 `(if (pair? init)
			      (if (not (null? (cdr init)))
				  (apply ,name init)
				  ((@ make-struct __structure)
				   ',name ,len (car init)))
			      ((@ make-struct __structure)
			       ',name ,len '()))))
		x)
               e)
            (cons
             (e (epairify
		 `(define-inline (,name ,@slots-name)
		     (let ((new ($create-struct ',name ,len)))
			,@(let loop ((slots slots-name)
				     (res   '()))
			     (if (null? slots)
				 res
				 (loop (cdr slots)
				       (cons `(,(symbol-append name
							       '-
							       (car slots)
							       '-set!)
					       new
					       ,(car slots))
					     res))))
			new))
		 x)
		e)
             (cons
              ;; on genere le predicat STRUCT?
              (e (epairify
		  `(define-inline (,(symbol-append name '?) o)
		      (if (struct? o)
			  (eq? ((@ struct-key __structure) o) ',name)
			  #f))
		  x)
                 e)
              ;; on genere les fonctions d'access aux slots
              (let loop ((i     0)
                         (slots slots-name)
                         (res   '((unspecified))))
                 (if (=fx i len)
                     res
                     (let ((pr (car slots)))
                        (loop (+fx i 1)
                              (cdr slots)
                              (cons
                               ;; la lecture
                               (e
                                (epairify
				 `(define-inline
				     (,(symbol-append name '- pr) s)
				     ,(if *unsafe-struct*
					  `((@ struct-ref __structure) s ,i)
					  `(if (eq? ((@ struct-key __structure) s) ',name)
					       (u-struct-ref s ,i)
					       (error
						"struct-ref:not an instance of"
						,(symbol->string name)
						s))))
				 x)
                                e)
                               (cons
                                ;; l'ecriture
                                (e
                                 (epairify
				  `(define-inline
				      (,(symbol-append name '- pr '-set!) s v)
				      ,(if *unsafe-struct*
					   `((@ struct-set! __structure) s ,i v)
					   `(if (eq? ((@ struct-key __structure) s) ',name)
						(u-struct-set! s ,i v)
						(error
						 "struct-set!:not an instance of"
						 ,(symbol->string name)
						 s))))
				  x)
                                 e)
                                res))))))))))))
      (else
       (error #f "Illegal `define-struct' form" x))))
 
