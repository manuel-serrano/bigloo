;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pkgcomp/src/Llib/class.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 12 05:53:24 2006                          */
;*    Last change :  Tue Apr 24 07:38:18 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The @record->class compiler                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    record-name? ...                                                 */
;*---------------------------------------------------------------------*/
(define (record-name? n)
   (symbol? n))

;*---------------------------------------------------------------------*/
;*    field-name? ...                                                  */
;*---------------------------------------------------------------------*/
(define (field-name? n)
   (symbol? n))

;*---------------------------------------------------------------------*/
;*    name? ...                                                        */
;*---------------------------------------------------------------------*/
(define (name? n)
   (symbol? n))

;*---------------------------------------------------------------------*/
;*    record->class ...                                                */
;*---------------------------------------------------------------------*/
(define (record->class form)
   (match-case form
      ((?- (and ?name (? record-name?)) . ?body)
       (let loop ((body body)
		  (parent (pkgcomp-root-record))
		  (final #f)
		  (abs #f)
		  (fields '()))
	  (cond
	     ((null? body)
	      (let ((cname (symbol-append name '|::| parent))
		    (key (cond
			    (final 'final-class)
			    (abs 'abstract-class)
			    (else 'class))))
		 (evepairify `(,key ,cname ,@(reverse! fields)) form)))
	     ((not (pair? body))
	      (error 'class "Illegal form" form))
	     ((keyword? (car body))
	      (if (null? (cdr body))
		  (error 'class "Illegal form" form)
		  (case (car body)
		     ((sealed:)
		      (loop (cddr body) parent (cadr body) abs fields))
		     ((parent:)
		      (loop (cddr body) (cadr body) final abs fields))
		     (else
		      (loop (cddr body) parent final abs fields)))))
	     (else
	      (match-case (car body)
		 ((? field-name?)
		  (loop (cdr body) parent final abs (cons (car body) fields)))
		 (else
		  (error 'class "Illegal form" form)))))))))

;*---------------------------------------------------------------------*/
;*    @class->class ...                                                */
;*---------------------------------------------------------------------*/
(define (@class->class form)
   form)
