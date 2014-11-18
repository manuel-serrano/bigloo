;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/assert.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  9 16:41:17 1993                          */
;*    Last change :  Tue Nov 18 10:37:37 2014 (serrano)                */
;*    Copyright   :  1993-2014 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    L'expansion des formes `assert'                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_assert
   (include "Tools/location.sch")
   (import  engine_param
	    backend_backend
	    tools_misc
	    tools_location)
   (export  (expand-assert ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-assert ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-assert x e)
   (match-case x
      ((?- check (and ?vars (? pair?)) . ?body)
       ;; this is the old (1.7) style to introduce assertion
       (let ((new `(assert ,vars ,@body)))
	  (replace! x new)
	  (e x e)))
      ((?- (and ?vars (? list?)) . ?pred)
       (if (or (and (fixnum? *compiler-debug*)
		    (>=fx *compiler-debug* 1)
		    (not *bmem-profiling*))
	       (and (memq 'bdb (backend-debug-support (the-backend)))
		    (fixnum? *bdb-debug*)
		    (>=fx *bdb-debug* 1)))
	   (let ((new (make-one-assert e x vars pred)))
	      (replace! x new))
	   #unspecified))
      (else
       (error #f "Illegal `assert' form" x))))
 
;*---------------------------------------------------------------------*/
;*    make-one-assert ...                                              */
;*---------------------------------------------------------------------*/
(define (make-one-assert e exp vars pred)
   (let* ((pred (if (null? pred)
		    (list #unspecified)
		    pred))
	  (old-pred (dup pred)))
      `(if ,(e (expand-progn pred) e)
	   #unspecified
	   (begin
	      ;; we send all vars to the interpreters
	      ,@(let loop ((vars vars)
			   (defs '()))
		   (if (null? vars)
		       defs
		       (loop (cdr vars)
			     (cons `(define-primop! ',(car vars) ,(car vars))
				   defs))))
	      ,(let ((loc (let ((loc (find-location exp)))
			     (if (location? loc)
				 loc
				 (find-location pred)))))
		  `(notify-assert-fail ',vars
				       ',(cons 'begin old-pred)
				       ,(if (location? loc)
					    `',(cons (location-full-fname loc)
						     (location-pos loc))
					    #f)))))))

;*---------------------------------------------------------------------*/
;*    dup ...                                                          */
;*---------------------------------------------------------------------*/
(define (dup pred)
   (cond
      ((pair? pred)
       (cons (dup (car pred)) (dup (cdr pred))))
      (else
       pred)))


