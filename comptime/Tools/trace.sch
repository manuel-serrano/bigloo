;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Tools/trace.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 31 08:56:22 1993                          */
;*    Last change :  Mon Jun 27 14:34:00 2022 (serrano)                */
;*    Copyright   :  1993-2022 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The tracing macro.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Les imports indispensables pour pouvoir tracer                   */
;*---------------------------------------------------------------------*/
(directives
   (import tools_trace))

;*---------------------------------------------------------------------*/
;*    trace ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (trace mask . forms)
   (let ((*debug-mode* #t))
      (let ((*pass-names* '(ast heap inline inline+ cfa cc effect effect+ expand
			    globalize integrate coerce cnst cgen reduce
			    reduce- reduce+ recovery egen jvmas init
			    make-add-heap make-heap fail return)))
	 (if *debug-mode*
	     (match-case mask
		((? symbol?)
		 (if (eq? mask 'get-pass-names)
		     `',*pass-names*
		     (if (memq mask *pass-names*)
			 `(if (trace-satisfy? ',mask 0)
			      (print-trace ,@forms))
			 (error #f "Illegal \"trace\" expression" mask))))
		(((and ?pass (? symbol?)) (and ?level (? integer?)))
		 (if (memq pass *pass-names*)
		     `(if (trace-satisfy? ',pass ,level)
			  (print-trace ,@forms))
		     (error #f "Illegal \"trace\" expression" mask)))
		((??- (? integer?))
		 (let* ((ksam (reverse mask))
			(level (car ksam))
			(rest (cdr ksam)))
		    (if (any (lambda (s) (memq s *pass-names*))  rest)
			`(if (or ,@(map (lambda (p)
					  `(trace-satisfy? ',p ,level))
				       rest))
			     (print-trace ,@forms))
			(error #f "Illegal \"trace\" expression" mask))))
		(else
		 (error #f "Illegal \"trace\" expression" mask)))
	     ''()))))

;*---------------------------------------------------------------------*/
;*    on-trace ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (on-trace mask . forms)
   (let ((*debug-mode* #t))
      (let ((*pass-names* '(ast heap inline inline+ cfa cc effect effect+ expand
			    globalize integrate coerce cnst cgen reduce
			    reduce- reduce+ recovery egen jvmas init)))
	 (if *debug-mode*
	     (match-case mask
		((? symbol?)
		 (if (eq? mask 'get-pass-names)
		     `',*pass-names*
		     (if (memq mask *pass-names*)
			 `(if (trace-satisfy? ',mask 0)
			      (begin ,@forms))
			 (error #f "Illegal \"on-trace\" expression" mask))))
		(((and ?pass (? symbol?)) (and ?level (? integer?)))
		 (if (memq pass *pass-names*)
		     `(if (trace-satisfy? ',pass ,level)
			  (begin ,@forms))
		     (error #f "Illegal \"on-trace\" expression" mask)))
		((??- (? integer?))
		 (let* ((ksam (reverse mask))
			(level (car ksam))
			(rest (cdr ksam)))
		    (if (any (lambda (s) (memq s *pass-names*))  rest)
			`(if (or ,@(map (lambda (p)
					  `(trace-satisfy? ',p ,level))
				       rest))
			     (begin ,@forms))
			(error #f "Illegal \"on-trace\" expression" mask))))
		(else
		 (error #f "Illegal \"on-trace\" expression" mask)))
	     ''()))))

