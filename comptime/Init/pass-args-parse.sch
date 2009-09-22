;*=====================================================================*/
;*    .../prgm/project/bigloo/comptime/Init/pass-args-parse.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 17 22:15:24 2009                          */
;*    Last change :  Thu Sep 17 22:16:53 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Runs parse-args several times over the arguments.                */
;*    each time only the 'selected' clauses are evaluated.             */
;*    default-pass selects all clauses that have no explicit           */
;*    pass identifier. each clause may have a clause-identifier        */
;*    as follows:                                                      */
;*         (("-help2" (help "The exhaustive help message"))            */
;*          (pass 2 (usage args-parse-usage 2 #f)))                    */
;*    passes are compared by 'equal?'. So passes can be numbers,       */
;*    symbols, strings, etc.                                           */
;*                                                                     */
;*    Sample invocation:                                               */
;*     (pass-parse-args (1 2 default) default                          */
;*         args                                                        */
;*         (section "Misc")                                            */
;*         ;; priliminary test                                         */
;*         (pass 1                                                     */
;*          (("-" (help "Read source code on current input channel"))  */
;*           (set! *src-files* (cons 'stdin *src-files*)))             */
;*          (("-x" (help "some x help"))                               */
;*           (do something for x)))                                    */
;*         (pass 2                                                     */
;*          (("-o" ?file (help "Name the output FILE"))                */
;*           (set! *dest* file)))                                      */
;*         ;; help                                                     */
;*         (("?")                                                      */
;*          (usage args-parse-usage 1 #f)))                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    pass-args-parse ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (pass-args-parse passes default-pass args . clauses)
   
   (define *pass* (gensym 'pass))

   ;; else-clauses will not be contained inside an 'else' anymore
   (define (clause->transformed-clause clause pass)
      (match-case clause
	 ;; the else-clause.
	 ((else . ?exprs)
	  `(when (equal? ',pass ,*pass*)
		 ,@exprs))
	 ;; an arg-clause
	 (((and ?arg/help (? pair?)) . ?exprs)
	  `(,arg/help
	    (when (equal? ',pass ,*pass*)
		  ,@exprs)))
	 ;; probably 'section' or something similar
	 (else
	  clause)))

   ;; returned result should be in reverse order.
   (define (pass-clauses->transformed-clauses clauses pass)
      (let loop ((clauses clauses)
		 (res '())
		 (else-clauses '()))
	 (if (null? clauses)
	     (values res else-clauses)
	     (match-case (car clauses)
		((else . ?exprs)
		 (loop (cdr clauses)
		       res
		       (cons (clause->transformed-clause (car clauses) pass)
			     else-clauses)))
		(else
		 (loop (cdr clauses)
		       (cons (clause->transformed-clause (car clauses) pass)
			     res)
		       else-clauses))))))

   (define (clauses->transformed-clauses clauses)
      (let loop ((clauses clauses)
		 (rev-transformed-clauses '())
		 (else-clauses '())) ;; without the 'else part
	 (cond
	    ((and (null? clauses) (null? else-clauses))
	     (reverse! rev-transformed-clauses))
	    ((null? clauses)
	     (append (reverse! rev-transformed-clauses)
		     `((else ,@else-clauses))))
	    (else
	     (match-case (car clauses)
		((else . ?exprs)
		 (loop (cdr clauses)
		       rev-transformed-clauses
		       (cons (clause->transformed-clause
			      (car clauses) default-pass)
			     else-clauses)))
		((pass ?pass . ?nested-clauses)
		 (multiple-value-bind (rev-trs elses)
		    (pass-clauses->transformed-clauses
		     nested-clauses pass)
		    (loop (cdr clauses)
			  (append rev-trs rev-transformed-clauses)
			  (append elses else-clauses))))
		(else
		 (loop (cdr clauses)
		       (cons (clause->transformed-clause
			      (car clauses) default-pass)
			     rev-transformed-clauses)
		       else-clauses)))))))

   (unless (list? passes)
	   (error 'pass-parse-args
		  "first argument must be a list of passes. eg: (1 2 default)"
		  passes))
   
   `(for-each (lambda (,*pass*)
		 (args-parse ,args ,@(clauses->transformed-clauses clauses)))
	      ',passes))
