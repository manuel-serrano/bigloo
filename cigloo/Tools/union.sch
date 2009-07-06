;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo0.0/Tools/union.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 30 14:49:50 1994                          */
;*    Last change :  Mon Dec 18 10:29:03 1995 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The macro which defines `union'                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-union ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (define-union name slots . nodes)
   (let ((*debug-mode* #t))
      (define (make-pred-name name)
	 (symbol-append name '?))
      (define (make-pred name nodes)
	 (if *debug-mode*
	     `(define (,(make-pred-name name) o)
		 (or ,@(map (lambda (node)
			       `(,(make-pred-name (cadr node)) o))
			    nodes)))
	     `(define (,(make-pred-name name) o)
		 ;; this hack has _to be left_ (see distrib/build script file)
		 (not #f))))
      (eval
       `(define-macro (,(symbol-append name '-case) exp . clauses)
	   (let ((val (gensym))
		 (find-node-key (lambda (node)
				   (let loop ((nodes ',nodes)
					      (key   0))
				      (cond
					 ((null? nodes)
					  (error ,(string-append
						   (symbol->string name)
						   "-case")
						 "Unknown node"
						 node))
					 ((eq? node (cadr (car nodes)))
					  key)
					 (else
					  (loop (cdr nodes)
						(+fx key 1)))))))) 
	      `(let ((,val ,exp))
		  (if (not (struct? ,val))
		      (error ,(string-append ,(symbol->string name)
					     "-case")
			     ,(string-append "Not an `"
					     ,(symbol->string name)
					     "' node")
			     ,val)
		      (case (struct-ref ,val 0)
			 ,@(let loop ((clauses  clauses)
				      (branches '()))
			      (cond
				 ((null? clauses)
				  (reverse!
				   (cons `(else (error
						 ,(string-append
						   ,(symbol->string name)
						   "-case")
						 "Unrecognized node"
						 (shape ,val)))
					 branches)))
				 ((not (pair? (car clauses)))
				  (error (string-append ,(symbol->string name)
							"-case")
					 "Illegal clause"
					 (car clauses)))
				 ((eq? (car (car clauses)) 'else)
				  (if (null? (cdr clauses))
				      (reverse! (cons `(else
							,@(cdr (car clauses)))
						      branches))
				      (error (string-append
					      ,(symbol->string name)
					      "-case")
					     "Illegal clause"
					     (car clauses))))
				 (else
				  (let* ((clause  (car clauses))
					 (nodes   (car clause))
					 (actions (cdr clause)))
				     (loop (cdr clauses)
					   (cons
					    `(,(map find-node-key nodes)
					      ,@actions)
					    branches))))))))))))
      (let loop ((node* nodes)
		 (def   '())
		 (key   0))
	 (if (null? node*)
	     ;; for-each global slots we declare generic accessors and mutators
	     (let loop ((slots slots)
			(index 1)
			(def   def))
		(if (null? slots)
		    ;; ok, every thing is over, we just return all functions.
		    (cons 'begin
			  (cons (make-pred name nodes)
				def))
		    (loop (cdr slots)
			  (+fx index 1)
			  (cons (if *debug-mode*
				    `(define (,(symbol-append name
							      '-
							      (car slots)) o)
					(if (,(make-pred-name name) o)
					    (struct-ref o ,index)
					    (error
					     ',(symbol-append name
							      '-
							      (car slots))
					     (string-append
					      "Not an instance of:"
					      ,(symbol->string name))
					     o)))
				    `(define (,(symbol-append name
							      '-
							      (car slots)) o)
					(struct-ref o ,index)))
				(cons
				 (if *debug-mode*
				     `(define (,(symbol-append name
							       '-
							       (car slots)
							       '-set!) o v)
					 (if (,(make-pred-name name) o)
					     (struct-set! o ,index v)
					     (error
					      ',(symbol-append name
							       '-
							       (car slots))
					      (string-append
					       "Not an instance of:"
					       ,(symbol->string name))
					      0)))
				     `(define (,(symbol-append name
							       '-
							       (car slots)
							       '-set!) o v)
					 (struct-set! o ,index v)))
				 def)))))
	     (match-case (car node*)
		((define-node ?lname . ?lslots)
		 (loop (cdr node*)
		       (append (list
				`(define-struct ,lname key ,@slots ,@lslots)
				`(define-inline (,(symbol-append name
								 '-
								 lname)
						 ,@slots ,@lslots)
				    (,lname ,key ,@slots ,@lslots)))
			       def)
		       (+fx key 1)))
		(else
		 (error "define-union" "Illegal union" (car node*))))))))

 
