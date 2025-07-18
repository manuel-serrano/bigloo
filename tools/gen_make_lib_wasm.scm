;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/tools/gen_make_lib_wasm.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Jul 17 08:06:57 2025                          */
;*    Last change :  Thu Jul 17 15:03:48 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Generate the wasm heap specific include.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gen_make_list_wasm
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let* ((src (cdr argv))
	  (scm (filter (lambda (s) (string=? (suffix s) "scm")) src))
	  (wat (filter (lambda (s) (string=? (suffix s) "wat")) src))
	  (preds (append-map parse-scm scm))
	  (inl (inlines wat preds)))
      ;; check unfound predicates
      (for-each (lambda (pred)
		   (unless (assq (cdr pred) inl)
		      (fprint (current-error-port) "predicate definition not found: " (cdr pred) " (" (car pred) ")")))
	 preds)
      (print ";; Automatically generated file (" (basename (car argv)) "), " (date))
      (pp `(directives (wasm ,@inl)))))

;*---------------------------------------------------------------------*/
;*    parse-scm ...                                                    */
;*    -------------------------------------------------------------    */
;*     - Read the module clause                                        */
;*     - Search for a pragma declaration                               */
;*     - If found, search for the predicates                           */
;*---------------------------------------------------------------------*/
(define (parse-scm file)
   
   (define (find-extern id extern)
      (let* ((tid (string-append (symbol->string! id) "::"))
	     (len (string-length tid)))
	 (let loop ((clauses (cdr extern)))
	    (when (pair? clauses)
	       (match-case (car clauses)
		  ((macro ?tsym ?args ?name)
		   (if (or (eq? tsym id)
			   (substring=? tid (symbol->string! tsym) len))
		       (cons name id)
		       (loop (cdr clauses))))
		  (else
		   (loop (cdr clauses))))))))
   
   (call-with-input-file file
      (lambda (port)
	 (let* ((mod (read port))
		(prag (assq 'pragma (cddr mod)))
		(ext (assq 'extern (cddr mod))))
	    (if (pair? prag)
		(filter-map (lambda (clause)
			       (when (find (match-lambda 
					      ((predicate-of ?-) #t)
					      (else #f))
					(cdr clause))
				  (find-extern (car clause) ext)))
		   (cdr prag))
		'())))))
	    
;*---------------------------------------------------------------------*/
;*    inlines ...                                                      */
;*---------------------------------------------------------------------*/
(define (inlines wats preds)
   (let loop ((wats wats)
	      (defs '()))
      (if (or (null? preds) (null? wats))
	  defs
	  (let ((ndefs (search-definition (car wats) preds)))
	     (loop (cdr wats) (append defs ndefs))))))

;*---------------------------------------------------------------------*/
;*    search-definition ...                                            */
;*---------------------------------------------------------------------*/
(define (search-definition wat preds)
   (filter-map (match-lambda
		  ((func ?- (export ?name) (param ?id ?-) (result i32)
		      (ref.test ?ty (local.get ?id)))
		   (let ((p (assoc name preds)))
		      (when (pair? p)
			 `(,(cdr p) ,(format "(ref.test ~a ~~0)" ty)))))
		  ((func ?- (export ?name) (param ?id ?-) (result i32)
		      (ref.eq ?var (local.get ?id)))
		   (let ((p (assoc name preds)))
		      (when (pair? p)
			 `(,(cdr p) ,(format "(ref.eq ~a ~~0)" var))))))
      (call-with-input-file wat read)))
   
