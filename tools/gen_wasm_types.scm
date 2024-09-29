;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/tools/gen_wasm_types.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 27 15:26:41 2024                          */
;*    Last change :  Sat Sep 28 07:11:25 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generate the complete types.wat file                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gen-wasm-types
   (main main))

;*---------------------------------------------------------------------*/
;*    include ...                                                      */
;*---------------------------------------------------------------------*/
(define (include expr root)
   (match-case expr
      ((@include ?file ?key)
       (call-with-input-file (if root (make-file-path root file) file)
	  (lambda (op)
	     (let ((mod (read op)))
		(match-case mod
		   ((module ?- . ?exprs)
		    (filter (lambda (e)
			       (when (pair? e)
				  (or (eq? (car e) key)
				      ;; (?key ...)
				      (when (and (eq? (car e) 'rec)
						 (pair? (cdr e)))
					  ;; (rec (?key ...) ...)
					 (any (lambda (e)
						 (and (pair? e)
						      (eq? (car e) key)))
					    (cdr e))))))
		       exprs))
		   (else
		    (error "gen_wasm_types" "wrong include file" file)))))))
      (else
       (list expr))))

;*---------------------------------------------------------------------*/
;*    generate-types ...                                               */
;*---------------------------------------------------------------------*/
(define (generate-types port root)
   (let ((mod (read port)))
      (match-case mod
	 ((module ?name . ?exprs)
	  (write `(module ,name
		     ,@(append-map (lambda (e) (include e root))
			 exprs))))
	 (else
	  (error "gen_wasm_types" "wrong module" mod)))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (if (pair? (cdr argv))
       (call-with-input-file (cadr argv)
	  (lambda (port)
	     (generate-types port (dirname (cadr argv)))))
       (generate-types (current-input-port) (pwd))))






	   
