;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/tools/gen_wasm_times.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 27 15:26:41 2024                          */
;*    Last change :  Fri Sep 27 15:31:48 2024 (serrano)                */
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
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (if (pair? (cdr argv))
       (call-with-input-file (cadr argv) generates-types)
       (generate-types (current-input-port))))

;*---------------------------------------------------------------------*/
;*    generate-types ...                                               */
;*---------------------------------------------------------------------*/
(define (generate-types port)
   (let ((mod (read port)))
      (match-case mod
	 ((module ?name . ?exprs)
	  (write `(module ,name ,@(append-map include exprs))))
	 (else
	  (error "gen_wasm_types" "wrong module" mod)))))

;*---------------------------------------------------------------------*/
;*    include ...                                                      */
;*---------------------------------------------------------------------*/
(define (include expr)
   (match-case expr
      ((@include ?file ?key)
       (call-with-input-file file
	  (lambda (op)
	     (match-case op
		((module ?- . ?exprs)
		 (filter (lambda (e)
			    (match-case e
			       ((?type . ?-) e)
			       (else #f)))
		    exprs))
		(else
		 (error "gen_wasm_types" "wrong include file" file))))))
      (else
       (list expr))))




	   
