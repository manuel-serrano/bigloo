;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Coerce/app.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 11:51:05 1995                          */
;*    Last change :  Sun Feb 11 08:57:27 2018 (serrano)                */
;*    Copyright   :  1995-2018 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    A little module which implement application arity checks.        */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_app
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    tools_error
	    type_type
	    type_cache
	    type_typeof
	    ast_var
	    ast_node
	    coerce_coerce
	    coerce_convert))

;*---------------------------------------------------------------------*/
;*    coerce! ::app ...                                                */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::app caller to safe)
   (trace (coerce 2) "coerce-app!: " (shape node) " -> " (shape to)
	  #\Newline)
   (let ((fun (var-variable (app-fun node))))
      (if (and (global? fun) (cfun? (variable-value fun)))
	  (coerce-foreign-app! fun caller node to safe)
	  (coerce-bigloo-app! fun caller node to safe))))

;*---------------------------------------------------------------------*/
;*    coerce-foreign-app! ...                                          */
;*---------------------------------------------------------------------*/
(define (coerce-foreign-app! callee::variable caller node to safe)
   (trace (coerce 2) "coerce-foreign-app!: " (shape node) " -> " (shape to)
	  #\Newline)
   (let* ((ffun  (variable-value callee))
	  (arity (fun-arity ffun)))
      (if (>=fx arity 0)
	  (coerce-foreign-fx-app! ffun callee caller node to safe)
	  (coerce-foreign-va-app! ffun callee caller node to safe))))

;*---------------------------------------------------------------------*/
;*    make-procedure-ids ...                                           */
;*---------------------------------------------------------------------*/
(define make-procedure-ids
   '(make-fx-procedure make-va-procedure make-l-procedure))

;*---------------------------------------------------------------------*/
;*    coerce-foreign-fx-app! ...                                       */
;*---------------------------------------------------------------------*/
(define (coerce-foreign-fx-app! fun callee::variable caller node to safe)
   (define (coerce-args! args types)
      (let loop ((actuals args)
		 (types   types))
	 (when (pair? actuals)
	    (set-car! actuals (coerce! (car actuals) caller (car types) safe))
	    (loop (cdr actuals) (cdr types)))))
   (define (coerce-procedure args types loc)
      (coerce-args! (cdr args) (cdr types))
      (let ((clo (car args)))
	 (if (var? clo)
	     (unless (sfun? (variable-value (var-variable clo)))
		(user-error/location loc
				     (variable-name (var-variable clo))
				     (shape (variable-type (var-variable clo)))
				     "procedure"))
	     (set-car! args (coerce! clo caller *procedure* safe)))))
   (with-access::app node (args loc)
      ;; make-XXX-procedure are special, their first
      ;; argument is not really typable because it's not a Bigloo correct
      ;; value (in C it's a pointer to function).
      (if (memq (global-id callee) make-procedure-ids)
	  (coerce-procedure args (cfun-args-type fun) loc)
	  (coerce-args! args (cfun-args-type fun))))
   (convert! node (get-type node #f) to safe))

;*---------------------------------------------------------------------*/
;*    coerce-foreign-va-app! ...                                       */
;*---------------------------------------------------------------------*/
(define (coerce-foreign-va-app! fun callee::variable caller node to safe)
   (let loop ((actuals (app-args node))
	      (types   (cfun-args-type fun))
	      (counter (fun-arity fun)))
      (if (=fx counter -1)
	  ;; this is the formals of a foreign va-args
	  (let loop ((actuals actuals))
	     (if (null? actuals)
		 (convert! node (get-type node #f) to safe)
		 (begin
		    (set-car! actuals (coerce! (car actuals)
					       caller
					       (car types)
					       safe))
		    (loop (cdr actuals)))))
	  (begin
	     (set-car! actuals (coerce! (car actuals)
					caller
					(car types)
					safe))
	     (loop (cdr actuals) (cdr types) (+fx counter 1))))))

;*---------------------------------------------------------------------*/
;*    coerce-bigloo-app! ...                                           */
;*---------------------------------------------------------------------*/
(define (coerce-bigloo-app! callee::variable caller node to safe)
   (trace (coerce 2) "coerce-bigloo-app!: " (shape node) " "
	  (shape (variable-type callee))
	  "(" (shape (node-type node)) ") -> " (shape to) #\Newline)
   (if (and (global? callee)
	    (eq? (global-import callee) 'import)
	    (pair? (sfun-args (variable-value callee)))
	    (type? (car (sfun-args (variable-value callee)))))
       (coerce-bigloo-extern-app! callee caller node to safe)
       (coerce-bigloo-intern-app! callee caller node to safe)))

;*---------------------------------------------------------------------*/
;*    coerce-bigloo-intern-app! ...                                    */
;*---------------------------------------------------------------------*/
(define (coerce-bigloo-intern-app! callee::variable caller node to safe)
   (trace (coerce 2) "coerce-bigloo-intern-app!: " (shape node) " "
	  (shape (variable-type callee))
	  "(" (shape (node-type node)) ") -> " (shape to) #\Newline)
   (let* ((fun   (variable-value callee))
	  (arity (sfun-arity fun))
	  (sh    (shape callee))
	  (ntype (get-type node #f)))
      (let loop ((actuals (app-args node))
		 (formals (sfun-args fun)))
	 (unless (=fx (length actuals) (length formals))
	    (internal-error "app" "formals/actuals mismatch"
	       (shape node))
	    (exit -1))
	 (assert (actuals formals sh) (=fx (length actuals) (length formals)))
	 (if (null? actuals)
	     (if (and (eq? caller callee) (not *unsafe-type*))
		 ;; As suggested by J.G Malecki, there is no need to
		 ;; type check the result of a self recursive call.
		 ;; Local are always tail-called (otherwise they would have
		 ;; been globalized).
		 (convert! node ntype to #f)
		 (convert! node ntype to safe))
	     (let ((type (local-type (car formals))))
		(set-car! actuals (coerce! (car actuals) caller type safe))
		(loop (cdr actuals) (cdr formals)))))))

;*---------------------------------------------------------------------*/
;*    coerce-bigloo-extern-app! ...                                    */
;*---------------------------------------------------------------------*/
(define (coerce-bigloo-extern-app! callee::variable caller node to safe)
   (let* ((fun (variable-value callee))
	  (arity (sfun-arity fun))
	  (ntype (get-type node #f)))
      (let loop ((actuals (app-args node))
		 (formals (sfun-args fun)))
	 (if (null? actuals)
	     (convert! node ntype to safe)
	     (let ((type (car formals)))
		(set-car! actuals (coerce! (car actuals) caller type safe))
		(loop (cdr actuals) (cdr formals)))))))
