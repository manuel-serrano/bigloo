;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Module/pragma.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  7 08:44:07 1996                          */
;*    Last change :  Thu May  9 09:38:02 2019 (serrano)                */
;*    Copyright   :  1996-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The pragma clause compilation                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_pragma
   (import module_module
	   module_eval
	   tools_error
	   tools_shape
	   type_type
	   engine_param
	   ast_var
	   ast_env
	   ast_remove
	   ast_node
	   type_env
	   effect_feffect
	   (find-location tools_location))
   (export (make-pragma-compiler)
	   (pragma-finalizer)))

;*---------------------------------------------------------------------*/
;*    make-pragma-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-pragma-compiler)
   (instantiate::ccomp
      (id 'pragma)
      (producer (pragma-producer *module*))
      (consumer (lambda (m c) ((pragma-producer m) c)))
      (finalizer pragma-finalizer)))

;*---------------------------------------------------------------------*/
;*    pragma-producer ...                                              */
;*---------------------------------------------------------------------*/
(define (pragma-producer module)
   (lambda (clause)
      (match-case clause
	 ((?- . ?protos)
	  (for-each (lambda (proto)
		       (pragma-parser proto module clause))
		    protos)
	  '())
	 (else
	  (user-error "pragma" "Illegal clause" clause '())))))
   
;*---------------------------------------------------------------------*/
;*    pragma-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (pragma-parser proto module clause)
   (match-case proto
      (((and ?id (? symbol?)) . ?prop)
       (set! *pragma-list*
	  (let ((prag (if (epair? proto)
			  (econs id (list module prop clause) (cer proto))
			  (list id module prop clause))))
	     (cons prag *pragma-list*))))
      (else
       (user-error "pragma" "Illegal clause" clause '()))))

;*---------------------------------------------------------------------*/
;*    *pragma-list* ...                                                */
;*---------------------------------------------------------------------*/
(define *pragma-list* '())

;*---------------------------------------------------------------------*/
;*    pragma-finalizer ...                                             */
;*---------------------------------------------------------------------*/
(define (pragma-finalizer)
   (for-each (lambda (pragma)
		(match-case pragma
		   ((?id ?module ?prop* ?clause)
		    (let ((global (let ((global (find-global/module id module)))
				     (if (global? global)
					 global
					 (find-global/module id 'foreign)))))
		       (if (not (global? global))
			   (when (eq? module *module*)
			      (user-warning/location
				 (find-location pragma)
				 "pragma"
				 "Can't find global variable for pragma"
				 `(@ ,id ,module)))
			   (set-pragma-properties! global prop* clause))))
		   (else
		    (internal-error "pragma-finalizer"
				    "Illegal \"pragma\" finalizer form"
				    pragma))))
	     *pragma-list*)
   (set! *pragma-list* '())
   'void)

;*---------------------------------------------------------------------*/
;*    set-pragma-properties! ...                                       */
;*---------------------------------------------------------------------*/
(define (set-pragma-properties! global prop* clause)
   (for-each (lambda (prop)
		(set-pragma-property! global prop clause))
	     prop*))
 
;*---------------------------------------------------------------------*/
;*    set-pragma-property! ...                                         */
;*---------------------------------------------------------------------*/
(define (set-pragma-property! global prop clause)
   (define (sfun-error p::bstring g::global)
      (if (not *all-export-mutable?*)
	  (user-error (string-append "pragma(" p ")")
	     "property is not concerning a function"
	     (shape g)
	     '())))
   (match-case prop
      ((? symbol?)
       (case prop
	  ;; the side-effect-free pragma
	  ((side-effect-free)
	   (let ((val (global-value global)))
	      (if (not (fun? val))
		  (sfun-error "side-effect-free" global)
		  (fun-side-effect-set! val #f))))
	  ;; the no-cfa-top pragma
	  ((no-cfa-top)
	   (let ((val (global-value global)))
	      (if (not (fun? val))
		  (sfun-error "no-cfa-top" global)
		  (fun-top?-set! val #f))))
	  ;; don't trace the argument of the function during
	  ;; the initflow analysis
	  ((no-init-flow)
	   (let ((val (global-value global)))
	      (if (not (fun? val))
		  (sfun-error "no-init-flow" global)
		  (sfun-property-set! val (cons prop (sfun-property val))))))
	  ;; this function is a generated allocator. That property is
	  ;; used when emitting symbol tables for profiling
	  ((allocator)
	   (let ((val (global-value global)))
	      (if (not (sfun? val))
		  (sfun-error "allocator" global)
		  (sfun-property-set! val (cons prop (sfun-property val))))))
	  ((no-trace)
	   (let ((val (global-value global)))
	      (if (not (sfun? val))
		  (sfun-error "no-trace" global)
		  (sfun-property-set! val (cons prop (sfun-property val))))))
	  ((nesting)
	   (let ((val (global-value global)))
	      (if (cfun? val)
		  (global-pragma-set! global
		     (cons 'nesting (global-pragma global))))))
	  ((args-safe)
	   (let ((val (global-value global)))
	      (if (cfun? val)
		  (global-pragma-set! global
		     (cons 'args-safe
			(global-pragma global))))))
	  ((fail-safe)
	   (let ((val (global-value global)))
	      (if (or (sfun? val) (cfun? val))
		  (global-pragma-set! global
		     (cons 'fail-safe
			(global-pragma global))))))
	  ((no-alloc)
	   ;; that function does not allocate anything
	   (let ((val (global-value global)))
	      (if (or (sfun? val) (cfun? val))
		  (global-pragma-set! global
		     (cons 'no-alloc
			(global-pragma global))))))
	  ((default-inline)
	   ;; inline when used in class field default value
	   ;; see (@ instantiate-fill expand_object)
	   (let ((val (global-value global)))
	      (when (or (sfun? val) (cfun? val))
		 (global-pragma-set! global
		    (cons 'default-inline
		       (global-pragma global))))))
	  ((thread-local)
	   ;; thread local variable
	   (let ((val (global-value global)))
	      (unless (or (sfun? val) (cfun? val))
		 (global-pragma-set! global
		    (cons 'thread-local
		       (global-pragma global))))))
	  (else
	   (user-error "Parse error" "Illegal \"pragma\" form" clause '()))))
      (((and (? symbol?) ?key) . ?val)
       (case key
	  ;; the predicate-of pragma
	  ((predicate-of)
	   (if (not (and (pair? val) (symbol? (car val))))
	       (user-error "Parse error" "Illegal \"predicate-of\" pragma" prop)
	       (let ((type  (use-type! (car val) (find-location prop)))
		     (value (global-value global)))
		  (if (not (fun? value))
		      (sfun-error "predicate-of" global)
		      (begin
			 (fun-predicate-of-set! value type)
			 ;; predicate cannot be remove until
			 ;; coercers introduction pass
			 (remove-var-from! 'coerce global)
			 ;; furthermore a predicate is _always_
			 ;; free of side effects
			 (fun-side-effect-set! value #f))))))
	  ((effect)
	   (let ((value (global-value global)))
	      (if (not (fun? value))
		  (sfun-error "effect" global)
		  (fun-effect-set! value (parse-effect prop)))))
	  ((stack-allocator)
	   (let ((value (global-value global)))
	      (if (not (fun? value))
		  (sfun-error "stack-allocator" global)
		  (fun-stack-allocator-set! value val))))
	  ((args-noescape)
	   ;; the nth argument does not escape
	   (let ((value (global-value global)))
	      (if (not (fun? value))
		  (sfun-error "args-noescape" global)
		  (with-access::fun value (args-noescape)
		     (cond
			((null? val)
			 (set! args-noescape '*))
			((integer? (car val))
			 (set! args-noescape (cons val args-noescape)))
			((symbol? (car val))
			 (if (not (sfun? value))
			     (user-error "Parse error" "Illegal \"args-noescape\" on non-function value" prop)
			     (let loop ((i 0)
					(args (sfun-args-name value)))
				(cond
				   ((null? args)
				    (user-error "Parse error"
				       "Illegal \"args-noescape\", cannot find argument"
				       prop))
				   ((eq? (car args) (car val))
				    (set! args-noescape (cons i args-noescape)))
				   (else
				    (loop (+fx i 1) (cdr args)))))))
			((not (integer? (car val)))
			 (user-error "Parse error" "Illegal \"args-noescape\" pragma" prop))
			((eq? args-noescape #unspecified)
			 (set! args-noescape (list val)))
			(else
			 (set! args-noescape (cons val args-noescape))))))))
	  (else
	   (user-error "Parse error" "Illegal \"pragma\" form" prop '()))))
      (else
       (user-error "Parse error"
	  "Illegal \"pragma\" form"
	  (if (pair? prop) prop clause)
	  '()))))
	 
