;*=====================================================================*/
;*    serrano/bigloo/5.0a/comptime/Ast/venv.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 25 11:32:49 1994                          */
;*    Last change :  Thu Jan 29 17:55:59 2026 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The global environment manipulation                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_env
   (import  tools_shape
	    engine_param
	    tools_error
	    type_type
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_hrtype
	    ast_dump
	    read_jvm
	    module_module
	    tools_location
	    backend_backend
	    inline_inline)
   (export  (initialize-genv!)
	    (set-genv! ::obj)
	    (add-genv! ::obj)
	    (get-genv) 
	    (find-global ::obj ::symbol . <symbol>)
	    (find-global/module ::obj ::symbol ::symbol)
	    (get-global/module ::obj ::symbol ::symbol)
	    (add-global! ::obj ::global ::symbol)
	    (bind-global!::global ::obj ::symbol ::obj ::symbol ::value ::symbol ::obj)
	    (unbind-global! ::obj ::symbol ::symbol)
	    (for-each-global! ::obj ::procedure)
	    (global-bucket-position::long ::obj ::symbol ::symbol)
	    (global-bucket-length::long ::obj ::symbol ::symbol)
	    (restore-global! new ::obj)
	    (additional-heap-restore-globals! ::obj)
	    (already-restored? fun)))

;*---------------------------------------------------------------------*/
;*    *Genv* ...                                                       */
;*    -------------------------------------------------------------    */
;*    The Global environment (for global variable definitions).        */
;*---------------------------------------------------------------------*/
(define *Genv* 'the-global-environment)

;*---------------------------------------------------------------------*/
;*    set-genv! ...                                                    */
;*---------------------------------------------------------------------*/
(define (set-genv! Genv)
   (set! *Genv* Genv))
		 
;*---------------------------------------------------------------------*/
;*    add-genv! ...                                                    */
;*    -------------------------------------------------------------    */
;*    When adding a new environment we have to mark that all global    */
;*    bindings are library ones.                                       */
;*---------------------------------------------------------------------*/
(define (add-genv! Genv)
   (hashtable-for-each
    Genv
    (lambda (k bucket)
       (for-each (lambda (new)
		    (delay-restore-global! new)
		    (let* ((module (global-module new))
			   (id     (global-id new))
			   (bucket (hashtable-get *Genv* id)))
		       (cond
			  ((not (pair? bucket))
			   (hashtable-put! *Genv* id (list id new)))
			  ((or (eq? module *module*)
			       (not (eq? *module*
					 (global-module (cadr bucket)))))
			   ;; we add the new global in first position if:
			   ;;   - we are binding a variable of the current
			   ;;     module
			   ;;   - the first global already bound is not owned
			   ;;     by the current module
			   (let ((new-bucket (cons new (cdr bucket))))
			      (set-cdr! bucket new-bucket)))
			  (else
			   (set-cdr! (cdr bucket) (cons new (cddr bucket)))))))
		 (cdr bucket))))
   (set! *restored* '()))

;*---------------------------------------------------------------------*/
;*    *delayed-restored-global* ...                                    */
;*---------------------------------------------------------------------*/
(define *delayed-restored-global* '())

;*---------------------------------------------------------------------*/
;*    delay-restore-global! ...                                        */
;*---------------------------------------------------------------------*/
(define (delay-restore-global! g)
   (set! *delayed-restored-global* (cons g *delayed-restored-global*)))

;*---------------------------------------------------------------------*/
;*    additional-heap-restore-globals! ...                             */
;*---------------------------------------------------------------------*/
(define (additional-heap-restore-globals! env)
   (for-each (lambda (g) (restore-global! g env)) *delayed-restored-global*)
   #t)

;*---------------------------------------------------------------------*/
;*    restore-global! ...                                              */
;*---------------------------------------------------------------------*/
(define (restore-global! new env)
   ;; we mark that the current global has been restored
   (mark-restored! new)
   (let* ((id (global-id new))
	  (type (global-type new))
	  (value (global-value new))
	  (typeid (type-id type)))
      ;; we restore type result
      (global-type-set! new (find-type typeid))
      ;; the parameters type
      (restore-value-types! value id env)
      ;; we restore the jvm qualified type name
      (when (and (backend-qualified-types (the-backend))
		 (not (eq? (global-module new) 'foreign)))
	 (add-qualified-type! (global-module new)
	    (global-qualified-type-name new)
	    (shape new)))))

;*---------------------------------------------------------------------*/
;*    *restored* ...                                                   */
;*---------------------------------------------------------------------*/
(define *restored* '())

;*---------------------------------------------------------------------*/
;*    mark-restored! ...                                               */
;*---------------------------------------------------------------------*/
(define (mark-restored! fun)
   (set! *restored* (cons fun *restored*)))

;*---------------------------------------------------------------------*/
;*    already-restored? ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is used only once:                                 */
;*      @ref hrtype.scm:already-restored@                              */
;*---------------------------------------------------------------------*/
(define (already-restored? fun)
   (memq fun *restored*))

;*---------------------------------------------------------------------*/
;*    restore-value-types! ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (restore-value-types! value::value id env)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    restore-value-types! ::fun ...                                   */
;*---------------------------------------------------------------------*/
(define-method (restore-value-types! value::fun id env)
   (with-access::fun value (predicate-of)
      (when (type? predicate-of)
         (set! predicate-of (find-type (type-id predicate-of))))))

;*---------------------------------------------------------------------*/
;*    restore-value-types! ::sfun ...                                  */
;*---------------------------------------------------------------------*/
(define-method (restore-value-types! value::sfun id env)
   (call-next-method)
   (with-access::sfun value (args)
      (let loop ((args args))
	 (cond
	    ((pair? args)
	     (let ((arg (car args)))
		(cond
		   ((type? arg)
		    (set-car! args (find-type (type-id arg))))
		   ((local? arg)
		    (let ((new-type (find-type (type-id (local-type arg)))))
		       (local-type-set! arg new-type)))
		   (else
		    (error "restore-value-types(sfun)"
			   "Illegal argument"
			   (shape arg))))
		(loop (cdr args))))
	    ((null? args)
	     (let ((body (sfun-body value)))
		;; we still have to restore the body types
		(if (node? body)
		    (let ((tres (node-type body)))
		       (hrtype-node! body env)
		       (when (type? tres)
			  (node-type-set! body (find-type (type-id tres))))))))
	    (else
	     (error "restore-value-types"
		    "Illegal non pair argument"
		    (shape args)))))))

;*---------------------------------------------------------------------*/
;*    restore-value-types! ::isfun ...                                 */
;*---------------------------------------------------------------------*/
(define-method (restore-value-types! value::isfun id env)
   (call-next-method)
   (hrtype-node! (isfun-original-body value) env))

;*---------------------------------------------------------------------*/
;*    restore-value-types! ::cfun ...                                  */
;*---------------------------------------------------------------------*/
(define-method (restore-value-types! value::cfun id env)
   (call-next-method)
   (with-access::cfun value (args-type)
      (let loop ((args args-type))
	 (if (pair? args)
	     (begin
		(if (eq? (type-id (car args)) '_)
		    (begin
		       (user-warning "head-restore"
			  "Illegal restored type for foreign function"
			  id)
		       (set-car! args *obj*))
		    (set-car! args (find-type (type-id (car args)))))
		(loop (cdr args)))))))
   
;*---------------------------------------------------------------------*/
;*    get-genv ...                                                     */
;*---------------------------------------------------------------------*/
(define (get-genv)
   *Genv*)

;*---------------------------------------------------------------------*/
;*    initialize-genv! ...                                             */
;*---------------------------------------------------------------------*/
(define (initialize-genv!)
   (set! *Genv* (make-hashtable)))

;*---------------------------------------------------------------------*/
;*    find-global ...                                                  */
;*---------------------------------------------------------------------*/
(define (find-global env id::symbol . module)
   (assert (module) (or (null? module) (symbol? (car module))))
   (let ((bucket (hashtable-get env id))
	 (module (if (null? module) '() (car module))))
      (cond
	 ((not (pair? bucket))
	  #f)
	 ((null? (cdr bucket))
	  #f)
	 ((null? module)
	  (cadr bucket))
	 (else
	  (let loop ((globals (cdr bucket)))
	     (cond
		((null? globals)
		 #f)
		((eq? (global-module (car globals)) module)
		 (car globals))
		(else
		 (loop (cdr globals)))))))))

;*---------------------------------------------------------------------*/
;*    find-global/module ...                                           */
;*---------------------------------------------------------------------*/
(define (find-global/module env id::symbol module)
   (let ((bucket (hashtable-get env id)))
      (cond
	 ((not (pair? bucket))
	  #f)
	 ((null? (cdr bucket))
	  #f)
	 ((null? module)
	  (cadr bucket))
	 (else
	  (let loop ((globals (cdr bucket)))
	     (cond
		((null? globals)
		 #f)
		((eq? (global-module (car globals)) module)
		 (car globals))
		(else
		 (loop (cdr globals)))))))))

;*---------------------------------------------------------------------*/
;*    get-global/module ...                                            */
;*---------------------------------------------------------------------*/
(define (get-global/module env id::symbol module)
   (let ((global (find-global/module env id module)))
      (when (and (not (global? global)) (not *lib-mode*))
	 (internal-error 'get-global/module
	    "Cannot find global variable"
	    (format "~a::~a" id module)))
      global))

;*---------------------------------------------------------------------*/
;*    warning-override-global! ...                                     */
;*---------------------------------------------------------------------*/
(define (warning-override-global! n o srce)
   (when (and (>fx (bigloo-warning) 0) *warning-overriden-variables*)
      (with-access::global o (id module src)
	 (let ((loc (or (find-location src) (find-location srce)))
	       (old `(@ ,id ,module))
	       (new `(@ ,(global-id n) ,(global-module n)))
	       (msg "Variable overridden by"))
	    (if loc
		(user-warning/location loc old msg new)
		(user-warning old msg new))))))

;*---------------------------------------------------------------------*/
;*    error-rebind-global! ...                                         */
;*---------------------------------------------------------------------*/
(define (error-rebind-global! o src)
   (with-access::global o (id module)
      (let ((loc (find-location src))
	    (msg "Duplicate definition"))
	 (if loc
	     (user-error/location loc module msg (shape o))
	     (user-error module msg (shape o))))))

;*---------------------------------------------------------------------*/
;*    warning-rebind-global! ...                                       */
;*---------------------------------------------------------------------*/
(define (warning-rebind-global! o src)
   (with-access::global o (id module)
      (let ((loc (find-location src))
	    (msg "Duplicate definition"))
	 (if loc
	     (user-warning/location loc module msg (shape o))
	     (user-warning module msg (shape o))))))

;*---------------------------------------------------------------------*/
;*    bind-global! ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function binds a new global variable in the global table.   */
;*    When several variables share the identifier the following rules  */
;*    are observed:                                                    */
;*       1- if the two declarations concerns the same module, an error */
;*          is notified.                                               */
;*       2- the variable defined in the current module has the highest */
;*          priority (it overrides the others definitions).            */
;*       3- imported variables have a higher priority than library     */
;*          variables.                                                 */
;*       4- user library variables have a higher priority than system  */
;*          library variables.                                         */
;*---------------------------------------------------------------------*/
(define (bind-global!::global env
	   id::symbol alias::obj module::symbol
	   value::value import::symbol
	   src::obj)
   (let* ((ident (or alias id))
	  (old (find-global env ident module)))
      (if (global? old)
	  (cond
	     (*lib-mode*
	      old)
	     ((eq? module (global-module old))
	      (warning-rebind-global! old src)
	      old)
	     (else
	      (error-rebind-global! old src)))
	  (let* ((qtn (cond
			 ((not (backend-qualified-types (the-backend))) "")
			 ((eq? import 'eval) "eval")
			 (else (module->qualified-type module))))
		 (new (instantiate::global
			 (type *_*)
			 (module module)
			 (qualified-type-name qtn)
			 (id ident)
			 (alias (when alias id))
			 (value value)
			 (src src)
			 (user? #t)
			 (import import))))
	     (add-global! env new ident)
	     new))))

;*---------------------------------------------------------------------*/
;*    add-global! ...                                                  */
;*---------------------------------------------------------------------*/
(define (add-global! env g::global ident::symbol)
   (let ((bucket (hashtable-get env ident)))
      (cond
	 ((or (not (pair? bucket)) (null? (cdr bucket)))
	  ;; this is the first time we see this identifier
	  (hashtable-put! env ident (list ident g)))
	 (else
	  (let* ((old* (cdr bucket))
		 (mid (module-initialization-id
			 (global-module (car old*)))))
	     (cond
		((eq? (global-module (car old*)) *module*)
		 (set-cdr! (cdr bucket) (cons g (cddr bucket))))
		(else
		 (let ((new* (cons g old*)))
		    (set-cdr! bucket new*)))))))))
   
;*---------------------------------------------------------------------*/
;*    unbind-global! ...                                               */
;*---------------------------------------------------------------------*/
(define (unbind-global! env id::symbol module::symbol)
   (let ((global (find-global env id module)))
      (if (not (global? global))
	  (user-error "unbind-global!" "Can't find global" `(@ ,id ,module))
	  (let ((bucket (hashtable-get env id)))
	     (let loop ((cur  (cdr bucket))
			(prev bucket))
		(if (eq? (car cur) global)
		    (set-cdr! prev (cdr cur))
		    (loop (cdr cur) (cdr prev))))))))
   
;*---------------------------------------------------------------------*/
;*    for-each-global! ...                                             */
;*---------------------------------------------------------------------*/
(define (for-each-global! env proc::procedure)
   (hashtable-for-each env
      (lambda (k bucket) (for-each proc (cdr bucket)))))
   
;*---------------------------------------------------------------------*/
;*    global-bucket-position                                           */
;*---------------------------------------------------------------------*/
(define (global-bucket-position env id module)
   (let ((bucket (hashtable-get env id)))
      (if (not (pair? bucket))
	  -1
	  (let loop ((globals (cdr bucket))
		     (pos 0))
	     (cond
		((null? globals) -1)
		((eq? (global-module (car globals)) module) pos)
		(else (loop (cdr globals) (+fx pos 1))))))))

;*---------------------------------------------------------------------*/
;*    global-bucket-length                                             */
;*---------------------------------------------------------------------*/
(define (global-bucket-length env id module)
   (let ((bucket (hashtable-get env id)))
      (if (not (pair? bucket))
	  0
	  (length (cdr bucket)))))
   
