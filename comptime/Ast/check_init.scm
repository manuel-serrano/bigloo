;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/check_init.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 24 11:02:16 1999                          */
;*    Last change :  Mon Nov 11 09:41:31 2013 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module checks that global variables are not used before     */
;*    being initialized. This function implements a walk thru          */
;*    the ast but it does not go on the function body. It just checks  */
;*    top-level expressions. Actually it checks the body of the        */
;*    function implementing the top-level forms.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module ast_check-global-init
   (include "Ast/unit.sch")
   (import  ast_env
	    ast_var
	    ast_node
	    tools_error
	    type_type
	    type_cache
	    object_class
	    module_module
	    module_include
	    module_class))

;*---------------------------------------------------------------------*/
;*    check-global-initialization ...                                  */
;*---------------------------------------------------------------------*/
(define (check-global-initialization-TOBEREMOVE-13mar2011)
   ;; this function (and its whole module) is to be removed, it has been
   ;; subsumed by the init flow analysis (see initflow_walk module). 
   (let* ((init-name (symbol-append (unit-id (get-toplevel-unit)) '-init))
 	  (global    (find-global init-name)))
      (if (and (global? global) (sfun? (global-value global)))
	  (begin
	     (init-defined-globals!)
	     ;; the object form (for class definitions)
	     (let ((unit (get-object-unit)))
		(if (unit? unit)
		    (let* ((obj-name (symbol-append (unit-id unit) '-init))
			   (global (find-global obj-name)))
		       (if (and (global? global) (sfun? (global-value global)))
			   ;; when processing the object-init section, we
			   ;; have to disconnect warning because we know
			   ;; the initialization order is uncorrect but we
			   ;; want to hide this.
			   (no-warning
			    (lambda ()
			       (check-init
				(sfun-body (global-value global)))))))))
	     ;; the init form (for toplevel definitions)
	     (check-init (sfun-body (global-value global)))
	     ;; we reset the global list of defined variable because
	     ;; it won't be used anymore.
	     (init-defined-globals!)))))

;*---------------------------------------------------------------------*/
;*    *globals* ...                                                    */
;*---------------------------------------------------------------------*/
(define *globals* '())

;*---------------------------------------------------------------------*/
;*    init-defined-globals! ...                                        */
;*---------------------------------------------------------------------*/
(define (init-defined-globals!)
   (set! *globals* '()))

;*---------------------------------------------------------------------*/
;*    define-global! ...                                               */
;*---------------------------------------------------------------------*/
(define (define-global! global)
   (set! *globals* (cons global *globals*)))

;*---------------------------------------------------------------------*/
;*    global-defined? ...                                              */
;*---------------------------------------------------------------------*/
(define (global-defined? global)
   (memq global *globals*))

;*---------------------------------------------------------------------*/
;*    check-init :: ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (check-init node::node)
   'done)

;*---------------------------------------------------------------------*/
;*    check-init ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (check-init node::var)
   (let ((var (var-variable node)))
      (if (and (global? var)
	       (not (fun? (global-value var)))
	       (not (global-defined? var))
	       (eq? (global-module var) *module*))
	  (begin
	     (user-warning/location (node-loc node)
				    "check-init"
				    "Variable used before initialized"
				    (variable-id var))
	     (if (eq? (global-type var) *_*)
		 (global-type-set! var *obj*)
		 (error (type-id (variable-type var))
			"Illegal type for prematurely used variable"
			(variable-id var)))
	     (define-global! var)))))

;*---------------------------------------------------------------------*/
;*    check-init ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (check-init node::sequence)
   (check-init* (sequence-nodes node)))

;*---------------------------------------------------------------------*/
;*    check-init ::sync ...                                            */
;*---------------------------------------------------------------------*/
(define-method (check-init node::sync)
   (check-init (sync-mutex node))
   (check-init (sync-prelock node))
   (check-init (sync-body node)))

;*---------------------------------------------------------------------*/
;*    check-init ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (check-init node::app)
   (with-access::app node (fun args)
      (check-init* args)))
 
;*---------------------------------------------------------------------*/
;*    check-init ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (check-init node::app-ly)
   (with-access::app-ly node (fun arg)
      (check-init fun)
      (check-init arg)))

;*---------------------------------------------------------------------*/
;*    check-init ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (check-init node::funcall)
   (with-access::funcall node (fun args)
      (check-init fun)
      (check-init* args)))

;*---------------------------------------------------------------------*/
;*    check-init ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (check-init node::extern)
   (check-init* (extern-expr* node)))

;*---------------------------------------------------------------------*/
;*    check-init ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (check-init node::cast)
   (check-init (cast-arg node)))

;*---------------------------------------------------------------------*/
;*    check-init ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (check-init node::setq)
   ;; we define the variable before checking the value because
   ;; very frequently we use expression like `(define var var)'.
   ;; we don't want these expressions to produce a warning.
   (let ((var (var-variable (setq-var node))))
      (if (global? var)
	  (define-global! var)))
   (check-init (setq-value node)))

;*---------------------------------------------------------------------*/
;*    check-init ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (check-init node::conditional)
   (with-access::conditional node (test true false)
       (check-init test)
       (check-init true)
       (check-init false)))

;*---------------------------------------------------------------------*/
;*    check-init ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (check-init node::fail)
   (with-access::fail node (proc msg obj)
      (check-init proc)
      (check-init msg)
      (check-init obj)))

;*---------------------------------------------------------------------*/
;*    check-init ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (check-init node::select)
   (with-access::select node (clauses test)
      (check-init test)
      (for-each (lambda (clause)
		   (check-init (cdr clause)))
		clauses)))

;*---------------------------------------------------------------------*/
;*    check-init ::let-fun ...                                         */
;*    -------------------------------------------------------------    */
;*    We do not go inside local function. Thus, we simply check for    */
;*    the body of the let-fun.                                         */
;*---------------------------------------------------------------------*/
(define-method (check-init node::let-fun)
   (with-access::let-fun node (body locals)
      (check-init body)))

;*---------------------------------------------------------------------*/
;*    check-init ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (check-init node::let-var)
   (with-access::let-var node (body bindings)
      (check-init body)
      (for-each (lambda (binding)
		   (check-init (cdr binding)))
		bindings)))

;*---------------------------------------------------------------------*/
;*    check-init ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (check-init node::set-ex-it)
   (check-init (set-ex-it-body node)))

;*---------------------------------------------------------------------*/
;*    check-init ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (check-init node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (check-init exit)
      (check-init value)))

;*---------------------------------------------------------------------*/
;*    check-init ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (check-init node::make-box)
   (check-init (make-box-value node)))

;*---------------------------------------------------------------------*/
;*    check-init ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (check-init node::box-set!)
   (with-access::box-set! node (value)
      (check-init value)))

;*---------------------------------------------------------------------*/
;*    check-init ...                                                   */
;*---------------------------------------------------------------------*/
(define (check-init* node*)
   (for-each check-init node*))
