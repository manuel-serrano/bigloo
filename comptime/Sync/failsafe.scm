;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Sync/failsafe.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 18 08:49:33 2012                          */
;*    Last change :  Fri Apr 21 18:40:29 2017 (serrano)                */
;*    Copyright   :  2012-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The property FAILSAFE for a node is true, IFF that node cannot   */
;*    raise an exception or invoke an exit.                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sync_failsafe

   (include "Tools/trace.sch"
	    "Tools/location.sch")
   
   (import  tools_error
	    tools_shape
	    engine_param
	    type_type
	    type_tools
	    type_cache
	    type_typeof
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_local
	    ast_sexp
	    ast_app
	    ast_dump
	    module_module
	    effect_effect
	    backend_cplib)

   (export (failsafe-sync? ::sync)))

;*---------------------------------------------------------------------*/
;*    failsafe-sync? ...                                               */
;*---------------------------------------------------------------------*/
(define (failsafe-sync? n::sync)
   (when *optim-sync-failsafe?*
      (with-access::sync n (body)
	 (failsafe? body '()))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (failsafe? n::node stk::pair-nil)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe? ::atom ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::atom stk)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::var stk)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::kwote ...                                            */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::kwote stk)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::sequence stk)
   (with-access::sequence n (nodes)
      (every (lambda (n) (failsafe? n stk)) nodes)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::sync ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::sync stk)
   (with-access::sync n (mutex prelock body)
      (and (failsafe? mutex stk)
	   (failsafe? prelock stk)
	   (failsafe? body stk))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::app stk)
   (with-access::app n (fun args)
      (let ((v (var-variable fun)))
	 (when (failsafe-fun? (variable-value v) v stk)
	    (every (lambda (n) (failsafe? n stk)) args)))))

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::fun ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (failsafe-fun? fun::fun var::variable stk)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::sfun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe-fun? fun::sfun var::variable stk)
   (with-access::sfun fun (failsafe)
      (cond
	 ((memq var stk)
	  #t)
	 ((boolean? failsafe)
	  failsafe)
	 ((and (global? var) (not (eq? (global-module var) *module*)))
	  ;; an imported global, check the pragma annotation
	  (with-access::global var (pragma)
	     (set! failsafe (pair? (memq 'fail-safe pragma)))
	     failsafe))
	 (else
	  (with-access::sfun fun (failsafe)
	     (let ((fsafe (failsafe? (sfun-body fun) (cons var stk))))
		;; mark the function only when the stack is empty
		(when (null? stk) (set! failsafe fsafe))
		fsafe))))))

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::cfun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe-fun? fun::cfun var::variable stk)
   (with-access::cfun fun (failsafe)
      (cond
	 ((boolean? failsafe)
	  failsafe)
	 ((global? var)
	  (with-access::global var (pragma)
	     (set! failsafe (pair? (memq 'fail-safe pragma)))
	     failsafe))
	 (else
	  #f))))
   
;*---------------------------------------------------------------------*/
;*    failsafe? ::extern ...                                           */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::extern stk)
   (with-access::extern n (expr*)
      (every (lambda (n) (failsafe? n stk)) expr*)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::pragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::pragma stk)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe? ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::cast stk)
   (with-access::cast n (arg)
      (failsafe? arg stk)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::setq stk)
   (with-access::setq n (value)
      (failsafe? value stk)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::conditional stk)
   (with-access::conditional n (test true false)
      (and (failsafe? test stk) (failsafe? true stk) (failsafe? false stk))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::switch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::switch stk)
   (with-access::switch n (test clauses)
      (when (failsafe? test stk)
	 (every (lambda (c) (failsafe? (cdr c) stk)) clauses))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::let-fun stk)
   (with-access::let-fun n (body locals)
      ;; don't traverse the local functions, they will be scanned on demand
      (failsafe? body stk)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::let-var stk)
   (with-access::let-var n (body bindings)
      (when (failsafe? body stk)
	 (every (lambda (b) (failsafe? (cdr b) stk)) bindings))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::make-box stk)
   (with-access::make-box n (value)
      (failsafe? value stk)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::box-ref stk)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::box-set! stk)
   (with-access::box-set! n (value)
      (failsafe? value stk)))


      
