;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Sync/failsafe.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 18 08:49:33 2012                          */
;*    Last change :  Sun Nov 18 19:12:58 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
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
      (with-access::sync n (nodes)
	 (every (lambda (n) (failsafe? n #f)) nodes))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (failsafe? n::node ctx)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe? ::atom ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::atom ctx)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::var ctx)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::kwote ...                                            */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::kwote ctx)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::sequence ctx)
   (with-access::sequence n (nodes)
      (every (lambda (n) (failsafe? n ctx)) nodes)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::sync ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::sync ctx)
   (with-access::sync n (mutex nodes)
      (and (failsafe? mutex ctx) (every (lambda (n) (failsafe? n ctx)) nodes))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::app ctx)
   (with-access::app n (fun args)
      (let ((v (var-variable fun)))
	 (when (failsafe-fun? (variable-value v) v ctx)
	    (every (lambda (n) (failsafe? n #f)) args)))))

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::fun ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (failsafe-fun? fun::fun var::variable ctx)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::sfun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe-fun? fun::sfun var::variable ctx)
   (with-access::sfun fun (failsafe)
      (cond
	 ((eq? var ctx)
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
	     (set! failsafe #f)
	     (let ((fsafe (failsafe? (sfun-body fun) var)))
		(set! failsafe fsafe)
		fsafe))))))

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::cfun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe-fun? fun::cfun var::variable ctx)
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
(define-method (failsafe? n::extern ctx)
   (with-access::extern n (expr*)
      (every (lambda (n) (failsafe? n ctx)) expr*)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::pragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::pragma ctx)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe? ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::cast ctx)
   (with-access::cast n (arg)
      (failsafe? arg ctx)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::setq ctx)
   (with-access::setq n (value)
      (failsafe? value ctx)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::conditional ctx)
   (with-access::conditional n (test true false)
      (and (failsafe? test ctx) (failsafe? true ctx) (failsafe? false ctx))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::select ...                                           */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::select ctx)
   (with-access::select n (test clauses)
      (when (failsafe? test ctx)
	 (every (lambda (c) (failsafe? (cdr c) ctx)) clauses))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::let-fun ctx)
   (with-access::let-fun n (body locals)
      ;; don't traverse the local functions, they will be scanned on demand
      (failsafe? body ctx)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::let-var ctx)
   (with-access::let-var n (body bindings)
      (when (failsafe? body ctx)
	 (every (lambda (b) (failsafe? (cdr b) ctx)) bindings))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::make-box ctx)
   (with-access::make-box n (value)
      (failsafe? value ctx)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::box-ref ctx)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::box-set! ctx)
   (with-access::box-set! n (value)
      (failsafe? value ctx)))


      
