;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Sync/failsafe.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 18 08:49:33 2012                          */
;*    Last change :  Sun Nov 18 14:33:57 2012 (serrano)                */
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
      (let ((r
      (with-access::sync n (nodes)
	 (every failsafe? nodes))))
	 (tprint "node=" (shape n) " -> failsafe=" r)
	 r)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (failsafe? n::node)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe? ::atom ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::atom)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::var)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::kwote ...                                            */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::kwote)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::sequence)
   (with-access::sequence n (nodes)
      (every failsafe? nodes)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::sync ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::sync)
   (with-access::sync n (mutex nodes)
      (and (failsafe? mutex) (every failsafe? nodes))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::app)
   (with-access::app n (fun args)
      (let ((v (var-variable fun)))
	 (when (failsafe-fun? (variable-value v) v)
	    (every failsafe? args)))))

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::fun ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (failsafe-fun? fun::fun var::variable)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::sfun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe-fun? fun::sfun var::variable)
   (with-access::sfun fun (failsafe)
      (cond
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
	     (let ((fsafe (failsafe? (sfun-body fun))))
		(set! failsafe fsafe)
		fsafe))))))

;*---------------------------------------------------------------------*/
;*    failsafe-fun? ::cfun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe-fun? fun::cfun var::variable)
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
(define-method (failsafe? n::extern)
   (with-access::extern n (expr*)
      (every failsafe? expr*)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::pragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::pragma)
   #f)

;*---------------------------------------------------------------------*/
;*    failsafe? ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::cast)
   (with-access::cast n (arg)
      (failsafe? arg)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::setq)
   (with-access::setq n (value)
      (failsafe? value)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::conditional)
   (with-access::conditional n (test true false)
      (and (failsafe? test) (failsafe? true) (failsafe? false))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::select ...                                           */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::select)
   (with-access::select n (test clauses)
      (when (failsafe? test)
	 (every (lambda (c)
		   (failsafe? (cdr c)))
	    clauses))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::let-fun)
   (with-access::let-fun n (body locals)
      ;; don't traverse the local functions, they will be scanned on demand
      (failsafe? body)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::let-var)
   (with-access::let-var n (body bindings)
      (when (failsafe? body)
	 (every (lambda (b)
		   (failsafe? (cdr b)))
	    bindings))))

;*---------------------------------------------------------------------*/
;*    failsafe? ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::make-box)
   (with-access::make-box n (value)
      (failsafe? value)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::box-ref)
   #t)

;*---------------------------------------------------------------------*/
;*    failsafe? ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (failsafe? n::box-set!)
   (with-access::box-set! n (value)
      (failsafe? value)))


      
