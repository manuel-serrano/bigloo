;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Initflow/walk.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar 12 06:58:13 2011                          */
;*    Last change :  Sat Mar 12 09:38:38 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Compute the initialization property for global variables. The    */
;*    init property of a global can be one of:                         */
;*                                                                     */
;*      - #t (never used before initialized)                           */
;*      - #f (used before initialized)                                 */
;*      - #unspecified (unknown)                                       */
;*                                                                     */
;*    The result of the analysis is used by the CFA analysis when it   */
;*    affects types to global variables.                               */
;*                                                                     */
;*    To compute that property, this analysis walks over the source    */
;*    code, using a depth-first traversal of the AST, starting with    */
;*    the top-level pseudo function code.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module initflow_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    module_module
	    engine_param)
   (export  (initflow-walk! ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    initflow-walk! ...                                               */
;*---------------------------------------------------------------------*/
(define (initflow-walk! globals)
   (pass-prelude "Initflow")
   (let ((root (find-global/module 'toplevel-init *module*)))
      (initflow-fun root #t)
      (pass-postlude globals)))

;*---------------------------------------------------------------------*/
;*    initflow-fun ...                                                 */
;*---------------------------------------------------------------------*/
(define (initflow-fun var::variable e::bool)
   (let ((f (variable-value var)))
      (initflow-node (sfun-body f) e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::node ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (initflow-node node::node e::bool)
   '())

;*---------------------------------------------------------------------*/
;*    initflow-node ::var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::var e)
   (with-access::var node (variable)
      (when (global? variable)
	 (global-init-set! variable #f)
	 (if (sfun? (variable-value variable))
	     (initflow-node var e)
	     '()))))

;*---------------------------------------------------------------------*/
;*    initflow-node* ...                                               */
;*---------------------------------------------------------------------*/
(define (initflow-node* nodes e)
   (let loop ((nodes nodes)
	      (is '()))
      (if (null? nodes)
	  (if e
	      (begin
		 (for-each (lambda (g)
			      (when (global-init g)
				 (global-init-set! g #t)))
			   is)
		 '())
	      (filter global-init is))
	  (let ((i (initflow-node (car nodes) #f)))
	     (loop (cdr nodes) (append i is))))))

;*---------------------------------------------------------------------*/
;*    initflow-node ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::sequence e)
   (with-access::sequence node (nodes)
      (let loop ((nodes nodes)
		 (is '()))
	 (if (null? nodes)
	     is
	     (let ((i (initflow-node (car nodes) e)))
		(loop (cdr nodes) (append i is)))))))

;*---------------------------------------------------------------------*/
;*    initflow-node ::app ...                                          */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::app e)
   (with-access::app node (fun args)
      (let ((is (initflow-node* args e)))
	 (if (and (var? fun) (sfun? (variable-value (var-variable fun))))
	     (append (initflow-fun (var-variable fun) e) is)
	     is))))

;*---------------------------------------------------------------------*/
;*    initflow-node ::app-ly ...                                       */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::app-ly e)
   (with-access::app-ly node (fun arg)
      (initflow-node* (list fun arg) e)))
      
;*---------------------------------------------------------------------*/
;*    initflow-node ::funcall ...                                      */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::funcall e)
   (with-access::funcall node (fun args)
      (initflow-node* (cons fun args) e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::extern ...                                       */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::extern e)
   (with-access::extern node (expr*)
      (initflow-node* expr* e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::setq ...                                         */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::setq e)
   (with-access::setq node (var value)
      (let ((is (initflow-node value e))
	    (v (var-variable var)))
	 (if (and (global? v) (eq? (global-init v) #unspecified))
	     (if e
		 (begin
		    (global-init-set! v #t)
		    '())
		 (cons var is))
	     is))))

;*---------------------------------------------------------------------*/
;*    lub ...                                                          */
;*---------------------------------------------------------------------*/
(define (lub is e)
   (if (null? is)
       '()
       (let ((lub '()))
	  (for-each (lambda (g)
		       (when (and (eq? (global-init g) #unspecified)
				  (every? (lambda (i) (memq g i)) (cdr is)))
			  (if e
			      (global-init-set! g #t)
			      (set! lub (cons g lub)))))
		    (car is))
	  lub)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::conditional ...                                  */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::conditional e)
   (with-access::conditional node (test true false)
      (let ((i0 (initflow-node test e)))
	 ;; compute the analysis for the two branches
	 (let ((i1 (initflow-node true #f))
	       (i2 (initflow-node false #f)))
	    ;; the lub
	    (append (lub (list i1 i2) e) i0)))))

;*---------------------------------------------------------------------*/
;*    initflow-node ::fail ...                                         */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::fail e)
   (with-access::fail node (proc msg obj)
      (initflow-node* (list proc msg obj) e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::select ...                                       */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::select e)
   (with-access::select node (test clauses)
      (let ((i0 (initflow-node test e)))
	 (let ((is (map (lambda (c) (initflow-node (cdr c) #f)) clauses)))
	    ;; the lub
	    (append (lub is e) i0)))))

;*---------------------------------------------------------------------*/
;*    initflow-node ::make-box ...                                     */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::make-box e)
   (with-access::make-box node (value)
      (initflow-node value e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::box-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::box-ref e)
   (with-access::box-ref node (var)
      (initflow-node var e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::box-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::box-set! e)
   (with-access::box-set! node (var value)
      (initflow-node value e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::let-fun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::let-fun e)
   (with-access::let-fun node (body)
      (initflow-node body e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::let-var ...                                      */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::let-var e)
   (with-access::let-var node (bindings body)
      (let* ((is (map (lambda (b) (initflow-node (cdr b) e)) bindings))
	     (lub (lub is e)))
	 (append lub (initflow-node body e)))))

;*---------------------------------------------------------------------*/
;*    initflow-node ::set-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::set-ex-it e)
   (with-access::set-ex-it node (body)
      (initflow-node body e)))

;*---------------------------------------------------------------------*/
;*    initflow-node ::jump-ex-it ...                                   */
;*---------------------------------------------------------------------*/
(define-method (initflow-node node::jump-ex-it e)
   (with-access::jump-ex-it node (exit value)
      (initflow-node* (list exit value) e)))
      
