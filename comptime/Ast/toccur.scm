;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/toccur.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  6 11:09:14 1995                          */
;*    Last change :  Fri Apr 21 18:48:38 2017 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Compute the occurrence number of each types of the AST.          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_type-occur
   (include "Ast/node.sch")
   (import  tools_error
	    tools_shape
	    ast_sexp
	    ast_env
	    ast_var
	    ast_node
	    type_type
	    type_typeof
	    ast_local)
   (import 
	    object_class
	    ast_dump)
   (export  (type-increment-global! ::global)))

;*---------------------------------------------------------------------*/
;*    type-increment-global! ...                                       */
;*---------------------------------------------------------------------*/
(define (type-increment-global! global)
   (if (sfun? (global-value global))
       (type-increment-sfun! global)
       (type-occurrence-increment! (global-type global))))

;*---------------------------------------------------------------------*/
;*    type-increment-sfun! ...                                         */
;*---------------------------------------------------------------------*/
(define (type-increment-sfun! var)
   (with-access::variable var (id)
      (type-occurrence-increment! (variable-type var))
      (for-each (lambda (a)
		   (cond
		      ((type? a)
		       (type-occurrence-increment! a))
		      ((local? a)
		       (type-occurrence-increment! (local-type a)))))
	 (sfun-args (variable-value var)))
      (let ((node (sfun-body (variable-value var))))
	 (when (node? node)
	    (occur-node! node)))))
   
;*---------------------------------------------------------------------*/
;*    occur-node-in! ...                                               */
;*---------------------------------------------------------------------*/
(define (occur-node-in! node global)
   (set! *global* global)
   (occur-node! node))
   
;*---------------------------------------------------------------------*/
;*    *global* ...                                                     */
;*---------------------------------------------------------------------*/
(define *global* #unspecified)

;*---------------------------------------------------------------------*/
;*    occur-node! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (occur-node! node::node)
   'done)

;*---------------------------------------------------------------------*/
;*    occur-node! ::var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::var)
   (let ((v (var-variable node)))
      (let ((value (variable-value v)))
	 (if (and (scnst? value) (node? (scnst-node value)))
	     (occur-node! (scnst-node value))))))

;*---------------------------------------------------------------------*/
;*    occur-node! ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::sequence)
   (occur-node*! (sequence-nodes node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::sync ...                                           */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::sync)
   (occur-node! (sync-mutex node))
   (occur-node! (sync-prelock node))
   (occur-node! (sync-body node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::app)
   (with-access::app node (fun args)
      (occur-node! fun)
      (occur-node*! args)
      (let* ((var (var-variable fun))
	     (val (variable-value var)))
      (when (cfun? val)
	 (type-occurrence-increment! (variable-type var))
	 (for-each type-occurrence-increment! (cfun-args-type val))))))

;*---------------------------------------------------------------------*/
;*    occur-node! ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::app-ly)
   (with-access::app-ly node (fun arg)
      (occur-node! fun)
      (occur-node! arg)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::funcall)
   (with-access::funcall node (fun args)
      (type-occurrence-increment! (get-type node #f))
      (occur-node! fun)
      (occur-node*! args)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::extern)
   (with-access::extern node (side-effect)
      (let ((nodes (extern-expr* node)))
	 (occur-node*! nodes))))

;*---------------------------------------------------------------------*/
;*    occur-node! ::getfield ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::getfield)
   (with-access::getfield node (type otype ftype)
      (type-occurrence-increment! type)
      (type-occurrence-increment! otype)
      (type-occurrence-increment! ftype)
      (call-next-method)))
			    
;*---------------------------------------------------------------------*/
;*    occur-node! ::setfield ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::setfield)
   (with-access::setfield node (type otype ftype)
      (type-occurrence-increment! type)
      (type-occurrence-increment! otype)
      (type-occurrence-increment! ftype)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::new ...                                            */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::new)
   (with-access::new node (type args-type)
      (type-occurrence-increment! type)
      (for-each type-occurrence-increment! args-type)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::cast)
   (with-access::cast node (type arg)
      (type-occurrence-increment! type)
      (occur-node! arg)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::cast-null ...                                      */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::cast-null)
   (with-access::cast-null node (type)
      (type-occurrence-increment! type)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::instanceof ...                                     */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::instanceof)
   (with-access::instanceof node (type class)
      (type-occurrence-increment! type)
      (type-occurrence-increment! class)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::vlength ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::vlength)
   (with-access::vlength node (type vtype)
      (type-occurrence-increment! type)
      (type-occurrence-increment! vtype)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::vref ...                                           */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::vref)
   (with-access::vref node (type ftype otype)
      (type-occurrence-increment! type)
      (type-occurrence-increment! ftype)
      (type-occurrence-increment! otype)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::vset! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::vset!)
   (with-access::vset! node (type ftype otype)
      (type-occurrence-increment! type)
      (type-occurrence-increment! ftype)
      (type-occurrence-increment! otype)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::valloc ...                                         */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::valloc)
   (with-access::valloc node (type ftype otype)
      (type-occurrence-increment! type)
      (type-occurrence-increment! ftype)
      (type-occurrence-increment! otype)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::setq)
   (with-access::setq node (var value)
      (occur-node! var)
      (occur-node! value)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::conditional)
   (with-access::conditional node (test true false)
      (occur-node! test)
      (occur-node! true)
      (occur-node! false)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::fail)
   (with-access::fail node (proc msg obj)
      (occur-node! proc)
      (occur-node! msg)
      (occur-node! obj)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::switch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::switch)
   (with-access::switch node (test clauses)
      (occur-node! test)
      (for-each (lambda (clause)
		   (occur-node! (cdr clause)))
		clauses)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each type-increment-sfun! locals)
      (occur-node! body)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (with-access::local (car binding) (type)
		      (type-occurrence-increment! type))
		   (occur-node! (cdr binding)))
		bindings)
      (occur-node! body)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::set-ex-it)
   (with-access::set-ex-it node (var)
      (occur-node! (set-ex-it-body node))))

;*---------------------------------------------------------------------*/
;*    occur-node! ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (occur-node! exit)
      (occur-node! value)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::make-box)
   (occur-node! (make-box-value node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::box-ref)
   (occur-node! (box-ref-var node)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::box-set!)
   (with-access::box-set! node (var value)
      (occur-node! var)
      (occur-node! value)))

;*---------------------------------------------------------------------*/
;*    occur-node! ::retblock ...                                       */
;*---------------------------------------------------------------------*/
(define-method (occur-node! node::retblock)
   (with-access::retblock node (body)
      (occur-node! body)))
		  
;*---------------------------------------------------------------------*/
;*    occur-node*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (occur-node*! node*)
   (for-each occur-node! node*))
