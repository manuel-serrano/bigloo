;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/lvtype.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  3 11:58:06 1996                          */
;*    Last change :  Thu Dec  5 09:13:49 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We type an node (straightforward typing used by Coerce and Cnst, */
;*    i.e. passes which occur after the Cfa). This pass only types     */
;*    local variables introduced in let-var.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_lvtype
   (import  type_type
	    type_cache
	    type_typeof
	    tools_shape
	    tools_error
	    ast_var
	    ast_node)
   (export  (generic lvtype-node! ::node)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (lvtype-node! node::node))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::atom ...                                          */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::atom)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::kwote ...                                         */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::kwote)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::var)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::closure ...                                       */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::closure)
   (internal-error "lvtype-node!" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::sequence ...                                      */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::sequence)
   (with-access::sequence node (nodes)
      (lvtype-node*! nodes)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::app ...                                           */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::app)
   (with-access::app node (args)
      (lvtype-node*! args)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::app-ly ...                                        */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::app-ly)
   (with-access::app-ly node (fun arg)
      (lvtype-node! fun)
      (lvtype-node! arg)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::funcall ...                                       */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::funcall)
   (with-access::funcall node (fun args)
      (lvtype-node! fun)
      (lvtype-node*! args)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::extern ...                                        */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::extern)
   (with-access::extern node (expr* type)
      (lvtype-node*! expr*)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::cast)
   (with-access::cast node (arg type)
      (lvtype-node! arg)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::setq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::setq)
   (with-access::setq node (var value)
      (lvtype-node! value)
      (lvtype-node! var)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::conditional ...                                   */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::conditional)
   (with-access::conditional node (test true false)
       (lvtype-node! test)
       (lvtype-node! true)
       (lvtype-node! false)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::fail ...                                          */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::fail)
   (with-access::fail node (type proc msg obj)
      (lvtype-node! proc)
      (lvtype-node! msg)
      (lvtype-node! obj)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::select ...                                        */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::select)
   (with-access::select node (clauses test)
      (lvtype-node! test)
      (for-each (lambda (clause)
		   (lvtype-node! (cdr clause)))
		clauses)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::let-fun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (lvtype-node! (sfun-body (local-value local))))
		locals)
      (lvtype-node! body)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::let-var ...                                       */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (lvtype-node! val)
		      (set-variable-type! var (typeof val))))
		bindings)
      (lvtype-node! body)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::set-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (lvtype-node! body)
      (lvtype-node! var)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::jump-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (lvtype-node! exit) 
      (lvtype-node! value)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::make-box ...                                      */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::make-box)
   (with-access::make-box node (value)
      (lvtype-node! value)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::box-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::box-set!)
   (with-access::box-set! node (var value)
      (lvtype-node! var)
      (lvtype-node! value)))

;*---------------------------------------------------------------------*/
;*    lvtype-node! ::box-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (lvtype-node! node::box-ref)
   (with-access::box-ref node (var)
      (lvtype-node! var)))

;*---------------------------------------------------------------------*/
;*    lvtype-node*! ...                                                */
;*---------------------------------------------------------------------*/
(define (lvtype-node*! node*)
   (for-each lvtype-node! node*))
   
;*---------------------------------------------------------------------*/
;*    set-variable-type! ...                                           */
;*---------------------------------------------------------------------*/
(define (set-variable-type! variable::variable type::type)
   (let ((ntype (if (eq? type *_*) *obj* type))
	 (otype (variable-type variable))) 
      (if (eq? otype *_*)
	  (variable-type-set! variable ntype))))

