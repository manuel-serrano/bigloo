;*=====================================================================*/
;*    serrano/prgm/project/bigloo2.3/comptime/Bdb/spread-obj.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 13 13:53:58 1995                          */
;*    Last change :  Thu Jul 13 11:16:40 2000 (serrano)                */
;*    Copyright   :  1992-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    When compiling for Bdb we have to turn all _ type into obj type. */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bdb_spread-obj
   (include "Engine/pass.sch"
	    "Ast/node.sch")
   (import  tools_shape
	    tools_error
	    tools_misc
	    type_env
	    type_cache
	    ast_sexp
	    engine_param)
   (export  (bdb-spread-obj! ast)))

;*---------------------------------------------------------------------*/
;*    bdb-spread-obj! ...                                              */
;*---------------------------------------------------------------------*/
(define (bdb-spread-obj! globals)
   (pass-prelude "Bdb (obj spreading)") 
   (for-each spread-obj-fun! globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    spread-obj-fun! ...                                              */
;*---------------------------------------------------------------------*/
(define (spread-obj-fun! var)
   (enter-function (variable-id var))
   (let* ((fun  (variable-value var))
	  (body (sfun-body fun))
	  (type (variable-type var))
	  (args (sfun-args fun)))
      (if (eq? type *_*)
	  (variable-type-set! var *obj*))
      (for-each (lambda (args)
		   (if (eq? (variable-type args) *_*)
		       (variable-type-set! args *obj*)))
		args)
      (if (>=fx *bdb-debug* 3)
	  (spread-obj-node! body))
      (leave-function)))
   
;*---------------------------------------------------------------------*/
;*    spread-obj-node! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (spread-obj-node! node::node)
   node)

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::sequence ...                                  */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::sequence)
   (spread-obj-node*! (sequence-nodes node)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::app ...                                       */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::app)
   (spread-obj-node*! (app-args node)))
 
;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::app-ly ...                                    */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::app-ly)
   (with-access::app-ly node (fun arg)
      (spread-obj-node! fun)
      (spread-obj-node! arg)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::funcall ...                                   */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::funcall)
   (with-access::funcall node (fun args)
      (spread-obj-node! fun)
      (spread-obj-node*! args)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::extern ...                                    */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::extern)
   (spread-obj-node*! (extern-expr* node)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::cast ...                                      */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::cast)
   (spread-obj-node! (cast-arg node)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::setq ...                                      */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::setq)
   (spread-obj-node! (setq-value node)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::conditional ...                               */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::conditional)
   (with-access::conditional node (test true false)
      (spread-obj-node! test)
      (spread-obj-node! true)
      (spread-obj-node! false)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::fail ...                                      */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::fail)
   (with-access::fail node (proc msg obj)
      (spread-obj-node! proc)
      (spread-obj-node! msg)
      (spread-obj-node! obj)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::select ...                                    */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::select)
   (with-access::select node (clauses test)
      (spread-obj-node! test)
      (for-each (lambda (clause)
		   (spread-obj-node! (cdr clause)))
		clauses)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::let-fun ...                                   */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each spread-obj-fun! locals)
      (spread-obj-node! body)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::let-var ...                                   */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((variable (car binding)))
		      (if (and (eq? (variable-type variable) *_*)
			       (local-user? variable))
			  (variable-type-set! variable *obj*)))
		   (spread-obj-node! (cdr binding)))
		bindings)
      (spread-obj-node! body)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::set-ex-it ...                                 */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::set-ex-it)
   (spread-obj-node! (set-ex-it-body node)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::jump-ex-it ...                                */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (spread-obj-node! exit) 
      (spread-obj-node! value))
      node)

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::make-box ...                                  */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::make-box)
   (spread-obj-node! (make-box-value node)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::box-ref ...                                   */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::box-ref)
   (spread-obj-node! (box-ref-var node)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node! ::box-set! ...                                  */
;*---------------------------------------------------------------------*/
(define-method (spread-obj-node! node::box-set!)
   (with-access::box-set! node (var value)
      (spread-obj-node! var)
      (spread-obj-node! value)))

;*---------------------------------------------------------------------*/
;*    spread-obj-node*! ...                                            */
;*---------------------------------------------------------------------*/
(define (spread-obj-node*! node*)
   (if (null? node*)
       'done
       (begin
	  (spread-obj-node! (car node*))
	  (spread-obj-node*! (cdr node*)))))
   
   


