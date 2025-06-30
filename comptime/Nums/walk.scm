;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Nums/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Mon Jun 30 08:01:15 2025 (serrano)                */
;*    Copyright   :  2010-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Remove uless cell that has been introduced for bind-exit forms   */
;*    that have finally been transformed into returns and gotos.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module nums_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_location
	    type_cache
	    ast_ident
	    ast_local
	    ast_env
	    ast_sexp
	    ast_private
	    ast_lvtype
	    ast_dump
	    ast_walk
	    engine_param
	    backend_backend)
   (export  (nums-walk! globals))
   (static  (wide-class local/info::local
	       (escape::bool (default #f)))))

;*---------------------------------------------------------------------*/
;*    nums-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (nums-walk! globals)
   (pass-prelude "Nums")
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    nums-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (nums-fun! var)
   (enter-function (variable-id var))
   (let* ((fun (variable-value var))
	  (body (sfun-body fun)))
      (mark-cell body)
      (nums! body)
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    mark-cell ...                                                    */
;*    -------------------------------------------------------------    */
;*    Mark cells that escape, i.e., that are not only used in          */
;*    box-ref and box-set! expressions.                                */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-cell node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-cell ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-cell node::var)
   (when (isa? (var-variable node) local/info)
      (with-access::local/info (var-variable node) (escape)
	 (set! escape #t))))
   
;*---------------------------------------------------------------------*/
;*    mark-cell ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-cell node::let-var)
   (with-access::let-var node (bindings body)
      (for-each (lambda (binding)
		   (when (and (eq? (variable-access (car binding)) 'read)
			      (make-box? (cdr binding)))
		      (widen!::local/info (car binding))))
	 bindings))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-cell ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-cell node::box-ref)
   node)

;*---------------------------------------------------------------------*/
;*    mark-cell ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-cell node::box-set!)
   (mark-cell (box-set!-value node)))

;*---------------------------------------------------------------------*/
;*    nums! ...                                                      */
;*    -------------------------------------------------------------    */
;*    Remove cells that never escape                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (nums! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    nums! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (nums! node::let-var)
   (with-access::let-var node (bindings body)
      (for-each (lambda (binding)
		   (when (and (eq? (variable-access (car binding)) 'read)
			      (make-box? (cdr binding)))
		      (with-access::local/info (car binding) (escape type)
			 (unless escape
			    (set-cdr! binding (make-box-value (cdr binding)))
			    (set! type *obj*)))))
	 bindings))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    nums! ::box-ref ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (nums! node::box-ref)
   (let ((var (var-variable (box-ref-var node))))
      (if (isa? var local/info)
	  (with-access::local/info var (escape)
	     (if escape
		 node
		 (box-ref-var node)))
	  node)))

;*---------------------------------------------------------------------*/
;*    nums! ::box-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (nums! node::box-set!)
   (let ((var (var-variable (box-set!-var node))))
      (if (isa? var local/info)
	  (with-access::local/info var (escape)
	     (if escape
		 (call-default-walker)
		 (instantiate::setq
		    (type *unspec*)
		    (var (box-set!-var node))
		    (value (nums! (box-set!-value node))))))
	  (call-default-walker))))


