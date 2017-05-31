;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Isa/walk.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Wed May 31 10:45:46 2017 (serrano)                */
;*    Copyright   :  2010-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Replace isa? calls with specialized inlinable versions           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module isa_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_location
	    type_cache
	    type_misc
	    type_typeof
	    ast_ident
	    ast_local
	    ast_env
	    ast_sexp
	    ast_private
	    ast_lvtype
	    ast_dump
	    ast_walk
	    object_class
	    engine_param)
   (export  (isa-walk! globals)))

;*---------------------------------------------------------------------*/
;*    isa-walk! ...                                                    */
;*---------------------------------------------------------------------*/
(define (isa-walk! globals)
   (pass-prelude "Isa" init-isa-cache!) 
   (for-each isa-fun! globals)
   (pass-postlude globals clear-isa-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *isa/cdepth* #f)
(define *isa-object/cdepth* #f)
(define *isa/final* #f)
(define *isa-object/final* #f)

;*---------------------------------------------------------------------*/
;*    init-isa-cache! ...                                              */
;*---------------------------------------------------------------------*/
(define (init-isa-cache!)
   (unless (global? *isa/cdepth*)
      (set! *isa/cdepth* (find-global/module '%isa/cdepth? '__object))
      (set! *isa-object/cdepth* (find-global/module '%isa-object/cdepth? '__object))
      (set! *isa/final* (find-global/module '%isa/final? '__object))
      (set! *isa-object/final* (find-global/module '%isa-object/final? '__object)))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-isa-cache! ...                                             */
;*---------------------------------------------------------------------*/
(define (clear-isa-cache!)
   (set! *isa/cdepth* #f)
   (set! *isa-object/cdepth* #f)
   (set! *isa/final* #f)
   (set! *isa-object/final* #f))

;*---------------------------------------------------------------------*/
;*    isa-fun! ...                                                     */
;*---------------------------------------------------------------------*/
(define (isa-fun! var)
   (enter-function (variable-id var))
   (let* ((fun (variable-value var))
	  (body (sfun-body fun)))
      (set! body (isa! body))
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    isa! ...                                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (isa! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    isa! ...                                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (isa! node::app)
   
   (define (uncasted-type node::node)
      (if (isa? node cast)
	  (with-access::cast node (arg)
	     (node-type arg))
	  (node-type node)))

   (define (static-final-class? typ)
      (and (eq? (global-import (tclass-holder typ)) 'static)
	   (null? (tclass-subclasses typ))))
   
   (let ((typ (isa-of node)))
      (if typ
	  (with-access::app node (fun args loc)
	     (cond
		((or (static-final-class? typ)
		     (and #f (tclass-final? typ)))
		 (if (isa? (uncasted-type (car args)) tclass)
		     (let ((nfun (duplicate::var fun
				    (variable *isa-object/final*))))
			(set-car! args
			   (instantiate::cast
			      (arg (car args))
			      (type (get-object-type))))
			(set! fun nfun))
		     (let ((nfun (duplicate::var fun
				    (variable *isa/final*))))
			(set! fun nfun))))
		((< (tclass-depth typ) (bigloo-config 'class-display-min-size))
		 (if (isa? (uncasted-type (car args)) tclass)
		     (let ((nfun (duplicate::var fun
				    (variable *isa-object/cdepth*)))
			   (depth (instantiate::literal
				     (loc loc)
				     (type (get-type-atom (tclass-depth typ)))
				     (value (tclass-depth typ))))
			   (arg0 (instantiate::cast
				    (arg (car args))
				    (type (get-object-type)))))
			(set! fun nfun)
			(set! args (list arg0 (cadr args) depth)))
		     (let* ((nfun (duplicate::var fun
				     (variable *isa/cdepth*)))
			    (depth (instantiate::literal
				      (loc loc)
				      (type (get-type-atom (tclass-depth typ)))
				      (value (tclass-depth typ)))))
			(set! fun nfun)
			(set! args (append args (list depth)))))))
	     node)
	  (call-default-walker))))
		  
