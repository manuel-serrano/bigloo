;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Nums/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Mon Jun 30 09:00:31 2025 (serrano)                */
;*    Copyright   :  2010-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This pass replace double type predicate invokation with a single */
;*    combined test. I.e., it rewrites:                                */
;*      (if (if ($fixnum? x) ($fixnum? y) #f) true false)              */
;*    with                                                             */
;*      (if ($fixnums? x y) true false)                                */
;*                                                                     */
;*    and the same thing for flonums.                                  */
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
;*    Nums cache                                                       */
;*---------------------------------------------------------------------*/
(define *$fixnum?* #f)
(define *$flonum?* #f)
(define *$fast-flonum?* #f)
(define *$fixnums?* #f)
(define *$flonums?* #f)
(define *$fast-flonums?* #f)

;*---------------------------------------------------------------------*/
;*    nums-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (nums-walk! globals)
   (pass-prelude "Nums")
   (init-cache!)
   (for-each nums-fun! globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    init-cache! ...                                                  */
;*---------------------------------------------------------------------*/
(define (init-cache!)
   (set! *$fixnum?* (find-global '$fixnum? 'foreign))
   (set! *$flonum?* (find-global '$flonum? 'foreign))
   (set! *$fast-flonum?* (find-global '$fast-flonum? 'foreign))
   (set! *$fixnums?* (find-global '$fixnums? 'foreign))
   (set! *$flonums?* (find-global '$flonums? 'foreign))
   (set! *$fast-flonums?* (find-global '$fast-flonums? 'foreign)))

;*---------------------------------------------------------------------*/
;*    nums-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (nums-fun! var)
   (enter-function (variable-id var))
   (let* ((fun (variable-value var))
	  (body (sfun-body fun)))
      (nums! body)
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    nums! ...                                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (nums! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    nums! ::cond ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (nums! node::conditional)
   (call-default-walker)
   (with-access::conditional node (test loc)
      (cond
	 ((preds? test *$fixnum?*)
	  =>
	  (lambda (args) (set! test (preds *$fixnums?* args loc))))
	 ((preds? test *$fast-flonum?*)
	  =>
	  (lambda (args) (set! test (preds *$fast-flonums?* args loc))))
	 ((preds? test *$flonum?*)
	  =>
	  (lambda (args) (set! test (preds *$flonums?* args loc))))))
   node)

;*---------------------------------------------------------------------*/
;*    preds? ...                                                       */
;*    -------------------------------------------------------------    */
;*    Detect the pattern (if ($fixnum? x) ($fixnum? y) #f)             */
;*---------------------------------------------------------------------*/
(define (preds? expr::node pred)
   
   (define (isfalse? node)
      (when (isa? node literal)
	 (with-access::literal node (value)
	    (eq? value #f))))
   
   (when (isa? expr conditional)
      (with-access::conditional expr (test true false)
	 (when (and (isfalse? false) (isa? test app))
	    (with-access::app test (fun (args0 args))
	       (when (and (eq? (var-variable fun) pred) (isa? true app))
		   (with-access::app true (fun (args1 args))
		      (when (eq? (var-variable fun) pred)
			  (list (car args0) (car args1))))))))))

;*---------------------------------------------------------------------*/
;*    preds ...                                                        */
;*---------------------------------------------------------------------*/
(define (preds pred::global args loc)
   (instantiate::app
      (type (variable-type pred))
      (loc loc)
      (fun (instantiate::ref
	      (type (variable-type pred))
	      (loc loc)
	      (variable pred)))
      (args args)))

