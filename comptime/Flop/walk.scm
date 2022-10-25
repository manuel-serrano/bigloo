;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Flop/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Tue Oct 25 05:54:13 2022 (serrano)                */
;*    Copyright   :  2010-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Optimize flonum operations by propagation ::real type in         */
;*    expressions. This optimization replaces generic operations with  */
;*    flonum operations and pushes the new types to the surrounding    */
;*    expression. Eg.                                                  */
;*    (2+ a (2+ 1.0 c)) => (+fl (->flonum a) (+fx 1.0 (->flonum c)))   */
;*    untagging operations. Typically, replaces:                       */
;*       (let ((z1::long (bint->long e1))                              */
;*             (z2::long e2))                                          */
;*          (long->bint (+ z1 z2)))                                    */
;*    with                                                             */
;*        ($addfl e1 (long->bint e2))                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module flop_walk
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
   (export  (flop-walk! globals)
	    (init-flop-cache!)))

;*---------------------------------------------------------------------*/
;*    flop-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (flop-walk! globals)
   (pass-prelude "Flop" init-flop-cache!) 
   (for-each flop-fun! globals)
   (pass-postlude globals clear-flop-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *flops* '())
(define *flonum?* #f)

;*---------------------------------------------------------------------*/
;*    init-flop-cache! ...                                             */
;*---------------------------------------------------------------------*/
(define (init-flop-cache!)
   (unless (pair? *flops*)
      (set! *flonum?* (find-global '$flonum? 'foreign))
      (set! *flops*
	 (map (lambda (op)
		 (cons (find-global (symbol-append '|2| op) '__r4_numbers_6_5)
		    (find-global (symbol-append 'c- op 'fl) 'foreign)))
	    '(+ - / *))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-flop-cache! ...                                            */
;*---------------------------------------------------------------------*/
(define (clear-flop-cache!)
   (set! *flonum?* #f)
   (set! *flops* '()))

;*---------------------------------------------------------------------*/
;*    flop-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (flop-fun! var)
   (enter-function (variable-id var))
   (let ((fun (variable-value var)))
      (sfun-body-set! fun (flop! (sfun-body fun)))
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    flop! ...                                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (flop! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    flop! ::let-var ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (flop! node::let-var)

   (define (find-flop expr)
      (when (isa? expr app)
	 (let ((v (var-variable (app-fun expr))))
	    (let ((c (assq v *flops*)))
	       (when c (cdr c))))))

   (define (flonum? expr)
      #f)
   
   (define (flop-bool-letvar? bindings body)
      ;; true iff:
      ;;   - there are one binding
      ;;   - it is a flonum call
      ;;   - the body is a call to a generic binary arithmetic op
      (and (=fx (length bindings) 1)
	   (app? body)
	   (find-flop (car (app-args body)))
	   (flonum? (cdar bindings))))

   (call-default-walker))
