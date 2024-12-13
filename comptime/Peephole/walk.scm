;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Peephole/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Fri Dec 13 14:16:59 2024 (serrano)                */
;*    Copyright   :  2010-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Various peephole optimizations:                                  */
;*      - (string-ref (symbol->string e) n)                            */
;*        => (string-ref (symbol->string! e) n)                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module peephole_walk
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
   (export  (peephole-walk! globals)))

;*---------------------------------------------------------------------*/
;*    peephole-walk! ...                                               */
;*---------------------------------------------------------------------*/
(define (peephole-walk! globals)
   (pass-prelude "peephole" init-peephole-cache!) 
   (for-each peephole-fun! globals)
   (pass-postlude globals clear-peephole-cache!))

;*---------------------------------------------------------------------*/
;*    global variables ...                                             */
;*---------------------------------------------------------------------*/
(define *$string-ref* #f)
(define *string-ref* #f)
(define *string-ref-ur* #f)
(define *substring* #f)
(define *symbol->string* #f)
(define *symbol->string!* #f)

;*---------------------------------------------------------------------*/
;*    init-peephole-cache! ...                                         */
;*---------------------------------------------------------------------*/
(define (init-peephole-cache!)
   (set! *$string-ref* (find-global/module '$string-ref 'foreign))
   (set! *string-ref* (find-global/module 'string-ref '__r4_strings_6_7))
   (set! *string-ref-ur* (find-global/module 'string-ref '__r4_strings_6_7))
   (set! *substring* (find-global/module 'substring '__r4_strings_6_7))
   (set! *symbol->string* (find-global/module 'symbol->string '__r4_symbols_6_4))
   (set! *symbol->string!* (find-global/module 'symbol->string! '__r4_symbols_6_4)))

;*---------------------------------------------------------------------*/
;*    clear-peephole-cache! ...                                        */
;*---------------------------------------------------------------------*/
(define (clear-peephole-cache!)
   (set! *$string-ref* #f)
   (set! *string-ref* #f)
   (set! *string-ref-ur* #f)
   (set! *substring* #f)
   (set! *symbol->string* #f)
   (set! *symbol->string!* #f))

;*---------------------------------------------------------------------*/
;*    peephole-fun! ...                                                */
;*---------------------------------------------------------------------*/
(define (peephole-fun! var)
   (with-trace 'peephole "peephole-fun"
      (trace-item "fun: " (shape var))
      (enter-function (variable-id var))
      (let ((fun (variable-value var)))
	 (sfun-body-set! fun (peephole! (sfun-body fun)))
	 (leave-function)
	 var)))

;*---------------------------------------------------------------------*/
;*    peephole! ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (peephole! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    peephole! ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (peephole! node::let-var)
   (call-default-walker)
   (with-access::let-var node (body)
      (cond
	 ((string-ref-app? body) (peephole-string-ref! node))
	 (else node))))

;*---------------------------------------------------------------------*/
;*    string-ref-app? ...                                              */
;*---------------------------------------------------------------------*/
(define (string-ref-app?::bool node::node)
   (when (isa? node app)
      (with-access::app node (fun)
	 (with-access::var fun (variable)
	    (or (eq? variable *$string-ref*)
		(eq? variable *string-ref*)
		(eq? variable *string-ref-ur*)
		(eq? variable *substring*))))))

;*---------------------------------------------------------------------*/
;*    symbol->string-app? ...                                          */
;*---------------------------------------------------------------------*/
(define (symbol->string-app? node::node)
   (when (isa? node app)
      (with-access::app node (fun)
	 (with-access::var fun (variable)
	    (eq? variable *symbol->string*)))))
       
;*---------------------------------------------------------------------*/
;*    peephole-string-ref! ...                                         */
;*---------------------------------------------------------------------*/
(define (peephole-string-ref! node::let-var)

   (define (normalize e::node env)
      (cond
	 ((isa? e var)
	  (with-access::var e (variable)
	     (let ((c (assq variable env)))
		(if (pair? c)
		    (normalize (cdr c) '())
		    e))))
	 ((isa? e let-var)
	  (with-access::let-var e (body)
	     (normalize body '())))
	 (else
	  e)))
   
   (with-access::let-var node (body bindings)
      (with-access::app body (fun args)
	 (let ((actual (normalize (car args) bindings)))
	    (when (symbol->string-app? actual)
	       (with-access::app actual (fun)
		  (with-access::var fun (variable)
		     (tprint "var=" (typeof variable)
			" s=" (typeof *symbol->string!*))
		     (set! variable *symbol->string!*)))))))
   node)

