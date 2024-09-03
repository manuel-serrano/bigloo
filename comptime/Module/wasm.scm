;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Module/wasm.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 16:28:03 1996                          */
;*    Last change :  Tue Sep  3 06:43:09 2024 (serrano)                */
;*    Copyright   :  1996-2024 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The wasm clauses compilation. Almost similar to extern clauses.  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_wasm
   (include "Ast/unit.sch"
	    "Tools/trace.sch")
   (import  module_module
	    module_checksum
	    engine_param
	    backend_backend
	    ast_glo-decl
	    tools_error
	    tools_shape
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_env
	    ast_ident
	    (find-location tools_location))
   (export  (make-wasm-compiler)))

;*---------------------------------------------------------------------*/
;*    make-wasm-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-wasm-compiler)
   (instantiate::ccomp
      (id 'wasm)
      (producer wasm-producer)
      (consumer (lambda (m c) (wasm-producer c)))
      (finalizer wasm-finalizer)))

;*---------------------------------------------------------------------*/
;*    wasm-producer ...                                                */
;*---------------------------------------------------------------------*/
(define (wasm-producer clause)
   (if (memq 'wasm (backend-foreign-clause-support (the-backend)))
       (match-case clause
	  ((?- . ?protos)
	   (for-each (lambda (p) (wasm-parser p #t)) protos)
	   '())
	  (else
	   (user-error "Parse error" "Illegal `wasm' clause" clause '())))
       '()))

;*---------------------------------------------------------------------*/
;*    wasm-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (wasm-parser wasm exportp)
   (trace (ast 2) "wasm parser: " wasm #\Newline)
   (match-case wasm
      (((and (? symbol?) ?id) (and (? string?) ?name) . ?deps)
       (set! *wasm-extern* (cons (vector id name deps) *wasm-extern*)))
      (else
       (user-error "Parse error" "Illegal wasm form" wasm '()))))

;*---------------------------------------------------------------------*/
;*    *wasm-extern* ...                                                */
;*---------------------------------------------------------------------*/
(define *wasm-extern* '())

;*---------------------------------------------------------------------*/
;*    wasm-finalizer ...                                               */
;*---------------------------------------------------------------------*/
(define (wasm-finalizer)
   (for-each (lambda (w)
		(let ((id (vector-ref w 0))
		      (name (vector-ref w 1))
		      (deps (vector-ref w 2)))
		   (let ((g (find-global/module id 'foreign)))
		      (if g
			  (begin
			     (global-qualified-type-name-set! g name)
			     (global-pragma-set! g
				(cons (cons 'wasm deps) (global-pragma g))))
			  (error "wasm" "Cannot find extern definition" id)))))
      *wasm-extern*))

	  
