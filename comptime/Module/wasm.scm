;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Module/wasm.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 16:28:03 1996                          */
;*    Last change :  Tue Jul  2 07:36:23 2024 (serrano)                */
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
;*    check-wasm-args? ...                                             */
;*---------------------------------------------------------------------*/
(define (check-wasm-args? proto)
   (let loop ((proto proto))
      (cond
	 ((null? proto) #t)
	 ((symbol? proto) #t)
	 ((not (pair? proto)) #f)
	 ((not (symbol? (car proto))) #f)
	 (else (loop (cdr proto))))))

;*---------------------------------------------------------------------*/
;*    wasm-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (wasm-parser wasm exportp)
   (trace (ast 2) "wasm parser: " wasm #\Newline)
   (match-case wasm
      ((export (and (? symbol?) ?bname) (and (? string?) ?cname))
       (set! *wasm-exported* (cons (cons wasm exportp) *wasm-exported*)))
      (((or export type include) . ?-)
       ;; an error
       (user-error "Parse error" "Illegal wasm form" wasm '()))
      ((or (macro ?type ?l-name ?proto ?c-name)
	   (infix macro ?type ?l-name ?proto ?c-name))
       (if (not (and (string? c-name)
		     (symbol? type)
		     (symbol? l-name)
		     (check-wasm-args? proto)))
	   (user-error "Parse error" "Illegal `macro' form" wasm '())
	   (declare-global-cfun! l-name #f 'wasm c-name
	      type proto (eq? (car wasm) 'infix) #t wasm wasm)))
      ((macro ?type ?l-name ?c-name)
       (if (not (and (string? c-name)
		     (symbol? type)
		     (symbol? l-name)))
	   (user-error "Parse error" "Illegal `macro' form" wasm '())
	   (declare-global-cvar! l-name #f c-name type #t wasm wasm)))
      ((macro . ?-)
       (user-error "Parse error" "Illegal wasm form" wasm '()))
      (else
       (user-error "Parse error" "Illegal wasm form" wasm '()))))

;*---------------------------------------------------------------------*/
;*    *wasm-exported* ...                                              */
;*---------------------------------------------------------------------*/
(define *wasm-exported* '())

;*---------------------------------------------------------------------*/
;*    wasm-finalizer ...                                               */
;*---------------------------------------------------------------------*/
(define (wasm-finalizer)
   ;; we patch bigloo wasm exported variables
   (for-each (lambda (wasm)
		(let* ((fo (car wasm))
		       (ex (cdr wasm))
		       (global (find-global (cadr fo)))
		       (name (caddr fo)))
		   (cond
		      ((not (global? global))
		       (when ex
			  (if (not (or (eq? *pass* 'make-add-heap)
				       (eq? *pass* 'make-heap)))
			      (user-error "Wasm"
				 (format "Unbound global variable \"~a\""
				    (cadr fo))
				 wasm
				 '()))))
		      ((string? (global-name global))
		       (user-warning
			  "Wasm"
			  "Re-exportation of global variable (ignored)"
			  wasm))
		      (else
		       (global-name-set! global name)))))
      *wasm-exported*)
   (set! *wasm-exported* '())
   ;; and we build a unit if Wasm types have generated codes
   'void)
	  
