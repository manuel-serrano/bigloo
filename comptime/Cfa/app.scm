;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/comptime/Cfa/app.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 24 17:36:29 1996                          */
;*    Last change :  Sun Jun 21 14:54:50 2026 (serrano)                */
;*    Copyright   :  1996-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The cfa on `app' node                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_app
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    module_module
	    cfa_info
	    cfa_info2
	    cfa_cfa
	    cfa_iterate
	    cfa_loose
	    cfa_approx
	    cfa_procedure)
   (export  (generic app!::approx ::fun ::var approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::app ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::app)
   (with-access::app node (fun args)
      (with-trace 'cfa_app "cfa! ::app"
	 (trace-item "fun=" (shape fun))
	 (trace-item "node=" (shape node))
	 (let ((args-approx (map cfa! args)))
	    (trace-item "args=" (map shape args-approx))
	    (app! (variable-value (var-variable fun)) fun args-approx)))))

;*---------------------------------------------------------------------*/
;*    app! ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (app!::approx fun::fun var::var args-approx)
   (internal-error "app!" "No method for this function" (cons fun (shape var))))

;*---------------------------------------------------------------------*/
;*    app! ::intern-sfun/Cinfo ...                                     */
;*---------------------------------------------------------------------*/
(define-method (app! fun::intern-sfun/Cinfo var::var args-approx)
   (with-trace 'cfa_app "app! ::intern-sfun/Cinfo"
      (trace-item "var=" (shape var))
      (trace-item "scope=" (if (local? (var-variable var))
			       "local"
			       (global-import (var-variable var))))
      (with-access::intern-sfun/Cinfo fun (args polymorphic? approx)
	 (trace-item "args=" (map typeof args))
	 ;; set the new formals approximation
	 (for-each (lambda (arg approx)
		      (unless (isa? arg local)
			 (tprint "PAS BON FUN=" (shape var)))
		      (union-approx! (svar/Cinfo-approx (local-value arg))
			 approx))
	    args
	    args-approx)
	 ;; and jump to the function body
	 (cond
	    ((or (not (global? (var-variable var)))
		 (and (global? (var-variable var))
		      (eq? (global-import (var-variable var)) 'static)))
	     ;; this is a unexported function
	     (cfa-intern-sfun! fun (var-variable var)))
	    ((and (eq? (global-import (var-variable var)) 'export)
		  ;; using module5, a variable might be exported but
		  ;; declared in another module
		  (eq? (global-module (var-variable var)) *module*))
	     ;; this is an exported function
	     (cfa-export-var! fun (var-variable var)))
	    (else
	     approx)))))

;*---------------------------------------------------------------------*/
;*    app! ::extern-sfun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (app! fun::extern-sfun/Cinfo var::var args-approx)
   (with-access::extern-sfun/Cinfo fun (top? approx polymorphic?)
      (with-trace 'cfa_app "app! ::extern-sfun"
	 (trace-item "var=" (shape var))
	 ;; we set the new formals approximation
	 (if top?
	     ;; calling a random extern function, loose everyting
	     (for-each (lambda (a) (loose! a)) args-approx)
	     ;; don't loose but mark functions as not candidate to X/T optim
	     (for-each (lambda (a) (disable-X-T! a "extern call")) args-approx))
	 (when polymorphic?
	    (with-access::approx approx (type)
	       (set! type (get-bigloo-type (approx-type approx)))))
	 ;; and we return the global approximation
	 approx)))
   
;*---------------------------------------------------------------------*/
;*    app! ::cfun/Cinfo ...                                            */
;*---------------------------------------------------------------------*/
(define-method (app! fun::cfun/Cinfo var::var args-approx)
   (with-trace 'cfa_app "app! ::cfun/Cinfo"
      (with-access::cfun/Cinfo fun (top? approx)
	 ;; we set the new formals approximation
	 (if top?
	     (for-each (lambda (a) (loose! a)) args-approx)
	     ;; don't loose but mark functions as not candidate to X/T optim
	     (for-each (lambda (a) (disable-X-T! a "extern call")) args-approx))
	 ;; and we return the global approximation
	 approx)))
