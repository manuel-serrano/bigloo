;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/comptime/Cfa/app.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 24 17:36:29 1996                          */
;*    Last change :  Thu Jan 29 17:05:30 2026 (serrano)                */
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
      (trace (cfa 2) (cfa-current)
	 ": >>> app " (shape fun) ": " (shape node) #\Newline)
      (let ((args-approx (map cfa! args)))
	 (trace (cfa 3) (cfa-current)
	    ":     args=" (map shape args-approx) #\Newline)
	 (app! (variable-value (var-variable fun)) fun args-approx))))

;*---------------------------------------------------------------------*/
;*    app! ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (app!::approx fun::fun var::var args-approx)
   (internal-error "app!" "No method for this function" (cons fun (shape var))))

;*---------------------------------------------------------------------*/
;*    app! ::intern-sfun/Cinfo ...                                     */
;*---------------------------------------------------------------------*/
(define-method (app! fun::intern-sfun/Cinfo var::var args-approx)
   (with-trace 'cfa "app! ::intern-sfun/Cinfo"
      (trace-item "var=" (shape var))
      (trace-item "scope=" (if (local? (var-variable var))
			       "local"
			       (global-import (var-variable var))))
      (with-access::intern-sfun/Cinfo fun (args polymorphic? approx)
	 (trace (cfa 3)
	    (cfa-current)
	    ": >>> app(intern-sfun/Cinfo)!" " polymorphic?=" polymorphic?
	    #\Newline)
	 (trace (cfa 3) (cfa-current) ": ~~~   app, formals="
	    (map shape args) #\Newline)
	 (trace (cfa 3) (cfa-current) ": ~~~   app, approx="
	    (map shape args-approx) #\Newline)
	 ;; we set the new formals approximation
	 (for-each (lambda (formal approx)
		      (union-approx! (svar/Cinfo-approx (local-value formal))
			 approx))
	    args
	    args-approx)
	 (trace (cfa 3)
	    (cfa-current)
	    ": --- app(intern-sfun/Cinfo)!" " polymorphic?=" polymorphic?
	    #\Newline)
	 (trace (cfa 3) (cfa-current) ": ~~~   app, formals="
	    (map shape args) #\Newline)
	 ;; and we jump to the function body
	 (cond
	    ((or (not (global? (var-variable var)))
		 (and (global? (var-variable var))
		      (eq? (global-import (var-variable var)) 'static)))
	     ;; this is a unexported function 
	     (let ((a (cfa-intern-sfun! fun (var-variable var))))
		(trace (cfa 3) (cfa-current)
		   ": <<< app " (shape var) " <- (intern) " (shape a) "\n")
		a))
	    ((and (eq? (global-import (var-variable var)) 'export)
		  ;; using module5, a variable might be exported but
		  ;; declared in another module
		  (eq? (global-module (var-variable var)) *module*))
	     ;; this is an exported function
	     (let ((a (cfa-export-var! fun (var-variable var))))
		(trace (cfa 3) (cfa-current)
		   ": <<< app " (shape var) " <- (export) " (shape a) "\n")
		a))
	    (else
	     approx)))))

;*---------------------------------------------------------------------*/
;*    app! ::extern-sfun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (app! fun::extern-sfun/Cinfo var::var args-approx)
   (with-access::extern-sfun/Cinfo fun (top? approx polymorphic?)
      (trace (cfa 3) (cfa-current) ": >>>   app(extern-sfun/Cinfo)!"
	 " polymorphic?=" polymorphic? #\Newline)
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
      (trace (cfa 3) (cfa-current)
	 ": <<< app " (shape var) " <- (extern) " (shape approx)
	 #\Newline)
      approx))
   
;*---------------------------------------------------------------------*/
;*    app! ::cfun/Cinfo ...                                            */
;*---------------------------------------------------------------------*/
(define-method (app! fun::cfun/Cinfo var::var args-approx)
   (trace (cfa 3) (cfa-current) ": >>>   app(cfun)!"
      " args=" (shape args-approx)
      #\Newline)
   (with-access::cfun/Cinfo fun (top? approx)
      ;; we set the new formals approximation
      (if top?
	  (for-each (lambda (a) (loose! a)) args-approx)
	  ;; don't loose but mark functions as not candidate to X/T optim
	  (for-each (lambda (a) (disable-X-T! a "extern call")) args-approx))
      (trace (cfa 3) (cfa-current)
	 ": <<< app " (shape var) " <- (foreign) " (shape approx)
	 #\Newline)
      ;; and we return the global approximation
      approx))
