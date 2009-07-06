;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/app.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 24 17:36:29 1996                          */
;*    Last change :  Wed Feb 22 18:08:38 2006 (serrano)                */
;*    Copyright   :  1996-2006 Manuel Serrano, see LICENSE file        */
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
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_cfa
	    cfa_iterate
	    cfa_loose
	    cfa_approx)
   (export  (generic app!::approx ::fun ::var approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::app ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::app)
   (trace (cfa 2) " app: " (shape node) #\Newline)
   (with-access::app node (fun args)
      (app! (variable-value (var-variable fun)) fun (map cfa! args))))

;*---------------------------------------------------------------------*/
;*    app! ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (app!::approx fun::fun var::var args-approx)
   (internal-error "app!"
		   "No method for this function"
		   (cons fun (shape var))))

;*---------------------------------------------------------------------*/
;*    app! ::intern-sfun/Cinfo ...                                     */
;*---------------------------------------------------------------------*/
(define-method (app! fun::intern-sfun/Cinfo var::var args-approx)
   (trace (cfa 3) "  app(intern)!: " (shape var) " " (shape args-approx)
	  #\Newline)
   (with-access::intern-sfun/Cinfo fun (args)
      ;; we set the new formals approximation
      (for-each (lambda (formal approx)
		   (union-approx! (svar/Cinfo-approx (local-value formal))
				  approx))
		args
		args-approx)
      ;; and we jump to the function body
      (if (or (not (global? (var-variable var)))
	      (and (global? (var-variable var))
		   (eq? (global-import (var-variable var)) 'static)))
	  ;; this is a unexported function 
	  (cfa-intern-sfun! fun (var-variable var))
	  ;; this is an exported function
	  (cfa-export-var! fun (var-variable var)))))

;*---------------------------------------------------------------------*/
;*    app! ::extern-sfun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (app! fun::extern-sfun/Cinfo var::var args-approx)
   (trace (cfa 3) "  app(extern)!: " (shape var) " " (shape args-approx)
	  #\Newline)
   (with-access::extern-sfun/Cinfo fun (top? approx)
      ;; we set the new formals approximation
      (if top? (for-each (lambda (a) (loose! a 'all)) args-approx))
      ;; and we return the global approximation
      approx))
   
;*---------------------------------------------------------------------*/
;*    app! ::cfun/Cinfo ...                                            */
;*---------------------------------------------------------------------*/
(define-method (app! fun::cfun/Cinfo var::var args-approx)
   (trace (cfa 3) "  app(cfun)!: " (shape var) #\Newline)
   (with-access::cfun/Cinfo fun (top? approx)
      ;; we set the new formals approximation
      (if top? (for-each (lambda (a) (loose! a 'all)) args-approx))
      ;; and we return the global approximation
      approx))
