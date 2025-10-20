;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Ast/exit.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 21 14:19:17 1995                          */
;*    Last change :  Mon Oct 20 13:05:12 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `set-exit' and `jmp-exit' management.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_exit
   (include "Ast/node.sch"
            "Tools/trace.sch")
   (import  ast_sexp
	    ast_local
	    ast_ident
	    type_cache
	    tools_progn
	    tools_location)
   (export  (set-exit->node::let-fun     <sexp> <stack> ::obj ::symbol ::obj)
            (jump-exit->node::jump-ex-it <sexp> <stack> ::obj ::symbol ::obj)))

;*---------------------------------------------------------------------*/
;*    set-exit->node ...                                               */
;*    -------------------------------------------------------------    */
;*    set-exit are always compiled as `set-jmp' `longjmp', then, we    */
;*    always have to make them nested into a globalized function.      */
;*    This function is called the `handling' function.                 */
;*---------------------------------------------------------------------*/
(define (set-exit->node exp stack loc site genv)
   
   (define (make-local-exit exit handler)
      (make-local-sexit exit *exit* (instantiate::sexit (handler handler))))
   
   (let ((loc (find-location/loc exp loc)))
      (match-case exp
         ((?- (?exit) ?body)
          (let* ((hdlg-name (mark-symbol-non-user!
			       (make-anonymous-name loc "exit")))
                 (hdlg-sexp `(labels ((,hdlg-name () #unspecified))
                                (,hdlg-name)))
                 (hdlg-node (sexp->node hdlg-sexp stack loc site genv))
                 (hdlg-fun (car (let-fun-locals hdlg-node)))
                 (exit (make-local-exit exit hdlg-fun))
                 (body (sexp->node body (cons exit stack) loc 'value genv))
		 (onexit (instantiate::pragma
			    (loc loc)
			    (type (strict-node-type *obj* *_*))
			    (format "BGL_EXIT_VALUE()")
			    (expr* '())))
                 (exit-body (instantiate::set-ex-it
			       (loc loc)
			       (type (strict-node-type *obj* *_*))
			       (var (instantiate::ref
				       (type *exit*)
				       (loc loc)
				       (variable exit)))
			       (body body)
			       (onexit onexit))))
	     ;; we have to mark that the local is a user function other
	     ;; bdb will get confused and will consider the handling function
	     ;; as a C function
	     (local-user?-set! hdlg-fun #t)
             ;; hdlg-name can't be inlined otherwise the `set-exit'
	     ;; is not correct (due to C setjmp/longjmp semantic)
	     (sfun-class-set! (local-value hdlg-fun) 'snifun)
	     (sfun-body-set!  (local-value hdlg-fun) exit-body)
             hdlg-node))
	 ((?- (?exit) ?body :onexit ?onexit)
	  ;; new form introduced Jun 2021
          (let* ((hdlg-name (mark-symbol-non-user!
			       (make-anonymous-name loc "exit")))
                 (hdlg-sexp `(labels ((,hdlg-name () #unspecified))
                                (,hdlg-name)))
                 (hdlg-node (sexp->node hdlg-sexp stack loc site genv))
                 (hdlg-fun (car (let-fun-locals hdlg-node)))
                 (exit (make-local-exit exit hdlg-fun))
                 (body (sexp->node body (cons exit stack) loc 'value genv))
                 (onexit (sexp->node onexit stack loc 'value genv))
                 (exit-body (instantiate::set-ex-it
			       (loc loc)
			       (type (strict-node-type *obj* *_*))
			       (var (instantiate::ref
				       (type *exit*)
				       (loc loc)
				       (variable exit)))
			       (body body)
			       (onexit onexit))))
	     ;; we have to mark that the local is a user function other
	     ;; bdb will get confused and will consider the handling function
	     ;; as a C function
	     (local-user?-set! hdlg-fun #t)
             ;; hdlg-name can't be inlined otherwise the `set-exit'
	     ;; is not correct (due to C setjmp/longjmp semantic)
	     (sfun-class-set! (local-value hdlg-fun) 'snifun)
	     (sfun-body-set!  (local-value hdlg-fun) exit-body)
             hdlg-node))
         (else
	  (error-sexp->node "Illegal `set-exit' form" exp loc genv)))))

;*---------------------------------------------------------------------*/
;*    jump-exit->node ...                                              */
;*---------------------------------------------------------------------*/
(define (jump-exit->node exp stack loc site genv)
   (let ((loc (find-location/loc exp loc)))
      (match-case exp
         ((?- ?exit . ?value)
          (let ((value (sexp->node (normalize-progn value) stack loc 'value genv))
                (exit (sexp->node exit stack loc 'value genv)))
	     (instantiate::jump-ex-it
		(loc loc)
		(type (strict-node-type *unspec* *_*))
		(exit exit)
		(value value))))
         (else
	  (error-sexp->node "Illegal `jump-exit' form" exp loc genv)))))
         
   
      
