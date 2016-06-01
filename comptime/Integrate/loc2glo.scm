;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/loc2glo.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 15 17:29:48 1995                          */
;*    Last change :  Wed Jun  1 17:45:25 2016 (serrano)                */
;*    Copyright   :  1995-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We translate a local function definition into a global one.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_local->global
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_args
	    module_module
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_glo-def
	    ast_env
	    ast_local
	    integrate_info
	    integrate_node)
   (export  (local->global ::local)
	    (the-global ::local)))

;*---------------------------------------------------------------------*/
;*    local->global ...                                                */
;*---------------------------------------------------------------------*/
(define (local->global local)
   (trace (integrate 2) (shape local) ", local->global:\n")
   (trace (integrate 3) (shape (sfun-body (local-value local))) "\n")
   (let* ((global   (the-global local))
	  (kaptured (sfun/Iinfo-kaptured (local-value local)))
	  (add-args (map (lambda (old)
			    (clone-local old
					 (duplicate::svar/Iinfo
					       (local-value old))))
			 kaptured))
	  (old-fun  (local-value local))
	  (new-fun  (duplicate::sfun old-fun
		       (arity (+-arity (sfun-arity old-fun) (length add-args)))
		       (args (append (reverse add-args)
				     (sfun-args old-fun))))))
      ;; we set the result type
      (global-type-set! global (local-type local))
      (for-each (lambda (l)
		   (if (integrate-celled? l)
		       (local-type-set! l *obj*)))
		(sfun-args new-fun))
      (sfun-body-set! new-fun
		      (integrate-globalize! (sfun-body old-fun)
					    local
					    (map cons kaptured add-args)))
      (global-value-set! global new-fun)
      global))

;*---------------------------------------------------------------------*/
;*    symbol-quote ...                                                 */
;*---------------------------------------------------------------------*/
(define symbol-quote (string->symbol "'"))

;*---------------------------------------------------------------------*/
;*    local-id->global-id ...                                          */
;*    -------------------------------------------------------------    */
;*    Generates a new global name for the globalized local function.   */
;*---------------------------------------------------------------------*/
(define (local-id->global-id local)
   (let ((p (string-append (symbol->string (local-id local)) "~")))
      (let loop ((count 0))
	 (let ((id (string->symbol (string-append p (integer->string count)))))
	    (if (global? (find-global/module id *module*))
		(loop (+fx count 1))
		id)))))

;*---------------------------------------------------------------------*/
;*    the-global ...                                                   */
;*---------------------------------------------------------------------*/
(define (the-global local::local)
   (let ((value (local-value local)))
      (if (global? (sfun/Iinfo-global value))
	  (sfun/Iinfo-global value)
	  (let* ((id     (local-id->global-id local))
		 (global (def-global-sfun-no-warning! id
			   ;; we set dummy empty args-id 
			   ;; and dummy empty args because a new-fun
			   ;; will be allocated.
			   '()
			   '()
			   *module*
			   'sfun
			   'a-integrated-body
			   'now
			   #unspecified)))
	     ;; we have to propagate the location definition
	     ;; of the local variable
	     (sfun-loc-set! (global-value global) (sfun-loc value))
	     ;; we check if the function is a user one
	     (if (not (local-user? local))
		 (global-user?-set! global #f))
	     (sfun/Iinfo-global-set! value global)
	     (sfun-side-effect-set! (global-value global)
				    (sfun-side-effect value))
	     global))))
