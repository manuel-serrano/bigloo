;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Inline/app.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 10 18:43:56 1995                          */
;*    Last change :  Fri Sep  4 08:32:57 2009 (serrano)                */
;*    Copyright   :  1995-2009 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The inlining of application node                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_app
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    tools_error
	    type_type
	    ast_var
	    ast_node
	    inline_walk
	    inline_inline
	    inline_size
	    inline_simple
	    inline_recursion)
   (export  (inline-app::node ::app ::long stack)
	    (inline-app?::bool ::variable ::long ::long ::obj)))

;*---------------------------------------------------------------------*/
;*    inline-app ...                                                   */
;*---------------------------------------------------------------------*/
(define (inline-app node kfactor stack)
   (let* ((callee (app-fun node))
	  (var    (var-variable callee))
	  (sfun   (variable-value var))
	  (args   (app-args node))
	  (loc    (node-loc node)))
      (trace (inline 3) "inline-app: " (shape node) #\Newline)
      (trace (inline+ 3) "inline-app: " (shape node) #\Newline)
      (if (not (sfun? sfun))
	  node
	  (if (inline-app? var
			   kfactor
			   (+fx 1 (length (app-args node)))
			   stack)
	      (begin
		 (if (not (eq? (sfun-class sfun) 'sifun))
		     (set! *inlined-calls* (+fx *inlined-calls* 1)))
		 (if (and *optim-loop-inlining?* (is-recursive? var))
		     (inline-app-recursive node kfactor stack)
		     (inline-app-simple node kfactor stack "simple")))
	      node))))

;*---------------------------------------------------------------------*/
;*    contains-kwote? ...                                              */
;*---------------------------------------------------------------------*/
(define (contains-kwote? node)
   (bind-exit (return)
      (node-walk node (lambda (n) (if (kwote? n) (return #t))))
      #f))
   
;*---------------------------------------------------------------------*/
;*    inline-app? ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-app? var::variable kfactor::long call-size::long stack)
   (trace inline
	  "inline-app?: " (shape var)
	  " [kfactor:" kfactor
	  "] [stack: " (shape stack) "] ... ")
   (let* ((sfun (variable-value var))
	  (body (if (isfun? sfun)
		    (isfun-original-body sfun)
		    (sfun-body sfun))))
      (cond
         ((not *inlining?*)
          ;; no, because the user said so
          (trace inline " no (no-inlining option)" #\Newline)
          #f)
         ((and *optim-loop-inlining?*
	       (not *optim-unroll-loop?*)
	       (memq var stack))
          ;; no we won't because we are already inlining a app to `fun'
          #f)
	 ((eq? (sfun-class sfun) 'snifun)
	  (trace inline " no (declared snifun)" #\Newline)
	  ;; non inlinable functions are never inlined (sic !).
	  #f)
	 ((and (eq? (sfun-class sfun) 'sifun)
	       (not (memq var stack))
	       (or (not (eq? *inline-mode* 'reducer))
		   (not (contains-kwote? (sfun-body sfun))))
	       (or (not (eq? *inline-mode* 'predicate))
		   (fun-predicate-of sfun)))
	  ;; yes, because the function has been declared inline
	  (trace inline " yes (sifun) mode=" *inline-mode* #\Newline)
	  #t)
	 ((and (global? var) (eq? (global-import var) 'import))
          ;; of course not.
          (trace inline " no (import)" #\Newline)
          #f)
	 ((not *user-inlining?*)
	  (trace inline " no, not user-inlining...\n")
	  #f)
	 ((<fx (node-size body) (*fx kfactor call-size))
          ;; yes, because the size does not grew
	  (trace inline " yes, small enough (size: "
 		 (node-size body)
		 " max: " (*fx kfactor call-size)
		 ")"
		 #\newline)
          #t)
	 ((and (=fx (node-size body) call-size) (not (memq var stack)))
	  ;; yes, because the call and the body are of the same size
	  ;; and we are not inlining an infinite loop
	  (trace inline " yes, same size and not in stack (size: "
 		 (node-size body)
		 ")"
		 #\newline)
	  #t) 
	 (else
          ;; no, because the function is too large
          (trace inline " no, too large (size: "
		 (node-size body)
		 " max: " (*fx kfactor call-size)
		 ")"
		 #\newline)
          #f))))
 
