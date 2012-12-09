;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Inline/app.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 10 18:43:56 1995                          */
;*    Last change :  Sat Dec  8 13:57:51 2012 (serrano)                */
;*    Copyright   :  1995-2012 Manuel Serrano, see LICENSE file        */
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
	    module_library
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
      (trace (inline 3) "inline-app: " (shape node)
	 " mode=" *inline-mode* #\Newline)
      (trace (inline+ 3) "inline-app: " (shape node)
	 " mode=" *inline-mode* #\Newline)
      (cond
	 ((not (sfun? sfun))
	  node)
	 ((inline-app? var kfactor (call-size node) stack)
	  (if (not (eq? (sfun-class sfun) 'sifun))
	      (set! *inlined-calls* (+fx *inlined-calls* 1)))
	  (when (and (global? var) (global-library var))
	     ;; MS 17feb2012, when inlining a library function,
	     ;; marks that its module needs to be initialized
	     (with-library-module! (global-module var)))
	  (if (and *optim-loop-inlining?* (is-recursive? var))
	      (inline-app-recursive node kfactor stack)
	      (inline-app-simple node kfactor stack "simple")))
	 (else
	  node))))

;*---------------------------------------------------------------------*/
;*    call-size ...                                                    */
;*---------------------------------------------------------------------*/
(define (call-size node::app)
   (with-access::app node (args)
      (let ((csize (+fx 1 (length args))))
	 (if *optim-atom-inlining?*
	     (let ((atoms (filter (lambda (x)
				     (or (isa? x atom) (isa? x kwote)))
			     args)))
		(+fx csize (length atoms)))
	     csize))))

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
   (trace inline+
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
	  (trace inline+ " no (no-inlining option)" #\Newline)
          #f)
         ((and *optim-loop-inlining?*
	       (not *optim-unroll-loop?*)
	       (memq var stack))
          ;; no we won't because we are already inlining a app to `fun'
          #f)
	 ((eq? (sfun-class sfun) 'snifun)
	  (trace inline " no (declared snifun)" #\Newline)
	  (trace inline+ " no (declared snifun)" #\Newline)
	  ;; non inlinable functions are never inlined (sic !).
	  #f)
	 ((and (not (eq? *inline-mode* 'all))
	       (eq? (sfun-class sfun) 'sifun)
	       (is-recursive? var))
	  (trace inline " no (recursive)" #\Newline)
	  (trace inline+ " no (recursive)" #\Newline)
	  ;; recursive functions can be inlined only on the first inlining
	  #f)
	 ((eq? (sfun-class sfun) 'sgfun)
	  (trace inline " no (generic)" #\Newline)
	  (trace inline+ " no (generic)" #\Newline)
	  ;; don't inline generic (they use find-method which is inline)
	  #f)
	 ((and (eq? (sfun-class sfun) 'sifun)
	       (not (memq var stack))
	       (or (not (eq? *inline-mode* 'reducer))
		   (not (contains-kwote? (sfun-body sfun))))
	       (or (not (eq? *inline-mode* 'predicate))
		   (fun-predicate-of sfun)))
	  ;; yes, because the function has been declared inline
	  (trace inline " yes (sifun) mode=" *inline-mode* #\Newline)
	  (trace inline+ " yes (sifun) mode=" *inline-mode* #\Newline)
	  #t)
	 ((and (global? var) (eq? (global-import var) 'import))
          ;; of course not.
          (trace inline " no (import)" #\Newline)
	  (trace inline+ " no (import)" #\Newline)
          #f)
	 ((not *user-inlining?*)
	  (trace inline " no, not user-inlining...\n")
	  (trace inline+ " no, not user-inlining...\n")
	  #f)
	 ((<fx (node-size body) (*fx kfactor call-size))
          ;; yes, because the size does not grew
	  (trace inline " yes, small enough (size: "
 		 (node-size body)
		 " max: " (*fx kfactor call-size)
		 ")"
		 #\newline)
	  (trace inline+ " yes, small enough (size: "
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
	  (trace inline+ " yes, same size and not in stack (size: "
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
	  (trace inline+ " no, too large (size: "
		 (node-size body)
		 " max: " (*fx kfactor call-size)
		 ")"
		 #\newline)
          #f))))
 
