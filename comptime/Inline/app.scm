;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Inline/app.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 10 18:43:56 1995                          */
;*    Last change :  Sat Apr 21 17:30:33 2018 (serrano)                */
;*    Copyright   :  1995-2018 Manuel Serrano, see LICENSE file        */
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
   (when (getenv "INLINE")
      (tprint "inline-app node=" (shape node) " kfactor=" kfactor
	 " stack=" (length stack)))
   (let* ((callee (app-fun node))
	  (var    (var-variable callee))
	  (sfun   (variable-value var))
	  (args   (app-args node))
	  (loc    (node-loc node)))
      (trace (inline inline+ 3) "inline-app: " (shape node)
	 " mode=" *inline-mode* #\Newline)
      (when (getenv "INLINE")
	 (when (sfun? sfun)
	    (tprint "inline-app inline-app?: "
	       (inline-app? var kfactor (call-size node) stack))
	    (tprint "inline-app inline-clo?: "
	       (inline-closure? var stack))))
      (cond
	 ((not (sfun? sfun))
	  node)
	 ((or (inline-app? var kfactor (call-size node) stack)
	      (inline-closure? var stack))
	  (unless (eq? (sfun-class sfun) 'sifun)
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
;*    inline-closure? ...                                              */
;*---------------------------------------------------------------------*/
(define (inline-closure? var stack)
   (when (and (pair? stack) (global? (car stack)))
      (when (eq? var (sfun-the-closure-global (variable-value (car stack))))
	 (and (or (local? var) (eq? (global-import var) 'static))
	      (=fx (variable-occurrence var) 1)))))

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
   (when (getenv "INLINE")
      (tprint "inline-app? var=" (shape var) " kfactor=" kfactor
	 " call-size=" call-size " stack=" (length stack)))
   (trace (inline inline+ 0)
      "inline-app?: " (shape var)
      " [kfactor:" kfactor
      "] [stack: " (shape stack) "] ... ")
   (let* ((sfun (variable-value var))
	  (body (if (isfun? sfun)
		    (isfun-original-body sfun)
		    (sfun-body sfun))))
      (when (getenv "INLINE")
	 (tprint "inline-app? var=" (shape var) "...")
	 (tprint "inline-app? node-size...")
	 (tprint (node-size body)))
      (cond
	 ((not (isa? body node))
	  (trace (inline inline+ 0) " no (no body)" #\Newline)
          #f)
         ((not *inlining?*)
	  (when (getenv "INLINE")
	     (tprint "inline-app? .1"))
          ;; no, because the user said so
          (trace (inline inline+ 0) " no (no-inlining option)" #\Newline)
          #f)
         ((and *optim-loop-inlining?*
	       (not *optim-unroll-loop?*)
	       (memq var stack))
	  (when (getenv "INLINE")
	     (tprint "inline-app? .2"))
          ;; no we won't because we are already inlining a app to `fun'
          #f)
	 ((eq? (sfun-class sfun) 'snifun)
	  (when (getenv "INLINE")
	     (tprint "inline-app? .3"))
	  (trace (inline inline+ 0) " no (declared snifun)" #\Newline)
	  ;; non inlinable functions are never inlined (sic !).
	  #f)
	 ((and (not (eq? *inline-mode* 'all))
	       (eq? (sfun-class sfun) 'sifun)
	       (is-recursive? var))
	  (when (getenv "INLINE")
	     (tprint "inline-app? .4"))
	  (trace (inline inline+ 0) " no (recursive)" #\Newline)
	  ;; recursive functions can be inlined only on the first inlining
	  #f)
	 ((eq? (sfun-class sfun) 'sgfun)
	  (trace inline " no (generic)" #\Newline)
	  (trace inline+ " no (generic)" #\Newline)
	  (when (getenv "INLINE")
	     (tprint "inline-app? .5"))
	  ;; don't inline generic (they use find-method which is inline)
	  #f)
	 ((and (eq? (sfun-class sfun) 'sifun)
	       (not (memq var stack))
	       (or (not (eq? *inline-mode* 'reducer))
		   (not (contains-kwote? (sfun-body sfun))))
	       (or (not (eq? *inline-mode* 'predicate))
		   (fun-predicate-of sfun)))
	  (when (getenv "INLINE")
	     (tprint "inline-app? .6"))
	  ;; yes, because the function has been declared inline
	  (trace (inline inline+ 0) " yes (sifun) mode=" *inline-mode* #\Newline)
	  #t)
	 ((and (global? var) (eq? (global-import var) 'import))
          ;; of course not.
          (trace (inline inline+ 0) " no (import)" #\Newline)
	  (when (getenv "INLINE")
	     (tprint "inline-app? .7"))
          #f)
	 ((not *user-inlining?*)
	  (when (getenv "INLINE")
	     (tprint "inline-app? .8"))
	  (trace (inline inline+ 0) " no, not user-inlining...\n")
	  #f)
	 ((<fx (node-size body) (*fx kfactor call-size))
          ;; yes, because the size does not grew
	  (when (getenv "INLINE")
	     (tprint "inline-app? .9"))
	  (trace (inline inline+ 0) " yes, small enough (size: "
 		 (node-size body)
		 " max: " (*fx kfactor call-size)
		 ")"
		 #\newline)
          #t)
	 ((and (=fx (node-size body) call-size) (not (memq var stack)))
	  ;; yes, because the call and the body are of the same size
	  ;; and we are not inlining an infinite loop
	  (when (getenv "INLINE")
	     (tprint "inline-app? .10"))
	  (trace (inline inline+ 0) "yes, same size and not in stack (size: "
 		 (node-size body)
		 ")"
		 #\newline)
	  #t)
	 ((and (=fx (variable-occurrence var) 1)
	       (or (not (global? var)) (eq? (global-import var) 'static))
	       (isa? (sfun-body (variable-value var)) retblock))
	  (when (getenv "INLINE")
	     (tprint "inline-app? .11"))
	  (trace (inline inline+ 0) "yes, because static 1 occ retblock"
		 ")"
		 #\newline)
	  #t)
	 (else
	  (when (getenv "INLINE")
	     (tprint "inline-app? .12"))
          ;; no, because the function is too large
          (trace (inline inline+ 0) " no, too large (size: "
		 (node-size body)
		 " max: " (*fx kfactor call-size)
		 ")"
		 #\newline)
          #f))))
 
