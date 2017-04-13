;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Inline/walk.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan  9 19:15:23 1995                          */
;*    Last change :  Thu Apr 13 10:34:48 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The inlining pass                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch"
	    "Ast/node.sch")
   (import  tools_shape
	    tools_error
	    module_module
	    engine_param
	    ast_remove
	    ast_occur
	    (inline-sfun! inline_inline))
   (export  (inline-walk! <globals> ::symbol)
	    (inline-setup! ::symbol)
	    *kfactor*
	    *inlined-calls*
	    *non-inlined-calls*
	    *inline-mode*))

;*---------------------------------------------------------------------*/
;*    *inline-mode* ...                                                */
;*---------------------------------------------------------------------*/
(define *inline-mode* 'all)

;*---------------------------------------------------------------------*/
;*    inline-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (inline-walk! globals what)
   (assert (what) (memq what '(all reducer predicate)))
   (pass-prelude "Inlining" reset-stat!)
   (trace (inline inline+ 0) "================ INLINING ================ "
      what "\n")
   ;; we setup the inlining
   (inline-setup! what)
   ;; count the number of occurences of all variables
   (when (memq what '(all reducer))
      (occur-var globals))
   ;; we scan all the local definitions to inline their body
   (for-each (lambda (g)
		(let ((kfactor (if (eq? (sfun-class (global-value g)) 'sifun)
				   1
				   *kfactor*)))
		   (enter-function (global-id g))
		   (inline-sfun! g kfactor '())
		   (leave-function)))
	     globals)
   ;; and we just return the reachable variables
   (let loop ((globals globals)
	      (new-globals '()))
      (cond
	 ((null? globals)
	  (pass-postlude
	     (remove-var 'inline (reverse! new-globals)) show-stat!))
	 ((eq? (global-module (car globals)) *module*)
	  (loop (cdr globals) (cons (car globals) new-globals)))
	 (else
	  (loop (cdr globals) new-globals)))))

;*---------------------------------------------------------------------*/
;*    Statistics variables                                             */
;*---------------------------------------------------------------------*/
(define *inlined-calls* 0)
(define *non-inlined-calls* 0)

;*---------------------------------------------------------------------*/
;*    reset-stat! ...                                                  */
;*---------------------------------------------------------------------*/
(define (reset-stat!)
   (set! *inlined-calls* 0)
   (set! *non-inlined-calls* 0))

;*---------------------------------------------------------------------*/
;*    show-stat! ...                                                   */
;*---------------------------------------------------------------------*/
(define (show-stat!)
   (verbose 2 "      inlined calls: " *inlined-calls* #"\n")
   (verbose 2 "      non inlined calls: " *non-inlined-calls* #"\n")
   #t)
   
;*---------------------------------------------------------------------*/
;*    inline-setup! ...                                                */
;*---------------------------------------------------------------------*/
(define (inline-setup! what)
   ;; we set the kfactor
   (case what
      ((all)
       (set! *inline-mode* 'all)
       (set! *kfactor* (max 1 (*inlining-kfactor* *optim*))))
      ((reducer)
       (set! *inline-mode* 'reducer)
       (set! *kfactor* 1))
      ((predicate)
       (set! *inline-mode* 'predicate)
       (set! *kfactor* 1))
      (else
       (internal-error "inline-setup!" "Illegal mode" what))))

;*---------------------------------------------------------------------*/
;*    *kfactor* ...                                                    */
;*---------------------------------------------------------------------*/
(define *kfactor* 1)



