;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-gc.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 20 10:13:26 2024                          */
;*    Last change :  Thu Jun 20 20:01:21 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    BBV gc                                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-gc
   
   (include "Tools/trace.sch"
            "SawMill/regset.sch"
            "SawMill/bbset.sch")
   
   (import  engine_param
            ast_var
            ast_node
            type_type
            type_cache
            type_typeof
            tools_shape
            tools_speek
            backend_backend
            saw_lib
            saw_defs
            saw_regset
            saw_regutils
            saw_bbv-config
            saw_bbv-types
            saw_bbv-specialize
            saw_bbv-cache
            saw_bbv-range
            saw_bbv-debug)

   (export  (bbv-gc! ::blockV)))

;*---------------------------------------------------------------------*/
;*    bbv-gc! ...                                                      */
;*---------------------------------------------------------------------*/
(define (bbv-gc! bv::blockV)
   (with-trace 'bbv-gc "bbv-gc!"
      (with-access::blockV bv (versions)
	 (let ((m0 (get-gc-mark!))
	       (l (blockV-live-versions bv)))
	    (when *bbv-debug*
	       (let ((m (get-gc-mark!)))
		  (for-each (lambda (b)
			       (walk! b m
				  (lambda (b)
				     (assert-block b "bbv-gc!>"))))
		     l)))
	    ;; reset all counters
	    (walk*! bv m0
	       (lambda (b)
		  (with-access::blockV b (versions)
		     (for-each (lambda (b)
				  (with-access::blockS b (cnt)
				     (set! cnt 0)))
			versions))))
	    ;; mark all reachables blocks
	    (let ((m (get-gc-mark!)))
	       (for-each (lambda (b)
			    (walk! b m
			       (lambda (b)
				  (with-access::blockS b (cnt gcmark)
				     (set! gcmark m0)
				     (set! cnt (+fx cnt 1))))))
		  l))
	    ;; filters out the unreachable preds block
	    (let ((m (get-gc-mark!)))
	       (for-each (lambda (b)
			    (walk! b m
			       (lambda (b)
				  (with-access::blockS b (preds)
				     (set! preds
					(filter (lambda (p)
						   (with-access::blockS p (gcmark)
						      (eq? gcmark m0)))
					   preds))))))
		  l))
	    (when *bbv-debug*
	       (let ((m (get-gc-mark!)))
		  (for-each (lambda (b)
			       (walk! b m
				  (lambda (b)
				     (trace-item (format "#~a[~a]" (block-label b) (blockS-cnt b)))
				     (assert-block b "bbv-gc!<"))))
		     l)))))))

;*---------------------------------------------------------------------*/
;*    *gc-mark* ...                                                    */
;*---------------------------------------------------------------------*/
(define *gc-mark* 0)

;*---------------------------------------------------------------------*/
;*    get-gc-mark! ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-gc-mark!)
   (set! *gc-mark* (+fx 1 *gc-mark*))
   *gc-mark*)

;*---------------------------------------------------------------------*/
;*    walk! ...                                                        */
;*---------------------------------------------------------------------*/
(define (walk! b::blockS m::long proc::procedure)
   (with-access::blockS b (cnt %mark succs)
      (proc b)
      (unless (eq? %mark m)
	 (set! %mark m)
	 (for-each (lambda (s) (walk! s m proc)) succs))))

;*---------------------------------------------------------------------*/
;*    walk*! ...                                                       */
;*---------------------------------------------------------------------*/
(define (walk*! b::blockV m::long proc::procedure)
   (with-access::blockV b (cnt %mark succs)
      (unless (eq? %mark m)
	 (set! %mark m)
	 (proc b)
	 (for-each (lambda (s) (walk*! s m proc)) succs))))
	 
