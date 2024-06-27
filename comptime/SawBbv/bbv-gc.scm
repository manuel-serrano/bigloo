;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-gc.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 20 10:13:26 2024                          */
;*    Last change :  Thu Jun 27 11:49:53 2024 (serrano)                */
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

   (export  (bbv-gc! ::blockV)
	    (bbv-gc-init! ::blockS)
	    (bbv-gc-block-reachable? ::blockS)
	    (bbv-gc-connect! ::blockS ::blockS)
	    (bbv-gc-redirect! ::blockS ::blockS)))

;*---------------------------------------------------------------------*/
;*    bbv-gc! ...                                                      */
;*---------------------------------------------------------------------*/
(define (bbv-gc! bv::blockV)
   (with-trace 'bbv-gc "bbv-gc!"
      (trace-item "bv= #" (blockV-label bv))
      (when (eq? *bbv-blocks-gc* 'cnt)
	 (with-access::blockV bv (versions)
	    (let ((m0 (get-gc-mark!))
		  (l (blockV-live-versions bv)))
	       ;; debug
	       (when *bbv-debug*
		  (let ((m (get-gc-mark!))
			(d '()))
		     (for-each (lambda (b)
				  (walkbs! b m
				     (lambda (b)
					(set! d (cons (format "#~a[~a]" (block-label b) (blockS-cnt b)) d))
					
					(assert-block b "bbv-gc!>"))))
			l)
		     (trace-item ">>> " (length l) "/" (length d) " " d)
		     (trace-item "### " (map (lambda (b)
						(format "#~a[~a]" (block-label b) (blockS-cnt b)))
					   versions))))
	       ;; reset all counters
	       (walkbv! bv (get-gc-mark!)
		  (lambda (b)
		     (with-access::blockV b (versions)
			(for-each (lambda (b)
				     (with-access::blockS b (cnt)
					(set! cnt 0)))
			   versions))))
	       ;; mark the root block (that cannot be collected)
	       (for-each (lambda (b)
			    (with-access::blockS b (creator cnt label)
			       (when (eq? creator 'root)
				  (set! cnt 1))))
		  l)
	       ;; mark all reachables blocks
	       (let ((m (get-gc-mark!)))
		  (for-each (lambda (b)
			       (walkbs! b m
				  (lambda (b)
				     (with-access::blockS b (cnt gcmark)
					(set! gcmark m0)
					(set! cnt (+fx cnt 1))))))
		     l))
	       ;; filters out the unreachable preds block
	       (let ((m (get-gc-mark!)))
		  (for-each (lambda (b)
			       (walkbs! b m
				  (lambda (b)
				     (with-access::blockS b (preds)
					(set! preds
					   (filter (lambda (p)
						      (with-access::blockS p (gcmark)
							 (eq? gcmark m0)))
					      preds))))))
		     l))
	       ;; cleanup unreachable blocks
	       (walkbv! bv (get-gc-mark!)
		  (lambda (b)
		     (with-access::blockV b (versions)
			(for-each (lambda (b)
				     (with-access::blockS b (cnt succs preds asleep)
					(when (=fx cnt 0)
					   (set! asleep #t)
					   (set! succs '())
					   (set! preds '()))))
			   versions))))
	       ;; debug
	       (when *bbv-debug*
		  (let ((m (get-gc-mark!))
			(d '()))
		     (for-each (lambda (b)
				  (walkbs! b m
				     (lambda (b)
					(set! d (cons (format "#~a[~a]" (block-label b) (blockS-cnt b)) d))
					(unless (eq? (>fx (blockS-cnt b) 0) (bbv-gc-block-reachable? b))
					   (with-access::blockS b (preds succs label cnt)
					      (tprint "bs=" label " cnt=" cnt)
					      (tprint "preds=" (map block-label preds))
					      (tprint "succs=" (map block-label succs))
					      (error "block-live?" "miss match for node" label)))
					(assert-block b "bbv-gc!<"))))
			l)
		     (trace-item "<<< " (length d) " " d))))))))

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
;*    walkbs! ...                                                      */
;*---------------------------------------------------------------------*/
(define (walkbs! b::blockS m::long proc::procedure)
   (with-access::blockS b (cnt %mark succs)
      (proc b)
      (unless (eq? %mark m)
	 (set! %mark m)
	 (for-each (lambda (s) (walkbs! s m proc)) succs))))

;*---------------------------------------------------------------------*/
;*    walkbv! ...                                                      */
;*---------------------------------------------------------------------*/
(define (walkbv! b::blockV m::long proc::procedure)
   (with-access::blockV b (cnt %mark succs)
      (unless (eq? %mark m)
	 (set! %mark m)
	 (proc b)
	 (for-each (lambda (s) (walkbv! s m proc)) succs))))

;*---------------------------------------------------------------------*/
;*    *gc-graph* ...                                                   */
;*---------------------------------------------------------------------*/
(define *gc-graph* #f)

;*---------------------------------------------------------------------*/
;*    bbv-gc-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-gc-init! root::blockS)
   (set! *gc-graph* (ssr-make-graph :source (block-label root))))

;*---------------------------------------------------------------------*/
;*    bbv-gc-block-reachable? ...                                      */
;*---------------------------------------------------------------------*/
(define (bbv-gc-block-reachable? b::blockS)
   (ssr-connected? *gc-graph* (block-label b)))

;*---------------------------------------------------------------------*/
;*    bbv-gc-connect! ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-gc-connect! from::blockS to::blockS)
   (with-trace 'bbv-gc "bbv-gc-connect!"
      (trace-item "from #" (block-label from) "[" (blockS-cnt from) "]")
      (trace-item "to #" (block-label to) "[" (blockS-cnt to) "]"
	 " "
	 (map (lambda (b) (format "#~a" (block-label b))) (blockS-preds to)))
      (ssr-add-edge! *gc-graph* (block-label from) (block-label to))))

;*---------------------------------------------------------------------*/
;*    bbv-gc-redirect! ...                                             */
;*---------------------------------------------------------------------*/
(define (bbv-gc-redirect! old::blockS new::blockS)
   (with-trace 'bbv-gc "bbv-gc-redirect"
      (trace-item "old #" (block-label old) "[" (blockS-cnt old) "]")
      (trace-item "new #" (block-label new) "[" (blockS-cnt new) "]"
	 " "
	 (map (lambda (b) (format "#~a" (block-label b))) (blockS-preds new)))
      (ssr-redirect! *gc-graph* (block-label old) (block-label new))
      (with-access::blockS new (preds cnt)
	 (set! preds (filter bbv-gc-block-reachable? preds))
	 (set! cnt (length preds)))))
