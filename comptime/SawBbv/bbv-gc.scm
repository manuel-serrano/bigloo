;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-gc.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 20 10:13:26 2024                          */
;*    Last change :  Fri Jun 28 07:49:06 2024 (serrano)                */
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
	    (bbv-gc-add-block! b::blockS)
	    (bbv-gc-block-reachable? ::blockS)
	    (bbv-gc-connect! ::blockS ::blockS)
	    (bbv-gc-disconnect! ::blockS ::blockS)
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
		  (with-trace 'bbv-gc-debug "bbv-gc-debug"
		     (let ((m (get-gc-mark!))
			   (d '()))
			(for-each (lambda (b)
				     (walkbs! b m
					(lambda (b)
					   (set! d (cons (format "#~a[~a]"
							    (block-label b)
							    (blockS-gccnt b)) d))
					   (assert-block b "bbv-gc!>"))))
			   l)
			(trace-item ">>> " (length l) "/" (length d) " " d)
			(trace-item "### " (map (lambda (b)
						   (format "#~a[~a]" (block-label b)
						      (blockS-gccnt b)))
					      versions)))))
	       ;; reset all counters
	       (walkbv! bv (get-gc-mark!)
		  (lambda (b)
		     (with-access::blockV b (versions)
			(for-each (lambda (b)
				     (with-access::blockS b (gccnt)
					(set! gccnt 0)))
			   versions))))
	       ;; mark the root block (that cannot be collected)
	       (for-each (lambda (b)
			    (with-access::blockS b (creator gccnt)
			       (when (eq? creator 'root)
				  (set! gccnt 1))))
		  l)
	       ;; mark all reachables blocks
	       (let ((m (get-gc-mark!)))
		  (for-each (lambda (b)
			       (walkbs! b m
				  (lambda (b)
				     (with-access::blockS b (gccnt gcmark)
					(set! gcmark m0)
					(set! gccnt (+fx gccnt 1))))))
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
				     (with-access::blockS b (gccnt succs preds asleep)
					(when (=fx gccnt 0)
					   (set! asleep #t)
					   (set! succs '())
					   (set! preds '()))))
			   versions))))
	       ;; debug
	       (when *bbv-debug*
		  (with-trace 'bbv-gc-debug "bbv-gc-debug"
		     (let ((m (get-gc-mark!))
			   (d '()))
			(for-each (lambda (b)
				     (walkbs! b m
					(lambda (b)
					   (set! d (cons (format "#~a[~a]"
							    (block-label b)
							    (blockS-gccnt b))
						      d))
					   (assert-block b "bbv-gc!<"))))
			   l)
			(trace-item "<<< " (length d) " " d)))))))))

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
   (with-access::blockS b (%mark succs)
      (proc b)
      (unless (eq? %mark m)
	 (set! %mark m)
	 (for-each (lambda (s) (walkbs! s m proc)) succs))))

;*---------------------------------------------------------------------*/
;*    walkbv! ...                                                      */
;*---------------------------------------------------------------------*/
(define (walkbv! b::blockV m::long proc::procedure)
   (with-access::blockV b (%mark succs)
      (unless (eq? %mark m)
	 (set! %mark m)
	 (proc b)
	 (for-each (lambda (s) (walkbv! s m proc)) succs))))

;*---------------------------------------------------------------------*/
;*    *gc-graph* ...                                                   */
;*---------------------------------------------------------------------*/
(define *gc-graph* #f)
(define *gc-blocks* (make-vector 16))
(define *gc-block-len* 0)

;*---------------------------------------------------------------------*/
;*    bbv-gc-init! ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-gc-init! root::blockS)
   (set! *gc-graph* (ssr-make-graph :source (block-label root)))
   (vector-fill! *gc-blocks* #f)
   (set! *gc-block-len* 0))

;*---------------------------------------------------------------------*/
;*    bbv-gc-add-block! ...                                            */
;*---------------------------------------------------------------------*/
(define (bbv-gc-add-block! b::blockS)
   (with-access::blockS b (label)
      (when (>=fx label (vector-length *gc-blocks*))
	 (let ((nv (make-vector (+fx label 1024))))
	    (vector-copy! nv 0 *gc-blocks* 0)
	    (set! *gc-blocks* nv)))
      (vector-set! *gc-blocks* label b)
      (when (>=fx label *gc-block-len*)
	 (set! *gc-block-len* (+fx label 1)))))

;*---------------------------------------------------------------------*/
;*    bbv-gc-block-reachable? ...                                      */
;*---------------------------------------------------------------------*/
(define (bbv-gc-block-reachable? b::blockS)
   (ssr-connected? *gc-graph* (block-label b)))

;*---------------------------------------------------------------------*/
;*    bbv-gc-ondisconnect! ...                                         */
;*---------------------------------------------------------------------*/
(define (bbv-gc-ondisconnect! n::long)
   (with-trace 'bbv-gc-debug "bbv-gc-ondisconnect!"
      (trace-item "n: -#" n)
      (let ((b (vector-ref *gc-blocks* n)))
	 (with-access::blockS b (preds succs)
	    (trace-item "preds: "
	       (map (lambda (b)
		       (format "#~a~a" (block-label b)
			  (if (block-live? b) "+" "-")))
		  (blockS-preds b)))
	    (trace-item "succs: "
	       (map (lambda (b)
		       (format "#~a~a" (block-label b)
			  (if (block-live? b) "+" "-")))
		  (blockS-succs b)))
	    (for-each (lambda (s)
			 (ssr-remove-edge! *gc-graph* n (block-label s))
			 (with-access::blockS s (preds)
			    (set! preds (remq! b preds))))
	       succs)
	    (set! succs '())))))

;*---------------------------------------------------------------------*/
;*    bbv-gc-onconnect! ...                                            */
;*---------------------------------------------------------------------*/
(define (bbv-gc-onconnect! n::long)
   (with-trace 'bbv-gc-debug "bbv-gc-onconnect!"
      (trace-item "n: +#" n)
      (let ((b (vector-ref *gc-blocks* n)))
	 (trace-item "preds: "
	    (map (lambda (b)
		    (format "#~a~a" (block-label b)
		       (if (block-live? b) "+" "-")))
	       (blockS-preds b)))
	 (trace-item "succs: "
	    (map (lambda (b)
		    (format "#~a~a" (block-label b)
		       (if (block-live? b) "+" "-")))
	       (blockS-succs b))))))

;*---------------------------------------------------------------------*/
;*    bbv-gc-connect! ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-gc-connect! from::blockS to::blockS)
   (with-trace 'bbv-gc "bbv-gc-connect!"
      (trace-item "from #" (block-label from))
      (trace-item "to #" (block-label to)
	 " preds: "
	 (map (lambda (b)
		 (format "#~a~a" (block-label b)
		    (if (block-live? b) "+" "-")))
	    (blockS-preds to)))
      (when (eq? *bbv-blocks-gc* 'ssr)
	 (ssr-add-edge! *gc-graph* (block-label from) (block-label to)
	    :onconnect bbv-gc-onconnect!))))

;*---------------------------------------------------------------------*/
;*    bbv-gc-disconnect! ...                                           */
;*---------------------------------------------------------------------*/
(define (bbv-gc-disconnect! from::blockS to::blockS)
   (with-trace 'bbv-gc "bbv-gc-disconnect!"
      (trace-item "from #" (block-label from))
      (trace-item "to #" (block-label to)
	 " preds: "
	 (map (lambda (b)
		 (format "#~a~a" (block-label b)
		    (if (block-live? b) "+" "-")))
	    (blockS-preds to)))
      (when (eq? *bbv-blocks-gc* 'ssr)
	 (ssr-remove-edge! *gc-graph* (block-label from) (block-label to)
	    :ondisconnect bbv-gc-ondisconnect!))))

;*---------------------------------------------------------------------*/
;*    bbv-gc-redirect! ...                                             */
;*---------------------------------------------------------------------*/
(define (bbv-gc-redirect! old::blockS new::blockS)
   (with-trace 'bbv-gc "bbv-gc-redirect"
      (trace-item "old #" (block-label old))
      (trace-item "new #" (block-label new)
	 " preds: "
	 (map (lambda (b)
		 (format "#~a~a" (block-label b)
		    (if (block-live? b) "+" "-")))
	    (blockS-preds new)))
      (when (eq? *bbv-blocks-gc* 'ssr)
	 (ssr-redirect! *gc-graph* (block-label old) (block-label new)
	    :ondisconnect bbv-gc-ondisconnect!
	    :onconnect bbv-gc-onconnect!))))
