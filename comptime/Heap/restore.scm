;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Heap/restore.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 26 10:53:23 1994                          */
;*    Last change :  Wed Oct 22 09:30:08 2025 (serrano)                */
;*    Copyright   :  1994-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We restore a heap                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module heap_restore
   (include "Engine/pass.sch")
   (export  (restore-heap)
	    (restore-additional-heaps ::obj)
	    (restore-additional-heap ::obj ::bstring)
	    (heap-file-name::bstring ::bstring)
	    (heap-module-list . args))
   (import  engine_param
	    engine_engine
	    init_main
	    tools_error
	    ast_env
	    type_type
	    type_env
	    ast_var
	    read_jvm
	    tools_shape
	    backend_backend))

;*---------------------------------------------------------------------*/
;*    correct-heap? ...                                                */
;*---------------------------------------------------------------------*/
(define (correct-heap? heap)
   (and (vector? heap) (=fx (vector-length heap) 5)))

;*---------------------------------------------------------------------*/
;*    compatible-bigloo-version? ...                                   */
;*---------------------------------------------------------------------*/
(define (compatible-bigloo-version? version)
   (or *unsafe-heap* (equal? version *bigloo-version*)))

;*---------------------------------------------------------------------*/
;*    compatible-bigloo-specific-version? ...                          */
;*---------------------------------------------------------------------*/
(define (compatible-bigloo-specific-version? specific)
   (or *unsafe-heap* (equal? specific *bigloo-specific-version*)))
   
;*---------------------------------------------------------------------*/
;*    backend-heap-compatible? ...                                     */
;*---------------------------------------------------------------------*/
(define (backend-heap-compatible? target)
   (with-access::backend (the-backend) (language heap-compatible)
      (or (eq? target language) (eq? target heap-compatible))))

;*---------------------------------------------------------------------*/
;*    *heap-cache* ...                                                 */
;*---------------------------------------------------------------------*/
(define *heap-cache* '())

;*---------------------------------------------------------------------*/
;*    read-heap ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-heap fname)
   (verbose 2 "      [reading " fname "]" #\Newline)
   (let ((port (open-input-binary-file fname)))
      (if (not (binary-port? port))
	  (let ((m (format "Cannot open heap file ~s" fname)))
	     (error "restore-heap" m *lib-dir*)
	     (compiler-exit 5))
	  (unwind-protect
	     (let ((heap (input-obj port)))
		(if (not (correct-heap? heap))
		    (error *heap-name* "Corrupted heap" heap)
		    (let* ((target (vector-ref heap 0))
			   (version (vector-ref heap 1))
			   (specific (vector-ref heap 2))
			   (genv (vector-ref heap 3))
			   (tenv (vector-ref heap 4)))
		       ;; heap correctness
		       (unless (backend-heap-compatible? target)
			  (error *heap-name*
			     "Target language mismatch"
			     (format "~a vs. ~a"
				target
				(backend-language (the-backend)))))
		       (unless (compatible-bigloo-version? version)
			  (error *heap-name*
			     "Release mismatch"
			     (format "Heap is `~a', Bigloo is `~a'"
				version
				*bigloo-version*)))
		       (unless (compatible-bigloo-specific-version? specific)
			  (error *heap-name*
			     "Specific version mismatch"
			     (format "Heap is `~a', Bigloo is `~a'"
				specific
				*bigloo-specific-version*)))
		       (unless *call/cc?* (unbind-call/cc! genv))
		       ;; in jvm mode, we have to propagate
		       ;; the package/module association
		       (when (backend-qualified-types (the-backend))
			  (for-each-global! genv
			     (lambda (new)
				(add-qualified-type!
				   (global-module new)
				   (global-qualified-type-name new)
				   (shape new))))
			  genv)
		       ;; add all the heap modules
		       (for-each-global! genv
			  (lambda (new)
			     (heap-module-list (global-module new))))
		       (values genv tenv))))
	     (close-binary-port port)))))

;*---------------------------------------------------------------------*/
;*    read-cache-heap ...                                              */
;*---------------------------------------------------------------------*/
(define (read-cache-heap fname::bstring)
   (let ((cache (assoc fname *heap-cache*)))
      (if (pair? cache)
	  (values (cadr cache) (cddr cache))
	  (multiple-value-bind (genv tenv)
	     (read-heap fname)
	     (set! *heap-cache*
		(cons (cons fname (cons genv tenv)) *heap-cache*))
	     (values genv tenv)))))

;*---------------------------------------------------------------------*/
;*    restore-heap ...                                                 */
;*---------------------------------------------------------------------*/
(define (restore-heap)
   (when (string? *heap-name*)
      (pass-prelude "Heap")
      (let ((fname (find-file/path *heap-name* *lib-dir*)))
	 (if (string? fname)
	     (multiple-value-bind (genv tenv)
		(read-cache-heap fname)
		(let ((ge (make-hashtable))
		      (te (make-hashtable)))
		   (hashtable-for-each genv
		      (lambda (k e) (hashtable-put! ge k e)))
		   (hashtable-for-each tenv
		      (lambda (k e) (hashtable-put! te k e)))
		   (values ge te)))
	     (let ((m (format "Cannot open heap file ~s" *heap-name*)))
		(error "restore-heap" m *lib-dir*)
		(compiler-exit 5))))))

;*---------------------------------------------------------------------*/
;*    unbind-call/cc! ...                                              */
;*---------------------------------------------------------------------*/
(define (unbind-call/cc! env)
   (if (find-global/module env 'call/cc '__r4_control_features_6_9)
       (unbind-global! env 'call/cc '__r4_control_features_6_9))
   (if (find-global/module env 'call-with-current-continuation
	  '__r4_control_features_6_9)
       (unbind-global! env 'call-with-current-continuation
	  '__r4_control_features_6_9)))

;*---------------------------------------------------------------------*/
;*    heap-file-name ...                                               */
;*---------------------------------------------------------------------*/
(define (heap-file-name heap)
   (string-append heap "." (backend-heap-suffix (the-backend))))

;*---------------------------------------------------------------------*/
;*    restore-additional-heaps ...                                     */
;*---------------------------------------------------------------------*/
(define (restore-additional-heaps env)
   (when (pair? *additional-heap-names*)
      (pass-prelude "Library")
      (for-each (lambda (h)
		   (unless (member h *restored-heap-names*)
		      (set! *restored-heap-names*
			 (cons h *restored-heap-names*))
		      (restore-additional-heap env (heap-file-name h))))
	 (reverse *additional-heap-names*))))

;*---------------------------------------------------------------------*/
;*    *restored-heap-names* ...                                        */
;*---------------------------------------------------------------------*/
(define *restored-heap-names* '())

;*---------------------------------------------------------------------*/
;*    restore-additional-heap ...                                      */
;*---------------------------------------------------------------------*/
(define (restore-additional-heap env heap)
   (let ((fname (find-file/path heap *lib-dir*)))
      (if (string? fname)
	  (let ((port (open-input-binary-file fname)))
	     (if (not (binary-port? port))
		 (let ((m (format "Cannot open heap file ~s" fname)))
		    (error (if (pair? *src-files*)
			       (car *src-files*)
			       "-")
		       m *lib-dir*)
		    (compiler-exit 6))
		 (begin
		    (verbose 2 "      [reading " fname "]" #\Newline)
		    (unwind-protect
		       (let* ((Envs (input-obj port))
			      (_ (unless (and (vector? Envs)
					      (=fx (vector-length Envs) 7))
				    (error heap "Corrupted heap" Envs)))
			      (target (vector-ref Envs 0))
			      (version (vector-ref Envs 1))
			      (specific (vector-ref Envs 2))
			      (Genv (vector-ref Envs 3))
			      (Tenv (vector-ref Envs 4))
			      (includes (vector-ref Envs 5))
			      (ccopts (vector-ref Envs 6)))
			  ;; check the target languages
			  (unless (backend-heap-compatible? target)
			     (error heap
				"Target language mismatch"
				(format "~a vs. ~a"
				   target
				   (backend-language (the-backend)))))
			  (unless (compatible-bigloo-version? version)
			     (error *heap-name*
				"Release mismatch"
				(format "Heap is `~a', Bigloo is `~a'"
				   version
				   *bigloo-version*)))
			  (unless (compatible-bigloo-specific-version? specific)
			     (error *heap-name*
				"Specific version mismatch"
				(format "Heap is `~a', Bigloo is `~a'"
				   specific
				   *bigloo-specific-version*)))
			  ;; @label heap class handling@
			  ;; The function add-Tenv! manages the import
			  ;; of class definitions. That is, if the additional
			  ;; heap contains class definitions, add-Tenv!
			  ;; creates the accessors for that classes. Note
			  ;; that set-Tenv! *doesn't* do the same job, it
			  ;; supposes that the env doesn't contain classes
			  (assert (Tenv) (hashtable? Tenv))
			  (assert (Genv) (hashtable? Genv))
			  (add-tenv! Tenv)
			  (add-genv! Genv)
			  ;; in jvm mode, we have to propagate
			  ;; the package/module association
			  (when (backend-qualified-types (the-backend))
			     (for-each-global! env
				(lambda (new)
				   (add-qualified-type!
				      (global-module new)
				      (global-qualified-type-name new)
				      (shape new)))))
			  ;; we add all the heap modules
			  (hashtable-for-each
			     Genv
			     (lambda (k bucket)
				(for-each (lambda (new)
					     (heap-module-list
						(global-module new)))
				   (cdr bucket))))
			  ;; we store the list of includes
			  (unless (eq? *pass* 'make-heap)
			     (set! *additional-include-foreign*
				(append *additional-include-foreign*
				   includes))
			     (set! *cc-options*
				(delete-duplicates
				   (append *cc-options* ccopts))))
			  #t)
		       (close-binary-port port)))))
	  (let ((m (format "Cannot open heap file ~s" heap)))
	     (error (if (pair? *src-files*) (car *src-files*) "-") m *lib-dir*)
	     (compiler-exit 6)))))

;*---------------------------------------------------------------------*/
;*    *heap-module-list* ...                                           */
;*    -------------------------------------------------------------    */
;*    The list of modules imported in a heap (i.e. a Bigloo            */
;*    library).					                       */
;*---------------------------------------------------------------------*/
(define *heap-module-list* '())

;*---------------------------------------------------------------------*/
;*    *heap-mark* ...                                                  */
;*---------------------------------------------------------------------*/
(define *heap-mark* 'heap-mark)

;*---------------------------------------------------------------------*/
;*    heap-module-list ...                                             */
;*---------------------------------------------------------------------*/
(define (heap-module-list . args)
   (cond
      ((null? args)
       *heap-module-list*)
      ((not (getprop (car args) *heap-mark*))
       (putprop! (car args) *heap-mark* #t)
       (set! *heap-module-list* (cons (car args) *heap-module-list*)))))

