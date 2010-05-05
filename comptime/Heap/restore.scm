;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Heap/restore.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 26 10:53:23 1994                          */
;*    Last change :  Thu Apr 15 14:52:18 2010 (serrano)                */
;*    Copyright   :  1994-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We restore a heap                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module heap_restore
   (include "Engine/pass.sch")
   (export  (restore-heap)
	    (restore-additional-heaps)
	    (restore-additional-heap ::bstring)
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
;*    backend-heap-compatible? ...                                     */
;*---------------------------------------------------------------------*/
(define (backend-heap-compatible? target)
   (with-access::backend (the-backend) (language heap-compatible)
      (or (eq? target language) (eq? target heap-compatible))))
   
;*---------------------------------------------------------------------*/
;*    restore-heap ...                                                 */
;*---------------------------------------------------------------------*/
(define (restore-heap)
   (when (string? *heap-name*)
      (pass-prelude "Heap")
      (let ((fname (find-file/path *heap-name* *lib-dir*)))
	 (if (string? fname)
	     (let ((port (open-input-binary-file fname)))
		(if (not (binary-port? port))
		    (let ((m (format "Cannot open heap file ~s" fname)))
		       (error 'restore-heap m *lib-dir*)
		       (compiler-exit 5))
		    (begin
		       (verbose 2 "      [reading " fname "]" #\Newline)
		       (unwind-protect
			  (let* ((Envs (input-obj port))
				 (_ (if (not (and (vector? Envs)
						  (=fx (vector-length Envs) 5)))
					(error *heap-name*
					       "Corrupted heap"
					       Envs)))
				 (target (vector-ref Envs 0))
				 (version (vector-ref Envs 1))
				 (specific (vector-ref Envs 2))
				 (Genv (vector-ref Envs 3))
				 (Tenv (vector-ref Envs 4)))
			     ;; check the target languages
			     (unless (backend-heap-compatible? target)
				(error *heap-name*
				       "Target language mismatch"
				       (format "~a vs. ~a"
					       target
					       (backend-language (the-backend)))))
			     (unless (or *unsafe-heap*
					 (equal? version *bigloo-version*))
				(error *heap-name*
				       "Release mismatch"
				       (format "Heap is `~a', Bigloo is `~a'"
					       version
					       *bigloo-version*)))
			     (unless (or *unsafe-heap*
					 (equal? specific *bigloo-specific-version*))
				(error *heap-name*
				       "Specific version mismatch"
				       (format "Heap is `~a', Bigloo is `~a'"
					       specific
					       *bigloo-specific-version*)))
			     (set-genv! Genv)
			     ;; for class handling see the note set
			     ;; for add-Tenv!:
			     ;; @ref restore.scm:heap class handling@
			     (set-tenv! Tenv)
			     (unless *call/cc?* (unbind-call/cc!))
			     ;; in jvm mode, we have to propagate
			     ;; the package/module association
			     (when (backend-qualified-types (the-backend))
				(for-each-global!
				 (lambda (new)
				    (add-qualified-type!
				     (global-module new)
				     (global-jvm-type-name new)
				     (shape new))))
				Genv)
			     ;; we add all the heap modules
			     (for-each-global!
			      (lambda (new)
				 (heap-module-list (global-module new))))
			     #t)
			  (close-binary-port port)))))
	     (let ((m (format "Cannot open heap file ~s" *heap-name*)))
		(error 'restore-heap m *lib-dir*)
		(compiler-exit 5))))))

;*---------------------------------------------------------------------*/
;*    unbind-call/cc! ...                                              */
;*---------------------------------------------------------------------*/
(define (unbind-call/cc!)
   (if (find-global/module 'call/cc '__r4_control_features_6_9)
       (unbind-global! 'call/cc '__r4_control_features_6_9))
   (if (find-global/module 'call-with-current-continuation
			   '__r4_control_features_6_9)
       (unbind-global! 'call-with-current-continuation
		       '__r4_control_features_6_9)))

;*---------------------------------------------------------------------*/
;*    restore-additional-heaps ...                                     */
;*---------------------------------------------------------------------*/
(define (restore-additional-heaps)
   (if (pair? *additional-heap-names*)
       (let ((suf (string-append "." (backend-heap-suffix (the-backend)))))
	  (pass-prelude "Library")
	  (for-each (lambda (h)
		       (restore-additional-heap (string-append h suf)))
		    (reverse *additional-heap-names*)))))

;*---------------------------------------------------------------------*/
;*    restore-additional-heap ...                                      */
;*---------------------------------------------------------------------*/
(define (restore-additional-heap heap)
   (let ((fname (find-file/path heap *lib-dir*)))
      (if (string? fname)
	  (let ((port (open-input-binary-file fname)))
	     (if (not (binary-port? port))
		 (let ((m (format "Cannot open heap file ~s" fname)))
		    (error "restore-additional-heap" m *lib-dir*)
		    (compiler-exit 6))
		 (begin
		    (verbose 2 "      [reading " fname "]" #\Newline)
		    (unwind-protect
		       (let* ((Envs (input-obj port))
			      (_ (if (not (and (vector Envs)
					       (=fx (vector-length Envs) 6)))
				     (error heap "Corrupted heap" Envs)))
			      (target (vector-ref Envs 0))
			      (version (vector-ref Envs 1))
			      (specific (vector-ref Envs 2))
			      (Genv (vector-ref Envs 3))
			      (Tenv (vector-ref Envs 4))
			      (includes (vector-ref Envs 5)))
			  ;; check the target languages
			  (unless (backend-heap-compatible? target)
			     (error heap
				    "Target language mismatch"
				    (format "~a vs. ~a"
					       target
					       (backend-language (the-backend)))))
			     (unless (equal? version *bigloo-version*)
				(error *heap-name*
				       "Release mismatch"
				       (format "Heap is `~a', Bigloo is `~a'"
					       version
					       *bigloo-version*)))
			     (unless (equal? specific *bigloo-specific-version*)
				(error *heap-name*
				       "Specific version mismatch"
				       (format "Heap is `~a', Bigloo is `~a'"
					       specific
					       *bigloo-specific-version*)))
			  ;; @label heap class handling@
			  ;; The function add-Tenv! manages the import
			  ;; of class definitions. That is, if the additional
			  ;; heap contains class definition, add-Tenv! will
			  ;; create the accessors for that classes. Note
			  ;; that set-Tenv! *doesn't* do the same job, it
			  ;; supposes that the env doesn't contain classes
			  [assert (Tenv) (hashtable? Tenv)]
			  [assert (Genv) (hashtable? Genv)]
			  (add-tenv! Tenv)
			  (add-genv! Genv)
			  ;; in jvm mode, we have to propagate
			  ;; the package/module association
			  (when (backend-qualified-types (the-backend))
			     (for-each-global!
			      (lambda (new)
				 (add-qualified-type!
				  (global-module new)
				  (global-jvm-type-name new)
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
			  (set! *additional-include-foreign*
				(append *additional-include-foreign*
					includes))
			  #t)
		       (close-binary-port port)))))
	  (let ((m (format "Cannot open heap file ~s" heap)))
	     (error 'restore-additional-heap m *lib-dir*)
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

