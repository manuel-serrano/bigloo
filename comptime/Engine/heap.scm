;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/heap.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Aug 14 09:36:34 2007                          */
;*    Last change :  Wed Jul 26 10:18:31 2017 (serrano)                */
;*    Copyright   :  2007-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dump heaps for debugging                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_heap
   (import engine_param
	   backend_backend
	   ast_env
	   ast_var
	   type_type
	   object_class
	   type_env
	   read_jvm
	   tools_shape
	   heap_restore
	   module_module
	   module_pragma)
   (export (dump-heaps names)))

;*---------------------------------------------------------------------*/
;*    dump-heaps ...                                                   */
;*---------------------------------------------------------------------*/
(define (dump-heaps names)
   ;; we build the ad-hoc backend
   (set-backend! *target-language*)
   ;; initialize the environment otherwise we won't be able to read heap files
   (initialize-Genv!)
   (initialize-Tenv!)
   ;; we dump all the heap specified on the command line
   (for-each dump-heap names))

;*---------------------------------------------------------------------*/
;*    read-heap ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-heap heap)
   (let ((fname (if (file-exists? heap)
		    heap
		    (find-file/path heap *lib-dir*))))
      (if (string? fname)
	  (let ((port (open-input-binary-file fname)))
	     (if (not (binary-port? port))
		 (error "dump-heap"
		    (format "Cannot open heap file ~s" fname)
		    *lib-dir*)
		 (unwind-protect
		    (let* ((Envs (input-obj port))
			   (_ (if (not (and (vector Envs)
					    (or (=fx (vector-length Envs) 5)
						(=fx (vector-length Envs) 7))))
				  (error "dump-heap" "Corrupted heap" heap)))
			   (target (vector-ref Envs 0))
			   (version (vector-ref Envs 1))
			   (specific (vector-ref Envs 2))
			   (Genv (vector-ref Envs 3))
			   (Tenv (vector-ref Envs 4))
			   (includes (if (=fx (vector-length Envs) 6)
					 (vector-ref Envs 5)
					 '())))
		       ;; @label heap class handling@
		       ;; The function add-Tenv! manages the import
		       ;; of class definitions. That is, if the additional
		       ;; heap contains class definition, add-Tenv! will
		       ;; create the accessors for that classes. Note
		       ;; that set-Tenv! *doesn't* do the same job, it
		       ;; supposes that the env doesn't contain classes
		       (assert (Tenv) (hashtable? Tenv))
		       (assert (Genv) (hashtable? Genv))
		       (if (=fx (vector-length Envs) 6)
			   (begin
			      ;(add-tenv! Tenv)
			      (add-genv! Genv))
			   (begin
			      (set-tenv! Tenv)
			      (set-genv! Genv)))
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
					(heap-module-list (global-module new)))
				     (cdr bucket))))
		       (with-output-to-port (current-error-port)
			  (lambda ()
			     (print "(heap \"" heap "\"")
			     (print " (variables")
			     (dump-Genv Genv)
			     (print " )\n")
			     (print " (types")
			     (dump-Tenv Tenv)
			     (print " )")
			     (when (pair? includes)
				(print " (includes")
				(print includes)
				(print " )"))
			     (print ")\n"))))
		    (close-binary-port port))))
	  (let ((m (format "Cannot open heap file ~s" heap)))
	     (error "dump-heap" m *lib-dir*)
	     #f))))

;*---------------------------------------------------------------------*/
;*    dump-Genv ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-Genv Genv)
   (define (dump-var new)
      (let* ((module (global-module new))
	     (id (global-id new))
	     (qt (module->qualified-type module))
	     (jt (global-jvm-type-name new))
	     (val (global-value new)))
	 (cond
	    ((sfun? val)
	     (print "   " `(function
			      ,(shape new)
			      "\n    "
			      (id ,id)
			      "\n    "
			      (module ,module)
			      "\n    "
			      (name ,(format "~s" (global-name new)))
			      "\n    "
			      (qualified-type ,qt)
			      ,(if (eq? (sfun-class val) 'sifun)
				   `(inline ,(shape (sfun-body val)))
				   "")
			      "\n    "
			      (jvm-type-name ,jt) "\n   "
			      (args ,(map shape (sfun-args val))))))
	    ((cfun? val)
	     (print "   " `(native
			      ,(shape new)
			      "\n    "
			      (id ,id)
			      "\n    "
			      (module ,module)
			      "\n    "
			      (name ,(format "~s" (global-name new)))
			      "\n    "
			      (qualified-type ,qt)
			      "\n    "
			      (jvm-type-name ,jt) "\n   "
			      (args ,(map shape (cfun-args-type val))))))
	    (else
	     (unless (eq? (type-id (global-type new)) 'class)
		(print "   " `(variable
				 ,(shape new)
				 "\n    "
				 (id ,id)
				 "\n    "
				 (module ,module)
				 "\n    "
				 (name ,(format "~s" (global-name new)))
				 "\n    "
				 (qualified-type ,qt)
				 "\n    "
				 (jvm-type-name ,jt))))))))
   (hashtable-for-each
      Genv
      (lambda (k bucket) (for-each dump-var (cdr bucket)))))

;*---------------------------------------------------------------------*/
;*    dump-Tenv ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-Tenv Tenv)
   (hashtable-for-each Tenv
		       (lambda (k new)
			  (let* ((id  (type-id new))
				 (name (type-name new)))
			     (if (tclass? new)
				 (with-access::tclass new (its-super slots)
				    (print "   "
					   `(class ,(shape new)
					       (super ,(shape its-super))
					       ,@(map shape slots))))
				 (print "   " `(type ,id (name ,name))))))))

