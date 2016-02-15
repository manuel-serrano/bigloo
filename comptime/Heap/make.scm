;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Heap/make.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  8 08:44:08 1995                          */
;*    Last change :  Sun Feb 14 07:00:01 2016 (serrano)                */
;*    Copyright   :  1995-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The creation of a library heap                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module heap_make
   (include "Engine/pass.sch"
	    "Tools/trace.sch")
   (import  engine_param
	    tools_error
	    type_type
	    type_env
	    ast_var
	    ast_env
	    foreign_library
	    backend_backend
	    heap_restore
	    module_module)
   (import  tools_shape)
   (export  (make-heap)
	    (make-add-heap)))

;*---------------------------------------------------------------------*/
;*    make-heap ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-heap)
   (pass-prelude "Heap" prepare-globals!)
   (set-obj-string-mode! 'pair)
   (if (not (string? *heap-name*))
       (user-error "make-heap" "Illegal heap's name" *heap-name*)
       (let ((hname (make-file-name (car *lib-dir*) *heap-name*)))
	  (let ((port (open-output-binary-file hname)))
	     (if (not (binary-port? port))
		 (error "make-heap" "Can't open output port" hname)
		 (begin
		    (output-obj port 
				(vector (backend-language (the-backend))
					*bigloo-version*
					*bigloo-specific-version*
					(get-genv)
					(get-tenv)))
		    (close-binary-port port)))))))

;*---------------------------------------------------------------------*/
;*    prepare-globals! ...                                             */
;*    -------------------------------------------------------------    */
;*    Before making a heap, we reset all the occurrence slots and for  */
;*    each exported variable, we declare it as imported. We remove     */
;*    static variables.                                                */
;*---------------------------------------------------------------------*/
(define (prepare-globals!)
   (for-each-global! (lambda (g)
			;; we set importation slots
			(cond
			   ((eq? (global-import g) 'static)
			    (unbind-global! (global-id g) (global-module g)))
			   ((eq? (global-import g) 'export)
			    (global-import-set! g 'import))
			   (else
			    #unspecified))
			;; shrink global to be sure that we do not save extra
			;; information
			(when (wide-object? g) (shrink! g))
			;; and occurrence ones
			(global-occurrence-set! g 0)
			(global-library-set! g *heap-library*)))
   #t)

;*---------------------------------------------------------------------*/
;*    make-add-heap ...                                                */
;*    -------------------------------------------------------------    */
;*    @label foreign accesors@                                         */
;*    @ref ../Foreign/library.scm:foreign accesors@                    */
;*---------------------------------------------------------------------*/
(define (make-add-heap)
   (pass-prelude "Library heap"
		 check-additional-heap-library
		 prepare-additional-globals!)
   (let ((genv (get-genv))
	 (tenv (get-tenv))
	 (lang (backend-language (the-backend))))
      (set-obj-string-mode! 'pair)
      (if (not (string? *additional-heap-name*))
	  (user-error "make-add-heap"
		      "Illegal heap's name"
		      *additional-heap-name*)
	  (let ((hname *additional-heap-name*))
	     (let ((port (open-output-binary-file hname)))
		(if (not (binary-port? port))
		    (error "make-addd-heap" "Can't open output port" hname)
		    (begin
		       (output-obj port (vector lang
						*bigloo-version*
						*bigloo-specific-version*
						genv
						tenv
						*additional-include-foreign*
						*cc-options*))
		       (close-binary-port port))))))))

;*---------------------------------------------------------------------*/
;*    check-additional-heap-library ...                                */
;*---------------------------------------------------------------------*/
(define (check-additional-heap-library)
   (cond
      ((eq? *heap-library* 'bigloo)
       (user-error 'make-add-heap
		   "Illegal reserved identifier for additional heap library (see `-heap-library' compiler option)"
		   *heap-library*))
      ((not (symbol? *heap-library*))
       (user-error 'make-add-heap
		   "Missing additional heap library identifier (see `-heap-library' compiler option)"
		   *heap-library*))
      (else
       #t)))

;*---------------------------------------------------------------------*/
;*    prepare-additional-globals! ...                                  */
;*    -------------------------------------------------------------    */
;*    Before making an additional heap, we reset all the occurrence    */
;*    slots and for each exported variable, we declare it as imported. */
;*    We remove static variables and library variables.                */
;*---------------------------------------------------------------------*/
(define (prepare-additional-globals!)
   (for-each-global! (lambda (g)
			;; we set importation slots
			(cond
			   ((or (eq? (global-import g) 'static)
				(global-library g))
			    (unbind-global! (global-id g) (global-module g)))
			   ((eq? (global-import g) 'export)
			    (global-import-set! g 'import))
			   (else
			    #unspecified))
			;; shrink global to be sure that we do not save extra
			;; information
			(when (wide-object? g) (shrink! g))
			;; and occurrence ones
			(global-occurrence-set! g 0)
			(global-library-set! g *heap-library*)))
   #t)
