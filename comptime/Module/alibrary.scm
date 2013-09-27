;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/alibrary.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 28 10:20:55 1998                          */
;*    Last change :  Thu Sep 19 11:59:07 2013 (serrano)                */
;*    Copyright   :  1998-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compiler library clause compilation                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_alibrary
   (include "Ast/unit.sch")
   (include "Module/libinfo.sch")
   (import  module_module
	    tools_error
	    tools_speek
	    engine_param
	    backend_backend
	    ast_var
	    type_type
	    ast_glo-decl
	    ast_ident
	    ast_env)
   (export  (make-alibrary-compiler)
            (load-library-init)
	    (use-library! library::symbol)
	    (get-alibrary-inits))
   (eval    (export use-library!)))

;*---------------------------------------------------------------------*/
;*    *library-init* ...                                               */
;*    -------------------------------------------------------------    */
;*    The list of init file that will have to be loaded for libraries. */
;*---------------------------------------------------------------------*/
(define *library-init* '())

;*---------------------------------------------------------------------*/
;*    load-library-init ...                                            */
;*    -------------------------------------------------------------    */
;*    loading an init file may demand other libraries to be loaded.    */
;*    This function iterates until the fix point is reached.           */
;*---------------------------------------------------------------------*/
(define (load-library-init)
   (let loop ()
      (when (pair? *library-init*)
	 (let ((l *library-init*))
	    (set! *library-init* '())
	    (for-each (lambda (fname)
			 (verbose 2 "      [reading " fname "]" #\Newline)
			 (loadq fname))
	       l))
	 (loop))))

;*---------------------------------------------------------------------*/
;*    register-library-init! ...                                       */
;*---------------------------------------------------------------------*/
(define (register-library-init! library)
   (let* ((init-name (string-append (symbol->string library) ".init"))
	  (fname (find-file/path init-name *lib-dir*)))
      (when fname
	 (set! *library-init* (cons fname *library-init*)))))

;*---------------------------------------------------------------------*/
;*    use-library! ...                                                 */
;*---------------------------------------------------------------------*/
(define (use-library! library)
   (unless (member library *additional-bigloo-libraries*)
      (set! *additional-bigloo-libraries*
	 (cons library *additional-bigloo-libraries*))
      (let ((heap-name (symbol->string library)))
	 (register-library-init! library)
	 (set! *additional-heap-names*
	    (cons heap-name *additional-heap-names*))
	 library)))

;*---------------------------------------------------------------------*/
;*    make-alibrary-compiler ...                                       */
;*---------------------------------------------------------------------*/
(define (make-alibrary-compiler)
   (instantiate::ccomp
      (id 'library)
      (producer alibrary-producer)))

;*---------------------------------------------------------------------*/
;*    alibrary-producer ...                                            */
;*---------------------------------------------------------------------*/
(define (alibrary-producer clause)
   (match-case clause
      ((?- . ?protos)
       (for-each (lambda (x)
		    (if (symbol? x)
			(use-library! x)
			(error 'library "Illegal prototype" x)))
		 protos)
       '())
      (else
       (user-error "Parse error" "Illegal `library' clause" clause '()))))
       
;*---------------------------------------------------------------------*/
;*    get-alibrary-inits ...                                           */
;*---------------------------------------------------------------------*/
(define (get-alibrary-inits)
   (filter-map (lambda (lib)
		  (let ((info (library-info lib)))
		     (when info
			`(begin
			    ,(when (and (libinfo-init_s info)
					(backend-pragma-support (the-backend)))
				`(pragma ,(format "~a()"
					     (libinfo-init_s info))))
			    ,(when (libinfo-module_s info)
				(let ((v (find-global 'module-initialization
					    (libinfo-module_s info))))
				   (if (global? v)
				       (let ((f (if (string? (car *src-files*))
						    (car *src-files*)
						    "-")))
					  `(,v 0 ,f))
				       (error 'library "Cannot find library init module"
					  (libinfo-module_s info)))))
			    ,(when (libinfo-init info)
				`(,(libinfo-init info)))))))
      (delete-duplicates *additional-bigloo-libraries*)))
