;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/alibrary.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 28 10:20:55 1998                          */
;*    Last change :  Wed Oct 20 07:50:34 2010 (serrano)                */
;*    Copyright   :  1998-2010 Manuel Serrano, see LICENSE file        */
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
	    engine_param
	    backend_backend
	    (setup-library-values init_setrc)
	    ast_var
	    type_type
	    ast_glo-decl
	    ast_ident
	    ast_env)
   (export  (make-alibrary-compiler)
	    (use-library! library::symbol mode)
	    (get-alibrary-inits)))

;*---------------------------------------------------------------------*/
;*    use-library! ...                                                 */
;*---------------------------------------------------------------------*/
(define (use-library! library mode)
   (if (not (member library *additional-bigloo-libraries*))
       (begin
	  (set! *additional-bigloo-libraries*
		(cons library *additional-bigloo-libraries*))
	  (let ((heap-name (symbol->string library)))
	     ;; when use-library is called from the argument parsing we have
	     ;; to delay the library initialization until all arguments have
	     ;; been parsed. In consequence setup-library-values is called
	     ;; only when use-library is not called from argument parsing
	     ;; (which is denotes by the 'now mode value)
	     (if (eq? mode 'now) (setup-library-values library))
	     (set! *additional-heap-names*
		   (cons heap-name *additional-heap-names*))
	     library))))

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
			(use-library! x 'now)
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
				   (when (global? v)
				      (let ((f (if (string? (car *src-files*))
						   (car *src-files*)
						   "-")))
					 `(,v 0 ,f)))))
			    ,(when (libinfo-init info)
				`(,(libinfo-init info)))))))
	       (delete-duplicates *additional-bigloo-libraries*)))
