;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Write/expanded.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:03:54 1994                          */
;*    Last change :  Tue May 13 09:53:25 2003 (serrano)                */
;*    Copyright   :  1994-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The pretty-print of expanded module.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module write_expanded
   (include "Ast/unit.sch")
   (import  engine_param
	    write_scheme
	    tools_args
	    ast_unit
	    module_module)
   (export  (write-expanded expanded)))

;*---------------------------------------------------------------------*/
;*    write-expanded ...                                               */
;*---------------------------------------------------------------------*/
(define (write-expanded units)
   (let* ((output-name (cond
			  ((string? *dest*)
			   *dest*)
			  ((eq? *dest* '--to-stdout)
			   #f)
			  ((and (pair? *src-files*)
				(string? (car *src-files*)))
			   (string-append (prefix (car *src-files*))
					  ".escm"))
			  (else
			   #f)))
	  (port        (if (string? output-name)
			   (open-output-file output-name)
			   (current-output-port))))
      (if (not (output-port? port))
	  (error "write-expanded" "Can't open output file" output-name)
	  (unwind-protect
	     (begin
		(write-scheme-file-header port "The expanded module")
		(write-scheme-comment
		 port
		 "---------------------------------------------------------")
		(write-scheme-comment
		 port
		 "!!! WARNING !!!      !!! WARNING !!!      !!! WARNING !!!")
		(write-scheme-comment
		 port
		 "---------------------------------------------------------")
		(write-scheme-comment
		 port
		 "This expanded file cannot be compiled \"as is\". In order to")
		(write-scheme-comment
		 port
		 "compile it:")
		(write-scheme-comment
		 port
		 "   - the explicit call to the MODULE-INITIALIZATION ")
		(write-scheme-comment
		 port
		 "     must be removed.")
		(write-scheme-comment
		 port
		 "   - If the source module was INCLUDING files,")
		(write-scheme-comment
		 port
		 "     you must select manually which files still have to")
		(write-scheme-comment
		 port
		 "     be included in the expanded forms.")
		(write-scheme-comment
		 port
		 "---------------------------------------------------------")
		(write-scheme-comment port "The module clause")
		(pp *module-clause* port)
		(newline port)
		(for-each
		 (lambda (u)
		    (if (unit-printable? u)
			(begin
			   (if (pair? (unit-sexp* u))
			       (write-scheme-comment port
						     (string-append
						      "unit: "
						      (symbol->string
						       (unit-id u)))))
			   ;; we loop on all units
			   (for-each
			    (lambda (code)
			       ;; we loop on all expression in a unit
			       (match-case code
				  ((define (?name . ?args) ?value)
				   (write-scheme-comment port name)
				   (pp code port))
				  ((define ?name ?value)
				   (write-scheme-comment port name)
				   (pp code port))
				  ((define-inline (?name . ?args) ?value)
				   (write-scheme-comment port name)
				   (pp code port))
				  (else
				   (pp code port))))
			    (let ((code (unit-sexp* u)))
			       (if (procedure? code)
				   (force code)
				   code)))
			   (newline port))))
		 units))
	     (if (and (output-port? port)
		      (not (eq? port (current-output-port))))
		 (close-output-port port))))))
