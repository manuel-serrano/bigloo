;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Write/unit.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:03:54 1994                          */
;*    Last change :  Sun Sep 21 12:45:28 2025 (serrano)                */
;*    Copyright   :  1994-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The pretty-print of expanded module.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module write_unit
   (include "Ast/unit.sch")
   (import  engine_param
	    write_scheme
	    tools_args
	    ast_unit
	    module_module)
   (export  (write-unit unit)))

;*---------------------------------------------------------------------*/
;*    write-unit ...                                                   */
;*---------------------------------------------------------------------*/
(define (write-unit units)
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
	  (error "write-unit" "Can't open output file" output-name)
	  (unwind-protect
	     (begin
		(write-scheme-file-header port *module*)
		(pp *module-clause* port)
		(for-each
		 (lambda (u)
		    (if (unit-printable? u)
			(begin
			   (when (pair? (unit-sexp* u))
			      (write-scheme-comment port
				 (format "unit: ~a" (unit-id u))))
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
