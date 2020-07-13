;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Write/ast.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 31 07:29:03 1994                          */
;*    Last change :  Fri Mar 18 11:43:48 2011 (serrano)                */
;*    Copyright   :  1994-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The ast pretty-printer                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module write_ast
   (include "Engine/pass.sch"
	    "Ast/node.sch")
   (import  engine_param
	    init_main
	    write_scheme
	    type_pptype
	    tools_shape
	    tools_args)
   (export  (write-ast ast)))

;*---------------------------------------------------------------------*/
;*    write-ast ...                                                    */
;*---------------------------------------------------------------------*/
(define (write-ast globals)
   (profile write
      (let* ((output-name (if (string? *dest*)
			      *dest*
			      (if (and (pair? *src-files*)
				       (string? (car *src-files*)))
				  (string-append (prefix (car *src-files*))
				     "." (symbol->string *pass*)
				     ".ast")
				  #f)))
	     (port (if (string? output-name)
		       (open-output-file output-name)
		       (current-output-port))))
	 (if (not (output-port? port))
	     (error "write-ast" "Can't open output file" output-name)
	     (unwind-protect
		(begin
		   (if (not *ast-case-sensitive*)
		       (set! *pp-case* 'lower))
		   (write-scheme-file-header port
					     (string-append "The AST ("
							    *current-pass*
							    ")"))
		   (for-each
		    (lambda (g)
		       (let ((fun (global-value g)))
			  (write-scheme-comment port (shape g))
			  (write-scheme-comment port
						(function-type->string g))
			  (write-scheme-comment port (make-sfun-sinfo g))
			  (pp `(,(case (sfun-class fun)
				    ((sgfun)
				     'define-generic)
				    ((sifun)
				     'define-inline)
				    ((smfun)
				     'define-method)
				    (else
				     'define))
				,(cons (shape g)
				       (args-list->args*
					(map shape (sfun-args fun))
					(sfun-arity fun)))
				,(shape (sfun-body fun)))
			      port)))
		    globals))
		(close-output-port port))))))

;*---------------------------------------------------------------------*/
;*    make-sfun-sinfo ...                                              */
;*---------------------------------------------------------------------*/
(define (make-sfun-sinfo::bstring g::global)
   (define (atom->string atom)
      (case atom
	 ((#t)
	  "#t")
	 ((#f)
	  "#f")
	 ((#unspecified)
	  "#unspecified")
	 (else
	  (cond
	     ((symbol? atom)
	      (if *ast-case-sensitive*
		  (symbol->string atom)
		  (string-downcase (symbol->string atom))))
	     ((number? atom)
	      (number->string atom))
	     ((string? atom)
	      atom)
	     (else
	      (let ((p (open-output-string)))
		 (display atom p)
		 (close-output-port p)))))))
   (let ((sfun (global-value g)))
      (string-append "["
		     (if *ast-case-sensitive*
			 (symbol->string (global-import g))
			 (string-downcase (symbol->string (global-import g))))
		     "  side-effect: " (atom->string (sfun-side-effect sfun))
		     (let ((t (sfun-predicate-of sfun)))
			(if (type? t)
			    (string-append "  predicate-of: "
					   (atom->string (shape t)))
			    ""))
		     "  occ: " (integer->string (global-occurrence g))
		     "  rm: " (atom->string (global-removable g))
		     "  loc: " (let ((p (open-output-string)))
				  (display (sfun-loc sfun) p)
				  (close-output-port p))
		     (if (global-user? g)
			 "  user?: #t"
			 "  user?: #f")
		     " removable: " (with-output-to-string
				       (lambda ()
					  (display (global-removable g))))
		     "]")))
