;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/load.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 29 10:30:51 1994                          */
;*    Last change :  Mon May 15 08:00:23 2000 (serrano)                */
;*    Copyright   :  1994-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We scan files in order to find `inline' definitions.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module read_load
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_speek
	    type_type
	    ast_ident)
   (export  (load-module module::symbol fnames::pair)))

;*---------------------------------------------------------------------*/
;*    load-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (load-module module fnames)
   (verbose 1 "      [reading loaded module " module "]" #\Newline)
   (let* ((file  (car fnames))
	  (fname (find-file/path file *load-path*)))
      (if (not (string? fname))
	  (user-error "load-module" "Can't find file" file)
	  (let ((port (open-input-file fname)))
	     (reader-reset!)
	     (if (not (input-port? port))
		 (user-error "load-module" "Can't open such file" file)
		 (unwind-protect (let ((decl (read port)))
				    (if (not (and (pair? decl)
						  (eq? (car decl) 'module)))
					(user-error
					 "load-module"
					 "Illegal module declaration"
					 decl)
					;; on verifie le nome
					(if (not (eq? (cadr decl) module))
					    (user-error
					     "load-module"
					     (string-append
					      "conflict in module's name: "
					      (symbol->string module) " vs "
					      (symbol->string (cadr decl)))
					     decl))))
				 (close-input-port port))))))
   (for-each loadq fnames))
