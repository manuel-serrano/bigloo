;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/with.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 16:28:03 1996                          */
;*    Last change :  Fri Jun 21 08:10:34 2013 (serrano)                */
;*    Copyright   :  1996-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The with clauses compilation.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_with
   (import module_module
	   engine_param
	   tools_error
	   tools_location
	   module_impuse
	   module_library
	   read_access
	   heap_restore)
   (export (make-with-compiler)
	   (early-with-clauses)))

;*---------------------------------------------------------------------*/
;*    make-with-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-with-compiler)
   (instantiate::ccomp (id 'with) (producer with-producer)))

;*---------------------------------------------------------------------*/
;*    with-producer ...                                                */
;*---------------------------------------------------------------------*/
(define (with-producer clause)
   (match-case clause
      ((?- . ?protos)
       (for-each (lambda (proto) (with-parser proto clause)) protos))
      (else
       (user-error/location (find-location/loc clause
					       (find-location *module-clause*))
			    "Parse error"
			    (string-append "Illegal `with' clause")
			    clause
			    '()))))
   
;*---------------------------------------------------------------------*/
;*    with-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (with-parser proto clause)
   (let ((loc (find-location/loc
	       proto
	       (find-location/loc clause (find-location *module-clause*)))))
      (match-case proto
	 (((and ?name (? symbol?)) (and ?file (? string?)) . ?rest)
	  (let loop ((rest   rest)
		     (fnames (list file)))
	     (cond
		((null? rest)
		 (module-add-access! name (reverse! fnames) "."))
		((string? (car rest))
		 (loop (cdr rest)
		       (cons (car rest) fnames)))
		(else
		 (user-error/location loc
				      "Parse error"
				      (string-append "Illegal `with' clause")
				      clause
				      '()))))
	  (set! *with-files* (cons (o-name file) *with-files*))
	  (import-with-module! name proto))
	 (else
	  (if (not (symbol? proto))
	      (user-error/location loc
				   "Parse error"
				   (string-append "Illegal `with' clause")
				   clause
				   '())
	      (let ((b ((bigloo-module-resolver) proto '() ".")))
		 ;; this is not a user module
		 (if (not (pair? b))
		     (if (memq proto (heap-module-list))
			 ;; we are requiring a library module
			 (with-library-module! proto)
			 (user-error/location loc
					      proto
					      "can't access module"
					      proto
					      '()))
		     (begin
			;; this is a regular user module
			(set! *with-files* (cons (o-name (car b)) *with-files*))
			(import-with-module! proto proto)))))))))

;*---------------------------------------------------------------------*/
;*    o-name ...                                                       */
;*---------------------------------------------------------------------*/
(define (o-name file)   
   (string-append (prefix file) "." *c-object-file-extension*))
	     
;*---------------------------------------------------------------------*/
;*    early-with-clauses ...                                           */
;*    -------------------------------------------------------------    */
;*    Compute the early with clause by simply collecting the           */
;*    *EARLY-WITH-MODULES* values (see init_parser-args.scm and        */
;*    engine_param).                                                   */
;*---------------------------------------------------------------------*/
(define (early-with-clauses)
   `(with ,@*early-with-modules*))
	    
   
   
