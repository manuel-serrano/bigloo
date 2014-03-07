;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/jvm.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 11:33:41 1993                          */
;*    Last change :  Fri Mar  7 12:26:52 2014 (serrano)                */
;*    Copyright   :  1993-2014 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The module which handles `qualified type <-> module' associations*/
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module read_jvm
   (import engine_param
	   engine_engine
	   backend_backend
	   module_module
	   tools_error
	   init_main
	   tools_speek)
   (export (jvm-class-sans-directory::bstring ::bstring)
	   (jvm-class-with-directory::bstring ::bstring)
	   (add-qualified-type! ::symbol ::bstring . obj)
	   (read-jfile)
	   (module->qualified-type::bstring ::symbol)
	   (source->qualified-type file::bstring)))

;*---------------------------------------------------------------------*/
;*    jvm-class-sans-directory ...                                     */
;*---------------------------------------------------------------------*/
(define (jvm-class-sans-directory file)
   (if (not (string? *jvm-directory*))
       file
       (let ((ld (string-length *jvm-directory*))
	     (lf (string-length file)))
	  (if (or (< lf (+fx ld 1))
		  (not (substring=? file *jvm-directory* ld))
		  (not (char=? (string-ref file ld) (file-separator))))
	      file
	      (substring file (+fx 1 ld) lf)))))

;*---------------------------------------------------------------------*/
;*    jvm-class-with-directory ...                                     */
;*---------------------------------------------------------------------*/
(define (jvm-class-with-directory class)
   (cond
      ((not (string? *jvm-directory*))
       class)
      ((string=? class "")
       *jvm-directory*)
      (else
       (make-file-name *jvm-directory* class))))

;*---------------------------------------------------------------------*/
;*    *jvm-mark* ...                                                   */
;*---------------------------------------------------------------------*/
(define *jvm-mark* 'jvm-qtype)

;*---------------------------------------------------------------------*/
;*    add-qualified-type! ...                                          */
;*---------------------------------------------------------------------*/
(define (add-qualified-type! module::symbol qtype::bstring . ident)
   (let ((bc (the-backend)))
      (when (and (backend? bc)
		 (string=? qtype "")
		 (backend-qualified-types (the-backend)))
	 (warning "add-qualified-type!"
		  "empty name for module -- "
		  module
		  (if (and (pair? ident) (symbol? (car ident)))
		      (string-append ", for identifier `"
				     (symbol->string (car ident))
				     "'")
		      "")))
      (let ((b (getprop module *jvm-mark*)))
	 (if (not b)
	     (putprop! module *jvm-mark* qtype)
	     (when (and (backend? bc)
			(not (equal? b qtype))
			(backend-qualified-types (the-backend)))
		(putprop! module *jvm-mark* qtype)
		(warning "add-qualified-type!"
			 "qualified type redefinition:\n  module/class=" module
			 (if (and (pair? ident) (symbol? (car ident)))
			     (string-append "\n  identifier="
					    (symbol->string (car ident)))
			     "")
			 "\n  old qualified type=" b
			 "\n  new qualified type=" qtype
			 "\n")
		(dump-trace-stack (current-error-port) 10))))))

;*---------------------------------------------------------------------*/
;*    read-jfile ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-jfile)
   (define (inner-read-qualified-type-file name::bstring)
      (let ((port (open-input-file name)))
	 (verbose 2 "      [reading jfile " name "]" #\Newline)
	 (if (not (input-port? port))
	     (user-error 'read-jfile "Can't open jfile" name)
	     (unwind-protect
		(do-read-jfile port name)
		(close-input-port port)))))
   ;; then, we try to read the actual jfile
   (cond
      ((not (string? *qualified-type-file*))
       (if (file-exists? *qualified-type-file-default*)
	   (inner-read-qualified-type-file *qualified-type-file-default*)
	   'done))
      ((not (file-exists? *qualified-type-file*))
       (user-error 'read-jfile "Can't find jfile" *qualified-type-file*))
      (else
       (inner-read-qualified-type-file *qualified-type-file*))))

;*---------------------------------------------------------------------*/
;*    do-read-jfile ...                                                */
;*---------------------------------------------------------------------*/
(define (do-read-jfile port jfname)
   (let* ((obj (read port #t))
	  (eof (read port)))
      (cond
	 ((eof-object? obj)
	  (user-error 'read-jfile "Illegal jfile format" obj))
	 ((not (eof-object? eof))
	  (user-error 'read-jfile "Illegal jfile format" eof))
	 (else
	  (let loop ((obj obj))
	     (if (null? obj)
		 'done
		 (match-case (car obj)
		    (((and (? symbol?) ?m) (and ?pckg (? string?)))
		     (add-qualified-type! m pckg)
		     (loop (cdr obj)))
		    (else
		     (user-error 'read-jfile
				 "Illegal jfile format"
				 (car obj))))))))))

;*---------------------------------------------------------------------*/
;*    add-current-module-qualified-type-name! ...                      */
;*---------------------------------------------------------------------*/
(define (add-current-module-qualified-type-name!)
   ;; then we add information specific to the current module
   (let ((qtype (getprop *module* *jvm-mark*)))
      (if (not (string? qtype))
	  ;; The current module is not present in loaded jfile, we
	  ;; have to infere a qualified type. For this, we look at
	  ;; the name of the destination file.
	  (cond
	     ((or (not (string? *dest*)) (eq? *pass* 'ld))
	      ;; there is no specified destination so the JVM package is
	      ;; just bigloo
	      (let ((qt (if (string? (car *src-files*))
			    (prefix (basename (car *src-files*)))
			    ".")))
		 (add-qualified-type! *module* qt)
		 qt))
	     (else
	      (let ((qt (prefix *dest*)))
		 ;; there is a destination
		 (add-qualified-type! *module* qt)
		 qt))))))

;*---------------------------------------------------------------------*/
;*    module->qualified-type ...                                       */
;*    -------------------------------------------------------------    */
;*    From a module name, returns the Java qualified type name.        */
;*---------------------------------------------------------------------*/
(define (module->qualified-type::bstring module::symbol)
   (let ((b (getprop module *jvm-mark*)))
      (cond
	 ((string? b)
	  b)
	 ((eq? module *module*)
	  (add-current-module-qualified-type-name!))
	 (else
	  (let* ((abase (map dirname *access-files*))
		 (files ((bigloo-module-resolver) module '() abase))
		 (default (if (pair? files)
	 		      (prefix (basename (car files)))
			      (symbol->string module))))
	     (if (backend-qualified-types (the-backend))
		 (warning
		  (string-append "Can't find qualified type name for module `"
				 (symbol->string module) "',")
		  "Using name `" default "'."))
	     (add-qualified-type! module default)
	     default)))))

;*---------------------------------------------------------------------*/
;*    source->qualified-type ...                                       */
;*    -------------------------------------------------------------    */
;*    From a file source name, returns the Java qualified type name.   */
;*---------------------------------------------------------------------*/
(define (source->qualified-type file::bstring)
   (if (file-exists? file)
       (with-input-from-file file
	  (lambda ()
	     (match-case (read)
		((module ?mod . ?-)
		 (module->qualified-type mod))
		(else
		 #f))))
       #f))
