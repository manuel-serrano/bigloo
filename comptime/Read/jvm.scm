;*=====================================================================*/
;*    serrano/bigloo/5.0a/comptime/Read/jvm.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 11:33:41 1993                          */
;*    Last change :  Fri Feb 13 08:10:31 2026 (serrano)                */
;*    Copyright   :  1993-2026 Manuel Serrano, see LICENSE file        */
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
   (export (module-package-get ::symbol #!optional warn)
	   (module-package-set! ::symbol ::symbol)
	   (class-qualified-type-name-get::bstring ::symbol)
	   (class-qualified-type-name-set! ::symbol ::bstring)
           (module-jvm-packages::obj)
	   (jvm-class-sans-directory::bstring ::bstring)
	   (jvm-class-with-directory::bstring ::bstring)
	   (read-jfile)))

;*---------------------------------------------------------------------*/
;*    *module-jvm-packages* ...                                        */
;*---------------------------------------------------------------------*/
(define *module-jvm-packages*
   (create-hashtable :size 512 :weak 'open-string))

;*---------------------------------------------------------------------*/
;*    *class-jvm-qualified-types* ...                                  */
;*---------------------------------------------------------------------*/
(define *class-jvm-qualified-types*
   (create-hashtable :size 512 :weak 'open-string))

;*---------------------------------------------------------------------*/
;*    module-jvm-packages ...                                          */
;*---------------------------------------------------------------------*/
(define (module-jvm-packages)
   *module-jvm-packages*)

;*---------------------------------------------------------------------*/
;*    module-package-get ...                                           */
;*---------------------------------------------------------------------*/
(define (module-package-get module::symbol #!optional warn)
   (let ((name (symbol->string! module)))
      (let ((pkg (hashtable-get *module-jvm-packages* name)))
	 (when (and warn (not pkg))
	    (warning
	       (string-append "Can't find package for module `"
		  (symbol->string module) "'.")))
	 pkg)))

;*---------------------------------------------------------------------*/
;*    module-package-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (module-package-set! module::symbol pkg::symbol)
   (let ((name (symbol->string! module)))
      (let ((old (hashtable-get *module-jvm-packages* name)))
	 (cond
	    ((not old)
	     (hashtable-put! *module-jvm-packages* name pkg))
	    ((eq? old pkg)
	     #unspecified)
	    (else
	     (warning name "module package redefinition"
		"\n  old package=" old
		"\n  new package=" pkg))))))

;*---------------------------------------------------------------------*/
;*    class-qualified-type-name-get ...                                */
;*---------------------------------------------------------------------*/
(define (class-qualified-type-name-get::bstring clazz::symbol)
   (let ((name (symbol->string! clazz)))
      (let ((qtn (hashtable-get *class-jvm-qualified-types* name)))
	 (unless (string? qtn)
	    (error "java" "Cannot find class qualified-type name" clazz))
	 qtn)))

;*---------------------------------------------------------------------*/
;*    class-qualified-type-name-set! ...                               */
;*---------------------------------------------------------------------*/
(define (class-qualified-type-name-set! clazz::symbol qtn::bstring)
   (let ((name (symbol->string! clazz)))
      (let ((old (hashtable-get *class-jvm-qualified-types* name)))
	 (cond
	    ((not old)
	     (hashtable-put! *class-jvm-qualified-types* name qtn))
	    ((not (string=? old qtn))
	     (error clazz "Using two different qualified names for class"
		(format "~a vs ~a" qtn old)))))))

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
   
   (with-trace 'jvm "read-jfile"
      (trace-item "jfile=" *qualified-type-file*)
      (cond
	 ((not (string? *qualified-type-file*))
	  (if (file-exists? *qualified-type-file-default*)
	      (inner-read-qualified-type-file *qualified-type-file-default*)
	      'done))
	 ((not (file-exists? *qualified-type-file*))
	  (user-error 'read-jfile "Can't find jfile" *qualified-type-file*))
	 (else
	  (inner-read-qualified-type-file *qualified-type-file*)))))

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
		    (((and (? symbol?) ?mod) (and ?qtype (? string?)))
		     (class-qualified-type-name-set! mod qtype)
		     (let ((pqtn (prefix qtype)))
			(if (string=? pqtn qtype)
			    (module-package-set! mod '||)
			    (module-package-set! mod (string->symbol pqtn))))
		     (loop (cdr obj)))
		    (else
		     (user-error 'read-jfile
			"Illegal jfile format" (car obj))))))))))
