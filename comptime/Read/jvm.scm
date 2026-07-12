;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/comptime/Read/jvm.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 11:33:41 1993                          */
;*    Last change :  Sun Jul 12 09:32:43 2026 (serrano)                */
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
   (export (default-jvm-package-set! ::symbol)
	   (default-jvm-package)
           (jvm-qualified-names::obj)
	   (jvm-package-get ::symbol #!optional warn)
	   (jvm-qualified-name-get ::symbol #!optional warn)
	   (jvm-qualified-name-set! ::symbol ::symbol)
	   (class-qualified-type-name-get::bstring ::symbol)
	   (class-qualified-type-name-get/def::bstring ::symbol)
	   (jvm-class-sans-directory::bstring ::bstring)
	   (jvm-class-with-directory::bstring ::bstring)
	   (read-jfile)))

;*---------------------------------------------------------------------*/
;*    *default-jvm-package* ...                                        */
;*---------------------------------------------------------------------*/
(define *default-jvm-package* #f)

;*---------------------------------------------------------------------*/
;*    default-jvm-package-set! ...                                     */
;*---------------------------------------------------------------------*/
(define (default-jvm-package-set! pkg::symbol)
   (cond
      ((not *default-jvm-package*)
       (set! *default-jvm-package* pkg))
      ((not (eq? *default-jvm-package* pkg))
       (error "jvm" "Two different default package names used"
	  (format "\"~a\" vs \"~a\"" *default-jvm-package* pkg)))))

;*---------------------------------------------------------------------*/
;*    default-jvm-package ...                                          */
;*---------------------------------------------------------------------*/
(define (default-jvm-package)
   *default-jvm-package*)

;*---------------------------------------------------------------------*/
;*    *jvm-qualified-names* ...                                        */
;*---------------------------------------------------------------------*/
(define *jvm-qualified-names*
   (create-hashtable :size 512 :weak 'open-string))

;*---------------------------------------------------------------------*/
;*    jvm-qualified-names ...                                          */
;*---------------------------------------------------------------------*/
(define (jvm-qualified-names)
   *jvm-qualified-names*)

;*---------------------------------------------------------------------*/
;*    jvm-qualified-name-get ...                                       */
;*---------------------------------------------------------------------*/
(define (jvm-qualified-name-get module::symbol #!optional warn)
   (let ((name (symbol->string! module)))
      (let ((pkg (hashtable-get *jvm-qualified-names* name)))
	 (when (and warn (not pkg))
	    (warning
	       (format "Can't find qualified name for module \"a\"." module)))
	 pkg)))

;*---------------------------------------------------------------------*/
;*    jvm-package-get ...                                              */
;*---------------------------------------------------------------------*/
(define (jvm-package-get module::symbol #!optional warn)
   (let ((qn (jvm-qualified-name-get module warn)))
      (when (symbol? qn)
	 (string->symbol (prefix (symbol->string! qn))))))

;*---------------------------------------------------------------------*/
;*    jvm-qualified-name-set! ...                                      */
;*---------------------------------------------------------------------*/
(define (jvm-qualified-name-set! module::symbol qn::symbol)
   (with-trace 'jvm "jvm-qualified-name-set!"
      (trace-item "module=" module)
      (trace-item "qn=" qn)
      (let ((name (symbol->string! module)))
	 (let ((old (hashtable-get *jvm-qualified-names* name)))
	    (cond
	       ((not old)
		(hashtable-put! *jvm-qualified-names* name qn))
	       ((eq? old qn)
		#unspecified)
	       (else
		(error "java"
		   (format "different qualified names used for module \"~a\""
		      module)
		   (format "\"~a\" vs \"~a\"" old qn))))))))

;*---------------------------------------------------------------------*/
;*    class-qualified-type-name-get ...                                */
;*---------------------------------------------------------------------*/
(define (class-qualified-type-name-get::bstring clazz::symbol)
   (with-trace 'jvm "class-qualified-type-name-get"
      (trace-item "clazz=" clazz)
      (let ((qn (jvm-qualified-name-get clazz #f)))
	 (if (symbol? qn)
	     (symbol->string qn)
	     (error "java" "Cannot find qualified-type name" clazz)))))

;*---------------------------------------------------------------------*/
;*    class-qualified-type-name-get/def ...                            */
;*---------------------------------------------------------------------*/
(define (class-qualified-type-name-get/def::bstring clazz::symbol)
   (with-trace 'jvm "class-qualified-type-name-get"
      (trace-item "clazz=" clazz)
      (let ((qn (jvm-qualified-name-get clazz #f)))
	 (cond
	    ((symbol? qn)
	     (symbol->string qn))
	    (*default-jvm-package*
	     (let ((qn (format "~a.~a" *default-jvm-package* clazz)))
		(jvm-qualified-name-set! clazz (string->symbol qn))
		qn))
	    (else
	     (jvm-qualified-name-set! clazz clazz)
	     (symbol->string clazz))))))

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
	  (user-error "read-jfile" "Can't find jfile" *qualified-type-file*))
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
	  (user-error "read-jfile" "Illegal jfile format" obj))
	 ((not (eof-object? eof))
	  (user-error "read-jfile" "Illegal jfile format" eof))
	 (else
	  (let loop ((obj obj))
	     (if (null? obj)
		 'done
		 (match-case (car obj)
		    (((and (? symbol?) ?mod) (and ?qtype (? string?)))
		     (jvm-qualified-name-set! mod (string->symbol qtype))
		     (loop (cdr obj)))
		    (else
		     (user-error "read-jfile"
			"Illegal jfile format" (car obj))))))))))

