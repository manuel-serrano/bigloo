;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bprof/bprof.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 31 07:37:29 1998                          */
;*    Last change :  Fri Nov  4 11:00:52 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bigloo profiler utility.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bprof
   (main main))

;*---------------------------------------------------------------------*/
;*    Global parameters                                                */
;*---------------------------------------------------------------------*/
(define *bprof-version*   "0.0")
(define *bprof-verbose*   #f)
(define *bprof-prof*      "gprof")
(define *bprof-prof-args* "")
(define *bprof-bmon*      "bmon.out")

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; we parse command line arguments
   (parse-args argv)
   ;; we have to load now the bmon.out file
   (load-bmon-out)
   ;; cleanup the demangling
   (for-each (lambda (k)
		(hashtable-remove! *demangling-env* k))
	     *demangling-unmangle*)
   ;; we now start prof.
   (gprof))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args cmd-args)
   (define (usage args-parse-usage level)
      (print "usage: bglprof -v [prof options] [ objfile [ gmon.out ] ]")
      (newline)
      (args-parse-usage #f)
      (newline)
      (exit 0))
   (args-parse (cdr cmd-args)
      (("?")
       (usage args-parse-usage 1))
      (("-help" (help "This help message"))
       (usage args-parse-usage 1))
      (("-v" (help "Be verbose"))
       (set! *bprof-verbose* #t))
      (else
       (set! *bprof-prof-args* (string-append *bprof-prof-args* " " else)))))

;*---------------------------------------------------------------------*/
;*    *demangling-env* ...                                             */
;*---------------------------------------------------------------------*/
(define *demangling-env* (make-hashtable))
(define *demangling-unmangle* '("%"))

;*---------------------------------------------------------------------*/
;*    load-bmon-out ...                                                */
;*---------------------------------------------------------------------*/
(define (load-bmon-out)
   (if (not (file-exists? *bprof-bmon*))
       (warning "bmon" "Can't find bmon.out file -- " *bprof-bmon*)
       (let ((iport (open-input-file *bprof-bmon*)))
	  (if (not (input-port? iport))
	      (warning "bmon" "Can't open file for input" *bprof-bmon*)
	      (unwind-protect
		 (let loop ((exp (extra-demangle (read iport))))
		    (if (and (not (eof-object? exp))
			     (not (eq? #a012 exp)))
			(begin
			   (if (pair? exp)
			       (hashtable-put! *demangling-env*
					       (cadr exp)
					       exp))
			   (loop (extra-demangle (read iport))))))
		 (close-input-port iport))))))

;*---------------------------------------------------------------------*/
;*    gprof ...                                                        */
;*---------------------------------------------------------------------*/
(define (gprof)
   (let* ((cmd  (string-append *bprof-prof* *bprof-prof-args*))
	  (port (open-input-file (string-append "| " cmd))))
      (unwind-protect
	 (read/rp *gprof-grammar* port)
	 (close-input-port port))))

;*---------------------------------------------------------------------*/
;*    *gprof-grammar* ...                                              */
;*---------------------------------------------------------------------*/
(define *gprof-grammar*
   (regular-grammar ()
      ((+ (out #\Space #\Tab #\Newline))
       (let* ((string (the-string))
	      (cell   (hashtable-get *demangling-env* string)))
	  (cond
	     ((pair? cell)
	      (display (car cell)))
	     ((bigloo-mangled? string)
	      (display (bigloo-demangle string)))
	     (else
	      (display string))))
       (ignore))
      ((+ (in #\Space #\Tab #\Newline))
       (display (the-string))
       (ignore))
      (else
       (the-failure))))
	  
;*---------------------------------------------------------------------*/
;*    extra-demangle ...                                               */
;*    -------------------------------------------------------------    */
;*    This function makes some extra demangling and hiding. It does    */
;*    the following transformations:                                   */
;*      1. Remove extra _[0-9]+ expression after Scheme identifiers.   */
;*      2. IMPORTED-MODULES-INIT is forgotten.                         */
;*      3. BIGLOO_MAIN is forgotten.                                   */
;*      4. PROF-INIT is forgotten.                                     */
;*      5. MODULE-INITIALIZATION is translated into @module-name.      */
;*      6. LIBRARY-MODULES-INIT is forgotten.                          */
;*---------------------------------------------------------------------*/
(define (extra-demangle expr)
   (if (not (pair? expr))
       expr
       (let ((expr (match-case expr
		      (((?id ?fname ?pos) ?cid . ?-)
		       (list id cid))
		      (else
		       expr))))
	  (let* ((name (car expr))
		 (scm  (string->symbol name))
		 (c    (cadr expr)))
	     (cond
		((memq scm '(BIGLOO_MAIN IMPORTED-MODULES-INIT
					 PROF-INIT
					 LIBRARY-MODULES-INIT))
		 #unspecified)
		((eq? scm 'MODULE-INITIALIZATION)
		 (let ((len (string-length c))
		       (len-min (string-length "module_initialization_")))
		    (if (>fx len len-min)
			(let loop ((i len-min))
			   (cond
			      ((=fx i len)
			       #unspecified)
			      ((char-numeric? (string-ref c i))
			       (loop (+fx i 1)))
			      (else
			       (list (string->symbol
				      (string-append
				       "@"
				       (string-upcase
					(substring c (+fx i 1) len))))
				     c))))
			#unspecified)))
		(else
		 (let ((len (string-length name)))
		    (let loop ((i (-fx len 1)))
		       (cond
			  ((=fx i 0)
			   expr)
			  ((char-numeric? (string-ref name i))
			   (loop (-fx i 1)))
			  ((char=? (string-ref name i) #\_)
			   (list (string->symbol (substring name 0 i)) c))
			  (else
			   expr))))))))))
