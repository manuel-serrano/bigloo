;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/tuning.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 18 14:35:46 2006                          */
;*    Last change :  Mon Apr 23 06:59:37 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Package tuning                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_tuning

   (library pkglib)

   (import  bglpkg_configure
	    bglpkg_param
	    bglpkg_utils)

   (export  (tune-package ::bstring ::bstring ::bstring ::obj ::pair)))

;*---------------------------------------------------------------------*/
;*    tune-package ...                                                 */
;*---------------------------------------------------------------------*/
(define (tune-package dir package version tuning language)
   (with-trace 3 'tune-package
      (trace-item "dir=" dir)
      (trace-item "package=" package)
      (trace-item "tuning=" tuning))
   (let* ((pdir (make-file-name dir package))
	  (tdir (make-file-path pdir tuning))
	  (suf (assq/default (car language) (bglpkg-language-suffix) ".scm")))
      ;; tune the configure
      (tune-configure package version tuning pdir tdir)
      ;; tune the source code
      (for-each (lambda (f)
		   (when (string-suffix-ci? ".spi" f)
		      (when (>fx (pkglib-verbose) 2)
			 (print "  tuning file " (pkglib-color 'arg0 f)))
		      (let ((pk (prefix f)))
			 (tune-package-bgl! pk version tuning pdir tdir suf)
			 (tune-package-spi! pk version tuning pdir tdir suf)
			 (tune-package-scm! pk version tuning pdir tdir suf))))
		(directory->list pdir))
      ;; tune the test suite
      (let ((testdir (make-file-name pdir "test")))
	 (when (and (file-exists? testdir) (directory? testdir))
	    (let ((pkgtest (string-append package "-test"))
		  (pdir (make-file-name pdir "test")))
	       (tune-package-bgl! pkgtest version tuning pdir tdir suf)
	       (tune-package-spi! pkgtest version tuning pdir tdir suf)
	       (tune-package-scm! pkgtest version tuning pdir tdir suf))))))

;*---------------------------------------------------------------------*/
;*    tune-configure ...                                               */
;*---------------------------------------------------------------------*/
(define (tune-configure pkg version tuning pdir tdir)
   (let ((configure.scm (make-file-name tdir "configure.scm"))
	 (configure.sh (make-file-name tdir "configure.sh")))
      (when (file-exists? configure.scm)
	 (load configure.scm))
      (when (file-exists? configure.sh)
	 (unless (=fx 0 (system (format "~a ~a" (bglpkg-shell) configure.sh)))
	    (fprint (current-error-port)
		    "Tuning configuration failed for package. "
		    pkg "-" version
		    ". Exiting...")
	    (exit 0)))))
      
;*---------------------------------------------------------------------*/
;*    tune-package-bgl! ...                                            */
;*---------------------------------------------------------------------*/
(define (tune-package-bgl! pkg version tuning pdir tdir suf)
   (let* ((bglfile (string-append pkg ".bgl"))
	  (tbgl (make-file-name tdir bglfile)))
      (when (file-exists? tbgl)
	 (copy-file tbgl (make-file-name pdir (string-append pkg suf)))
	 (with-output-to-file (make-file-name pdir (string-append pkg ".spi"))
	    (lambda ()
	       (print `(interface ,pkg
			  (language bigloo)
			  (bigloo (module-override)))))))))

;*---------------------------------------------------------------------*/
;*    tune-package-spi! ...                                            */
;*---------------------------------------------------------------------*/
(define (tune-package-spi! pkg version tuning pdir tdir suf)
   (let* ((spifile (string-append pkg ".spi"))
	  (tspi (make-file-name tdir spifile)))
      (if (file-exists? tspi)
	  ;; substitute the whole spi file
	  (copy-file tspi (make-file-name pdir spifile))
	  (let ((after (make-file-name tdir (string-append pkg "-after.spi"))))
	     (if (file-exists? after)
		 ;; add clauses after the original interface declaration
		 (let ((intf (read-file (make-file-name pdir spifile)))
		       (add (read-file after)))
		    ;; remove the last closing parenthesis
		    (with-output-to-file (make-file-name pdir spifile)
		       (lambda ()
			  (let ((i (string-index-right intf #\))))
			     (print (if i (string-shrink! intf i) intf))
			     (print ";; " tuning " after .spi tuning")
			     (print add)
			     (print ")"))))))))))

;*---------------------------------------------------------------------*/
;*    tune-package-scm! ...                                            */
;*---------------------------------------------------------------------*/
(define (tune-package-scm! pkg version tuning pdir tdir suf)
   (let* ((scmfile (string-append pkg suf))
	  (tscm (make-file-name tdir scmfile)))
      (if (file-exists? tscm)
	  ;; substitute the whole scm file
	  (copy-file tscm (make-file-name pdir scmfile))
	  (let ((ovd (make-file-name tdir (string-append pkg "-override.scm")))
		(bef (make-file-name tdir (string-append pkg "-before.scm")))
		(aft (make-file-name tdir (string-append pkg "-after.scm"))))
	     (when (file-exists? bef)
		;; add the before body.
		(let* ((sfil (make-file-name pdir scmfile))
		       (add (read-file bef))
		       (old (read-file sfil)))
		   (with-output-to-file sfil
		      (lambda ()
			 (print ";; " tuning " before .scm tuning")
			 (display add)
			 (print ";; original code")
			 (display old)
			 (newline)))))
	     (when (file-exists? aft)
		;; add the after body at the end of the source
		(let ((add (read-file aft)))
		   ;; append the file
		   (with-append-to-file (make-file-name pdir scmfile)
		      ;;; manual indent
		      (lambda ()
			 (print ";; " tuning " after .scm tuning")
			 (display add)
			 (newline)))))
	     ;; override the body
	     (when (file-exists? ovd)
		(body-override tdir pdir scmfile ovd))))))

;*---------------------------------------------------------------------*/
;*    body-override ...                                                */
;*    -------------------------------------------------------------    */
;*    Override the originial definitions with the ones found           */
;*    in the override file.                                            */
;*---------------------------------------------------------------------*/
(define (body-override tdir pdir scmfile override)
   (define (get-def-ident exp)
      (match-case exp
	 ((@undef ?var)
	  var)
	 ((define (and ?var (or (? symbol?) (? keyword?))) . ?-)
	  var)
	 (((or @define define define-macro define-inline) (?var . ?-) . ?-)
	  var)
	 ((define-syntax ?var . ?-)
	  var)
	 ((define-expander ?var . ?-)
	  var)
	 ((define-record (or (and ?var (? symbol?))
			     ((and ?var (? symbol?)) ?-))
	     . ?-)
	  var)))
   (let ((defs (make-hashtable))
	 (tscmfile (make-file-name tdir scmfile))
	 (pscmfile (make-file-name pdir scmfile)))
      ;; during a first pass, read all the definitions
      (with-input-from-file override
	 (lambda ()
	    (let loop ()
	       (let ((e (read)))
		  (unless (eof-object? e)
		     (let ((i (get-def-ident e)))
			(when i (hashtable-put! defs i i))
			(loop)))))))
      ;; get the commented definitions position
      (let ((pos (with-input-from-file pscmfile
		    (lambda ()
		       (let loop ((pos '()))
			  (let ((e (read (current-input-port) #t)))
			     (if (eof-object? e)
				 (reverse! pos)
				 (let ((i (get-def-ident e)))
				    (if (and i (hashtable-get defs i))
					(loop (cons (caddr (cer e)) pos))
					(loop pos))))))))))
	 ;; build the new source file
	 (with-output-to-file tscmfile
	    (lambda ()
	       ;; comment the definitions
	       (with-input-from-file pscmfile
		  (lambda ()
		     (let loop ((pos (if (and (pair? pos) (=fx (car pos) 0))
					 (begin
					    (display "#;")
					    (cdr pos))
					 pos)))
			(if (null? pos)
			    (display (read-string))
			    (let ((p (input-port-position (current-input-port))))
			       (display (read-chars (- (car pos) p)))
			       (display "#;")
			       (loop (cdr pos)))))))
	       ;; write the new definitions
	       (print ";; overriden definitions")
	       (print '(define-macro (@undef . args) #f))
	       (display (read-file override))
	       (newline)))
	 ;; switch the files
	 (delete-file pscmfile)
	 (rename-file tscmfile pscmfile))))

;*---------------------------------------------------------------------*/
;*    with-append-to-file ...                                          */
;*---------------------------------------------------------------------*/
(define (with-append-to-file file proc)
   (let ((p (append-output-file file)))
      (unwind-protect
	 (with-output-to-port p proc)
	 (close-output-port p))))

;*---------------------------------------------------------------------*/
;*    *body* ...                                                       */
;*---------------------------------------------------------------------*/
(define *package* '())
(define *body* '())
(define *dir* #f)

;*---------------------------------------------------------------------*/
;*    tune ...                                                         */
;*---------------------------------------------------------------------*/
(define (tune suf package tuning tuner packagedir pkgdecl body)
   (set! *package* pkgdecl)
   (set! *body* body)
   (set! *dir* packagedir)
   (when (>=fx (pkglib-verbose) 2)
      (print "  tuning for " (pkglib-color 'ok tuning)))
   (loadq tuner)
   (let* ((pfile (string-append package "." suf))
	  (path (make-file-name packagedir pfile))
	  (op (open-output-file path)))
      (unless (output-port? op)
	 (raise (instantiate::&io-port-error
		   (proc 'tune)
		   (msg "Cannot open file for output")
		   (obj pfile))))
      (unwind-protect
	 (begin
	    (fprint op ";; automatically generated, don't edit")
	    (fprint op ";; generated by " (bglpkg-name) ", tuned for " tuning)
	    (fprint op ";; " (current-date))
	    (newline op)
	    (fprint op ";; package")
	    (pp-package *package* op)
	    (newline op)
	    (fprint op ";; body")
	    (for-each (lambda (b)
			 (pp b op)
			 (newline op))
		      *body*))
	 (close-output-port op))))

;*---------------------------------------------------------------------*/
;*    pp-package ...                                                   */
;*---------------------------------------------------------------------*/
(define (pp-package pkg port)
   (with-output-to-port port
      (lambda ()
	 (match-case pkg
	    ((?key ?pkg ?v . ?clauses)
	     (display* "(" key  " " pkg " " v)
	     (for-each (lambda (c)
			  (display* "\n   (" (car c))
			  (if (eq? (car c) provide:)
			      (for-each (lambda (d)
					   (display "\n    ")
					   (write d))
					(cdr c))
			      (for-each (lambda (d)
					   (display " ")
					   (write d))
					(cdr c)))
			  (display ")"))
		       clauses)
	     (print ")"))))))

;*---------------------------------------------------------------------*/
;*    bglpkg-tune-package-declaration! ...                             */
;*---------------------------------------------------------------------*/
(define (bglpkg-tune-package-declaration! p)
   (unless (and (correct-arity? p 3))
      (error 'bglpkg-tune-package-declaration! "Incorrect package tuner" p))
   (let ((r (p *package* "" *dir*)))
      (unless (eq? r #unspecified) (set! *package* r)))
   *package*)

;*---------------------------------------------------------------------*/
;*    bglpkg-tune-body! ...                                            */
;*---------------------------------------------------------------------*/
(define (bglpkg-tune-body! p)
   (unless (and (correct-arity? p 3))
      (error 'bglpkg-tune-body! "Incorrect package tuner" p))
   (let ((r (p *body* "" *dir*)))
      (unless (eq? r #unspecified) (set! *body* r)))
   *body*)

;*---------------------------------------------------------------------*/
;*    bglpkg-replace-package-provide! ...                              */
;*---------------------------------------------------------------------*/
(define (bglpkg-replace-package-provide! src replacements)
   (bglpkg-replace-definitions! src replacements))

;*---------------------------------------------------------------------*/
;*    bglpkg-replace-body-definitions! ...                             */
;*---------------------------------------------------------------------*/
(define (bglpkg-replace-body-definitions! src replacements)
   (bglpkg-replace-definitions! src replacements))

;*---------------------------------------------------------------------*/
;*    bglpkg-replace-definitions! ...                                  */
;*---------------------------------------------------------------------*/
(define (bglpkg-replace-definitions! src replacements)
   (define (replace orig id)
      (let ((c (assq id replacements)))
	 (if (pair? c)
	     (cadr c)
	     orig)))
   (filter (lambda (x) x)
	   (map! (lambda (orig)
		    (match-case orig
		       (((or define* define) (and ?id (? symbol?)) . ?-)
			(replace orig id))
		       (((or define* define define-macro* define-macro)
			 ((and ?id (? symbol?)) . ?-) . ?-)
			(replace orig id))
		       ((define-record* ?id . ?-)
			(replace orig id))
		       (else
			orig)))
		 src)))
