;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pkglib/src/Llib/package.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 13 11:57:08 2006                          */
;*    Last change :  Wed Dec 26 09:39:28 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Tools for handling packages                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pkglib_package
   
   (import __pkglib_param
	   __pkglib_misc
	   __pkglib_interface)
   
   (export (package-filename?::bool ::bstring)
	   (package-tuning?::bool ::bstring)
	   (package-version?::bool ::bstring)
	   (package-name-parse::obj ::bstring)
	   (package-sans-suffix::bstring ::bstring)
	   (make-package-name ::bstring ::bstring ::obj ::obj)
	   (package-extract-interface ::bstring ::bstring)
	   (package-extract-meta ::bstring ::bstring)
	   (package-companions ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    package-filename? ...                                            */
;*---------------------------------------------------------------------*/
(define (package-filename? name)
   (string-suffix-ci? (pkglib-package-suffix) name))

;*---------------------------------------------------------------------*/
;*    package-tuning? ...                                              */
;*---------------------------------------------------------------------*/
(define (package-tuning? name)
   (let ((i (string-index-right name (pkglib-package-tuning-mark))))
      (when i
	 (let loop ((j 0))
	    (cond
	       ((=fx j i)
		#f)
	       ((char=? (string-ref name j) (pkglib-package-tuning-mark))
		(loop (+fx j 1)))
	       (else
		#t))))))

;*---------------------------------------------------------------------*/
;*    package-version? ...                                             */
;*---------------------------------------------------------------------*/
(define (package-version? name)
   (string-case name
      ((: (= 2 (: (+ digit) #\.)) (+ digit))
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    package-sans-suffix ...                                          */
;*---------------------------------------------------------------------*/
(define (package-sans-suffix name)
   (substring name
	      0
	      (-fx (string-length name)
		   (+fx 1 (string-length (pkglib-package-suffix))))))

;*---------------------------------------------------------------------*/
;*    package-name-parse ...                                           */
;*    -------------------------------------------------------------    */
;*    Parses a package file name and returns the base name, the        */
;*    host tuning name, the version number, and release number.        */
;*    The syntax of a package name is:                                 */
;*    <package> -> <interface> | <tuning>                              */
;*    <interface> -> .*-[0-9]+.0-9]+.[0-9]+(-[0-9]+).tar.tz            */
;*    <tuning> -> .+_.+-[0-9]+.0-9]+.[0-9]+(-[0-9]+).tar.tz            */
;*---------------------------------------------------------------------*/
(define (package-name-parse name)
   (unless (package-filename? name)
      (pkglib-error 'package-name-parse "Illegal package name" name))
   (let* ((n (package-sans-suffix (basename name)))
	  (index (string-index-right n #\-))
	  (vdot (string-index-right n #\.)))
      (cond
	 ((not index)
	  (pkglib-error 'package-name-parse "Illegal package name" name))
	 ((and (fixnum? vdot) (>fx vdot index))
	  ;; a package without release
	  (let* ((version (substring n (+fx 1 index) (string-length n)))
		 (base (substring n 0 index))
		 (tindex (string-index-right n (pkglib-package-tuning-mark))))
	     (if (and (fixnum? tindex) (>fx tindex 0))
		 (values (substring base 0 tindex)
			 version
			 "0"
			 (substring base (+fx 1 tindex) (string-length base)))
		 (values base
			 version
			 "0"
			 #f))))
	 (else
	  ;; a package with release
	  (let ((vindex (string-index-right n #\- index))
		(tindex (string-index-right n (pkglib-package-tuning-mark))))
	     (if (not vindex)
		 (pkglib-error 'package-name-parse "Illegal package name" name)
		 (let* ((version (substring n (+fx 1 vindex) index))
			(release (substring n (+fx 1 index) (string-length n)))
			(base (substring n 0 vindex)))
		    (if (and (fixnum? tindex) (>fx tindex 0))
			(values (substring base 0 tindex)
				version
				release
				(substring base (+fx 1 tindex) vindex))
			(values base
				version
				release
				#f)))))))))

;*---------------------------------------------------------------------*/
;*    make-package-name ...                                            */
;*---------------------------------------------------------------------*/
(define (make-package-name package version release tuning)
   (let ((base (if tuning
		   (string-append package
				  (string (pkglib-package-tuning-mark))
				  tuning
				  "-"
				  version)
		   (string-append package "-" version))))
      (if release
	  (string-append base "-" release "." (pkglib-package-suffix))
	  (string-append base "." (pkglib-package-suffix)))))
		       
;*---------------------------------------------------------------------*/
;*    package-extract-interface ...                                    */
;*    -------------------------------------------------------------    */
;*    Uncompress a package a extract the package information.          */
;*---------------------------------------------------------------------*/
(define (package-extract-interface path base)
   (with-trace 4 'package-extract-interface
      (trace-item "path=" path)
      (trace-item "base=" base)
      (let ((intf (make-file-name base
				  (string-append base
						 "."
						 (pkglib-interface-suffix))))
	    (pz (open-input-gzip-file path)))
	 (unless (input-port? pz)
	    (raise (instantiate::&io-read-error
		      (proc 'package-extract-interface)
		      (msg "Can't open file for input")
		      (obj path))))
	 (unwind-protect
	    (with-trace 10 'package-extract-interface::untar
	       (let untar ()
		  (let ((h (tar-read-header pz)))
		     (if (not h)
			 #f
			 (with-access::tar-header h (type name)
			    (trace-item "header name=" name " type=" type)
			    (if (eq? type 'normal)
				(if (string=? name intf)
				    (interface-read-interface pz)
				    (begin
				       (tar-read-block h pz)
				       (untar)))
				(untar)))))))
	    (close-input-port pz)))))

;*---------------------------------------------------------------------*/
;*    package-extract-meta ...                                         */
;*    -------------------------------------------------------------    */
;*    Uncompress a package a extract the info information.             */
;*---------------------------------------------------------------------*/
(define (package-extract-meta path base)
   (with-trace 4 'package-extract-info
      (trace-item "path=" path)
      (trace-item "base=" base)
      (let ((meta (make-file-path base (pkglib-meta-filename)))
	    (pz (open-input-gzip-file path)))
	 (unless (input-port? pz)
	    (raise (instantiate::&io-read-error
		      (proc 'package-extract-provide)
		      (msg "Can't open file for input")
		      (obj path))))
	 (unwind-protect
	    (with-trace 10 'package-extract-info::untar
	       (let untar ()
		  (let ((h (tar-read-header pz)))
		     (if (not h)
			 #f
			 (with-access::tar-header h (type name)
			    (trace-item "header name=" name " type=" type)
			    (if (eq? type 'normal)
				(if (string=? name meta)
				    (read pz)
				    (begin
				       (tar-read-block h pz)
				       (untar)))
				(untar)))))))
	    (close-input-port pz)))))

;*---------------------------------------------------------------------*/
;*    package-companions ...                                           */
;*    -------------------------------------------------------------    */
;*    The list of interfaces that are defined in the package bundle.   */
;*---------------------------------------------------------------------*/
(define (package-companions path base)
   (with-trace 4 'package-companions
      (trace-item "path=" path)
      (trace-item "base=" base)
      (let ((pz (open-input-gzip-file path)))
	 (unless (input-port? pz)
	    (raise (instantiate::&io-read-error
		      (proc 'package-companions)
		      (msg "Can't open file for input")
		      (obj path))))
	 (unwind-protect
	    (let loop ((res '()))
	       (let ((h (tar-read-header pz)))
		  (if (not h)
		      res
		      (with-access::tar-header h (type name)
			 (trace-item "header name=" name " type=" type)
			 (tar-read-block h pz)
			 (if (and (eq? type 'normal)
				  (string-suffix-ci? (pkglib-interface-suffix)
						     name))
			     (let ((p (prefix (basename name))))
				(loop (cons (string->symbol p) res)))
			     (loop res))))))
	    (close-input-port pz)))))
