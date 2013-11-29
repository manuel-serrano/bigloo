;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/utils.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  3 15:34:26 2007                          */
;*    Last change :  Fri Nov 29 21:13:16 2013 (serrano)                */
;*    Copyright   :  2007-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    bglpkg utils                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_utils
   
   (library pkglib
	    web)
   
   (import  bglpkg_param)
   
   (export  (rm-rf ::bstring)
	    (cp* ::bstring ::bstring)
	    (cp-r ::bstring ::bstring)
	    (download ::bstring ::bstring #!optional md5)
	    (download-package ::bstring)
	    (read-file::bstring ::bstring)
	    (tarball-untar ::bstring ::bstring #!optional file)
	    (tarball-untar-gz ::bstring ::bstring #!optional file)
	    (url?::bool ::bstring)
	    (yes-or-no?::bool ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    rm-rf ...                                                        */
;*---------------------------------------------------------------------*/
(define (rm-rf path)
   (when (file-exists? path)
      (if (directory? path)
	  (let ((files (directory->list path)))
	     (for-each (lambda (f) (rm-rf (make-file-name path f))) files)
	     (delete-directory path))
	  (delete-file path))))

;*---------------------------------------------------------------------*/
;*    cp* ...                                                          */
;*---------------------------------------------------------------------*/
(define (cp* src dest)
   (unless (or (directory? dest) (make-directory dest))
      (raise (instantiate::&io-write-error
		(proc 'cp*)
		(msg "Cannot create directory")
		(obj dest))))
   (for-each (lambda (f)
		(let ((path (make-file-name src f)))
		   (unless (directory? path)
		      (copy-file path (make-file-name dest f)))))
	     (directory->list src)))

;*---------------------------------------------------------------------*/
;*    cp-r ...                                                         */
;*---------------------------------------------------------------------*/
(define (cp-r src dest)
   (unless (or (directory? dest) (make-directory dest))
      (raise (instantiate::&io-write-error
		(proc 'cp-r)
		(msg "Cannot create directory")
		(obj dest))))
   (let loop ((src src)
	      (dest dest))
      (if (directory? src)
	  (let ((destdir (make-file-name dest (basename src))))
	     (unless (or (directory? destdir) (make-directory destdir))
		(raise (instantiate::&io-write-error
			  (proc 'cp-r)
			  (msg "Cannot create directory")
			  (obj destdir))))
	     (for-each (lambda (f)
			  (loop (make-file-name src f) destdir))
		       (directory->list src)))
	  (copy-file src (make-file-name dest (basename src))))))

;*---------------------------------------------------------------------*/
;*    read-file ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-file file)
   (unless (file-exists? file)
      (raise (instantiate::&io-error
		(proc 'read-file)
		(msg "File does not exist")
		(obj file))))
   (let* ((p (open-input-file file))
	  (s (read-string p)))
      (close-input-port p)
      s))

;*---------------------------------------------------------------------*/
;*    download-error ...                                               */
;*---------------------------------------------------------------------*/
(define (download-error line url)
   (raise (instantiate::&io-read-error
	     (proc 'download-tarball)
	     (msg line)
	     (obj url))))

;*---------------------------------------------------------------------*/
;*    download-redirect ...                                            */
;*---------------------------------------------------------------------*/
(define (download-redirect url ip)
   (let laap ((line (read-line ip)))
      (if (eof-object? line)
	  (download-error line url)
	  (let ((m (pregexp-match "[Ll]ocation: \(.*\)" line)))
	     (if (pair? m)
		 (cadr m)
		 (laap (read-line ip)))))))

;*---------------------------------------------------------------------*/
;*    download ...                                                     */
;*---------------------------------------------------------------------*/
(define (download path url #!optional md5)
   ;; download the file if it does not exist
   (with-trace 3 'download
      (trace-item "path=" path)
      (trace-item "url=" url)
      (when (or (not (file-exists? path))
		(and (bglpkg-force-download)
		     (not (string=? url ""))))
	 (unless (and (string? url) (not (string=? url "")))
	    (error 'download "No URL for tarball" path))
	 (let ((tmp (make-file-name (bglpkg-tmp-directory) (basename path))))
	    (trace-item "tmp=" tmp)
	    (unwind-protect
	       (let ((op (open-output-file tmp)))
		  (when (>=fx (pkglib-verbose) 2)
		     (print "  downloading "
			    (pkglib-color 'arg0 url)
			    " into "
			    (pkglib-color 'arg1 (basename path))))
		  (if (not (output-port? op))
		      (raise (instantiate::&io-write-error
				(proc 'download)
				(msg "Cannot open file for output")
				(obj tmp)))
		      (unwind-protect
			 (let liip ((url url))
			    (trace-item "url=" url)
			    (let ((ip (open-input-file url)))
			       (unless (input-port? ip)
				  (raise (instantiate::&io-read-error
					    (proc 'download)
					    (msg "Cannot open url")
					    (obj url))))
			       (unwind-protect
				  (let* ((line (read-line ip))
					 (status (string-split line " ")))
				     (unless (and (pair? status)
						  (pair? (cdr status)))
					(download-error line url))
				     (case (string->integer (cadr status))
					((200)
					 (let laap ()
					    (let ((e (read-line ip)))
					       (if (string=? e "")
						   (send-chars ip op)
						   (laap)))))
					((301 302 307)
					 (let ((nu (download-redirect url ip)))
					    (when (>=fx (pkglib-verbose) 3)
					       (print
						"  download redirected to "
						(pkglib-color 'arg0 nu)))
					    (liip nu)))
					(else
					 (download-error line url))))
				  (close-input-port ip))))
			 (close-output-port op)))
		  ;; check the md5
		  (when md5
		     (let ((nmd5 (md5sum-file tmp)))
			(unless (or (string=? nmd5 md5) (bglpkg-force-action))
			   (error 'download "Corrupted tarball" url))))
		  (make-directories (dirname path))
		  (unless (string=? tmp path)
		     (rename-file tmp path)))
	       (when (and (file-exists? tmp)
			  (bglpkg-delete-tmp-files)
			  (not (string=? tmp path)))
		  (delete-file tmp)))))))

;*---------------------------------------------------------------------*/
;*    download-package ...                                             */
;*---------------------------------------------------------------------*/
(define (download-package url)
   (let* ((match (pregexp-match ".*tarball=([^&]+)" url))
	  (match (or match (pregexp-match ".*/([^/]+[.]tar[.]gz)" url))))
      (unless (pair? match)
	 (error 'bglpkg "Cannot infer package name from url" url))
      (let* ((name (cadr match))
	     (path (make-file-name (bglpkg-tmp-directory) name)))
	 (download path url)
	 path)))

;*---------------------------------------------------------------------*/
;*    tar-header-type ...                                              */
;*---------------------------------------------------------------------*/
(define (tar-header-type t)
   (with-access::tar-header t (type)
      type))

;*---------------------------------------------------------------------*/
;*    tar-header-name ...                                              */
;*---------------------------------------------------------------------*/
(define (tar-header-name t)
   (with-access::tar-header t (name)
      name))

;*---------------------------------------------------------------------*/
;*    port-untar ...                                                   */
;*---------------------------------------------------------------------*/
(define (port-untar pz path base file)
   (let loop ((lst '()))
      (let ((h (tar-read-header pz)))
	 (if (not h)
	     lst
	     (case (tar-header-type h)
		((dir)
		 (let ((path (make-file-name base (tar-header-name h))))
;* 		    (rm-rf path)                                       */
		    (if (or (directory? path) (make-directory path))
			(loop lst)
			(raise (instantiate::&io-write-error
				  (proc 'tarball-untar)
				  (msg "Cannot create directory")
				  (obj path))))))
		((normal)
		 (let* ((path (make-file-name base (tar-header-name h)))
			(dir (dirname path)))
		    (when (and (file-exists? dir) (not (directory? dir)))
		       (delete-file dir))
		    (unless (file-exists? dir)
		       (make-directory dir))
		    (let ((p (open-output-file path)))
		       (if (output-port? p)
			   (unwind-protect
			      (display (tar-read-block h pz) p)
			      (close-output-port p))
			   (raise (instantiate::&io-write-error
				     (proc 'untar)
				     (msg "Cannot open file for output")
				     (obj p)))))
		    (loop (cons path lst))))
		(else
		 (raise (instantiate::&io-parse-error
			   (proc 'tarball-untar)
			   (msg (format "Illegal file type `~a'"
					(tar-header-type h)))
			   (obj path)))))))))

;*---------------------------------------------------------------------*/
;*    tarball-untar ...                                                */
;*---------------------------------------------------------------------*/
(define (tarball-untar path base #!optional file)
   (let ((pz (open-input-file path)))
      (unwind-protect
	 (port-untar pz path base file)
	 (close-input-port pz))))

;*---------------------------------------------------------------------*/
;*    tarball-untar-gz ...                                             */
;*---------------------------------------------------------------------*/
(define (tarball-untar-gz path base #!optional file)
   (let ((pz (open-input-gzip-file path)))
      (unwind-protect
         (port-untar pz path base file)
	 (close-input-port pz))))

;*---------------------------------------------------------------------*/
;*    url? ...                                                         */
;*---------------------------------------------------------------------*/
(define (url? str)
   (or (string-prefix? "http://" str) (string-prefix? "https://" str)))

;*---------------------------------------------------------------------*/
;*    yes-or-no? ...                                                   */
;*---------------------------------------------------------------------*/
(define (yes-or-no? message obj)
   (if (bglpkg-noconfirm)
       #t
       (begin
	  (cond
	     ((pair? obj)
	      (for-each (lambda (f)
			   (display f)
			   (display " "))
			obj)
	      (newline))
	     ((string? obj)
	      (print obj)))
	  (let loop ()
	     (display message)
	     (let ((c (read)))
		(if (eq? c 'y)
		    #t
		    (if (eq? c 'n)
			#f
			(loop))))))))

