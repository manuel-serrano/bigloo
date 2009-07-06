;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/mzscheme.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 12 15:24:25 2007                          */
;*    Last change :  Sat Feb 24 09:46:31 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Planet plugin for bglpkg                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_mzscheme

   (library pkglib
	    web)

   (include "param.sch")
   
   (import  bglpkg_param
	    bglpkg_utils)
   
   (export  (planet-base-url::bstring)
	    (planet-base-url-set! ::bstring)
            (planet-plugin ::bstring ::bstring ::bstring ::bstring ::obj)
	    (mzscheme-preprocessor ::bstring)))

;*---------------------------------------------------------------------*/
;*    planet-base-url ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter planet-base-url
   "http://planet.plt-scheme.org/servlets/planet-servlet.ss")

;*---------------------------------------------------------------------*/
;*    planet-plugin ...                                                */
;*    -------------------------------------------------------------    */
;*    The plugin to install Planet's PLANETs.                          */
;*---------------------------------------------------------------------*/
(define (planet-plugin tmpdir name version path args)
   (let* ((planet-name (string-append name ".plt"))
	  (cache-dir (make-file-name (bglpkg-cache-directory) "planet"))
	  (cache-name (make-file-name cache-dir planet-name))
	  (ecache-name (make-file-path tmpdir name "cache" planet-name)))
      ;; create the cache directory
      (unless (directory? cache-dir)
	 (make-directories cache-dir))
      ;; download the planet file if not present in the cache
      ;; of the force download is set
      (cond
	 ((bglpkg-force-download)
	  (download cache-name (require->url (car args))))
	 ((file-exists? cache-name)
	  #unspecified)
	 ((file-exists? ecache-name)
	  (cp-r ecache-name cache-name))
	 (else
	  (download cache-name (require->url (car args)))))
      ;; unpack the source file
      (extract-planet cache-name (make-file-name tmpdir name))))

;*---------------------------------------------------------------------*/
;*    base64-digit ...                                                 */
;*---------------------------------------------------------------------*/
(define base64-digit
   (let ((v (make-vector 256)))
      (let loop ((n 0))
	 (unless (=fx n 256)
	    (cond	       ((<= (char->integer #\A) n (char->integer #\Z)) 
		(vector-set! v n (- n (char->integer #\A))))
	       ((<= (char->integer #\a) n (char->integer #\z)) 
		(vector-set! v n (+ 26 (- n (char->integer #\a)))))
	       ((<= (char->integer #\0) n (char->integer #\9)) 
		(vector-set! v n (+ 52 (- n (char->integer #\0)))))
	       ((= (char->integer #\+) n)
		(vector-set! v n 62))
	       ((= (char->integer #\/) n)
		(vector-set! v n 63))
	       (else
		(vector-set! v n #f)))
	    (loop (+fx n 1))))
      v))

;*---------------------------------------------------------------------*/
;*    arithmethic-shift ...                                            */
;*---------------------------------------------------------------------*/
(define (aritmetic-shift x n)
   (cond
      ((<fx n 0)
       (bit-rsh x (negfx n)))
      ((>fx n 0)
       (bit-lsh x n))
      (else
       x)))

;*---------------------------------------------------------------------*/
;*    base64-decode-stream ...                                         */
;*---------------------------------------------------------------------*/
(define (base64-decode-stream in out)
   (let loop ((waiting 0)
	      (waiting-bits 0))
      (if (>=fx waiting-bits 8)
	  (begin
	     (let ((v (aritmetic-shift waiting (- 8 waiting-bits))))
		(display (integer->char v) out))
	     (let ((waiting-bits (-fx waiting-bits 8)))
		(loop (bit-and waiting
			       (-fx (aritmetic-shift 1 waiting-bits) 1))
		      waiting-bits)))
	  (let* ((c0 (read-byte in))
		 (c (if (eof-object? c0) (char->integer #\=) c0))
		 (v (vector-ref base64-digit c)))
	     (cond
		(v (loop (+fx (bit-lsh waiting 6) v)
			 (+fx waiting-bits 6)))
		((eq? c (char->integer #\=)) #unspecified)
		(else (loop waiting waiting-bits)))))))

;*---------------------------------------------------------------------*/
;*    mzuntar ...                                                      */
;*---------------------------------------------------------------------*/
(define (mzuntar ip dest)
   (let loop ()
      (let ((kind (read ip)))
	 (unless (eof-object? kind)
	    (case kind
	       ((dir)
		(read ip)
		(loop))
	       ((file file-replace)
		(let* ((name (read ip))
		       (size (read ip))
		       (_ (read-char ip))
		       (mark (read-char ip)))
		   (unless (char=? mark #\*)
		      (error 'mzuntar "Illegal file mark" mark))
		   (let* ((fname (apply make-file-path dest (cdr name)))
			  (dir (dirname fname)))
		      (unless (file-exists? dir)
			 (make-directories dir))
		      (with-output-to-file fname
			 (lambda ()
			    (display (read-chars size ip))))))
		(loop))
	       (else
		(error 'mzuntar "unknown file tar" kind)))))))

;*---------------------------------------------------------------------*/
;*    mzunpack ...                                                     */
;*---------------------------------------------------------------------*/
(define (mzunpack ip dest)
   (unless (and (eq? #\P (read-char ip))
		(eq? #\L (read-char ip))
		(eq? #\T (read-char ip)))
      (error 'mzunpack "not an unpackable distribution archive" ip))
   (read ip)
   (read ip)
   (mzuntar ip dest))

;*---------------------------------------------------------------------*/
;*    extract-planet ...                                               */
;*---------------------------------------------------------------------*/
(define (extract-planet file dest)
   (with-input-from-file file
      (lambda ()
	 (let ((b64 (with-output-to-string
		       (lambda ()
			  (base64-decode-stream (current-input-port)
						(current-output-port))))))
	    (with-input-from-string b64
	       (lambda ()
		  (let ((ip (open-input-gzip-port (current-input-port))))
		     (mzunpack ip dest))))))))

;*---------------------------------------------------------------------*/
;*    planet-url ...                                                   */
;*---------------------------------------------------------------------*/
(define (planet-url name maj min-lo path
		    #!key
		    (min-hi "%23f")
		    (http (planet-base-url))
		    (lang 360))
   (format "~a?lang=%22~a%22;name=%22~a%22;maj=~a;min-lo=~a;min-hi=~a;path=%28%22~a%22%29"
	   http lang
	   name
	   maj
	   min-lo
	   min-hi
	   path))

;*---------------------------------------------------------------------*/
;*    require->url ...                                                 */
;*---------------------------------------------------------------------*/
(define (require->url form)
   (match-case form
      ((require (planet ?- (?path ?name ?maj ?min)))
       (planet-url name maj min path))
      (else
       (error 'require->url "Unknown require form" form))))

;*---------------------------------------------------------------------*/
;*    mzscheme-preprocessor ...                                        */
;*---------------------------------------------------------------------*/
(define (mzscheme-preprocessor body)
   (pregexp-replace*
    "[^']#[(]"
    (pregexp-replace*
     "#\\\\nul"
     (pregexp-replace* "[.] .*-> [.]" body "->")
     "#\\\\null")
    "'#("))
   
