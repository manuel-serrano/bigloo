;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/chicken.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 12 15:24:25 2007                          */
;*    Last change :  Fri Apr 13 09:40:29 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    EGG plugin for bglpkg                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_chicken

   (library pkglib
	    web)

   (import  bglpkg_param
	    bglpkg_utils)
   
   (export  (egg-plugin ::bstring ::bstring ::bstring ::bstring ::obj)
	    (chicken-preprocessor ::bstring)))

;*---------------------------------------------------------------------*/
;*    egg-plugin ...                                                   */
;*    -------------------------------------------------------------    */
;*    The plugin to install Chicken's EGGs.                            */
;*---------------------------------------------------------------------*/
(define (egg-plugin tmpdir name version path args)
   (let* ((egg-name (string-append (basename (car args))))
	  (cache-dir (make-file-name (bglpkg-cache-directory) "egg"))
	  (cache-name (make-file-name cache-dir egg-name))
	  (ecache-name (make-file-path tmpdir name "cache" egg-name))
	  (tar-name cache-name))
      ;; create the cache directory
      (unless (directory? cache-dir) (make-directories cache-dir))
      ;; download the egg file if not present in the cache
      ;; of the force download is set
      (cond
	 ((bglpkg-force-download)
	  (download cache-name (car args)))
	 ((file-exists? cache-name)
	  #unspecified)
	 ((file-exists? ecache-name)
	  (set! tar-name ecache-name))
	 (else
	  (download cache-name (car args))))
      ;; untar the source file
      (tarball-untar-gz tar-name
			(make-file-name tmpdir name)
			egg-name)))

;*---------------------------------------------------------------------*/
;*    *chicken-end-of-c-grammar* ...                                   */
;*---------------------------------------------------------------------*/
(define *chicken-end-of-c-grammar*
   (regular-grammar ()
      ((bol (: "<#" (* all)))
       (display (the-string)))
      ((+ (out #\< #\Newline))
       (display (the-string))
       (ignore))
      (#\<
       (display #\<)
       (ignore))
      (#\Newline
       (display "\n;; ")
       (ignore))
      (else
       (raise (instantiate::&io-parse-error
		 (proc 'chicken-preprocessor)
		 (msg "Premature end of file")
		 (obj "#>"))))))
		 
;*---------------------------------------------------------------------*/
;*    *chicken-grammar* ...                                            */
;*---------------------------------------------------------------------*/
(define *chicken-grammar*
   (regular-grammar ((letter (out #\space #\return #\tab #\newline #\#)))
      ((bol (: "#>" (* all)))
       (display ";; ")
       (display (the-string))
       (read/rp *chicken-end-of-c-grammar* (the-port))
       (ignore))
      ((+ (out #\# #\())
       (display (the-string))
       (ignore))
      ((: #\( (out #\: #\#))
       (display (the-string))
       (ignore))
      ("(:optional"
       (display "(_:optional")
       (ignore))
      ((: "##" (+ letter) "#" (+ letter))
       (display (string-replace! (the-string) #\# #\_))
       (ignore))
      ((or #\: #\# #\()
       (display (the-character))
       (ignore))
      (else
       (the-failure))))

;*---------------------------------------------------------------------*/
;*    chicken-preprocessor ...                                         */
;*---------------------------------------------------------------------*/
(define (chicken-preprocessor body)
   (with-input-from-string body
      (lambda ()
	 (with-output-to-string
	    (lambda ()
	       (read/rp *chicken-grammar* (current-input-port)))))))
