;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/m3u.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul 30 15:30:22 2005                          */
;*    Last change :  Wed Sep  9 19:05:08 2009 (serrano)                */
;*    Copyright   :  2005-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    M3U (music playlist) handling                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-m3u

   (export (read-m3u::pair-nil ::input-port)
	   (write-m3u ::pair-nil ::output-port)))

;*---------------------------------------------------------------------*/
;*    read-m3u ...                                                     */
;*---------------------------------------------------------------------*/
(define (read-m3u ip::input-port)
   ;; the header
   (read/rp *m3u-header-grammar* ip)
   ;; the body
   (let loop ((ser '()))
      (let ((song (read/rp *m3u-song-grammar* ip)))
	 (if (eof-object? song)
	     (reverse! ser)
	     (loop (cons song ser))))))

;*---------------------------------------------------------------------*/
;*    write-m3u ...                                                    */
;*---------------------------------------------------------------------*/
(define (write-m3u m3u op::output-port)
   (define (write-m3u-song s)
      (match-case s
	 ((?file ?name (and (? integer?) ?len))
	  (fprint op "#EXTINF:" len "," name "\n" file))
	 ((?file ?name)
	  (fprint op "#EXTINF:0," name "\n" file))
	 (else
	  (raise
	   (instantiate::&io-write-error
	      (proc 'write-m3u)
	      (msg "Illegal m3u format")
	      (obj s))))))
   (fprint op "#EXTM3U")
   (for-each write-m3u-song m3u))
   
;*---------------------------------------------------------------------*/
;*    *m3u-header-grammar* ...                                         */
;*---------------------------------------------------------------------*/
(define *m3u-header-grammar*
   (regular-grammar ()
      ((: (or "#EXTM3U" "#Extended M3U") (? #\Return) #\Newline)
       #t)
      (else
       (raise
	(instantiate::&io-parse-error
	   (proc 'read-m3u)
	   (msg "Illegal header")
	   (obj (the-failure))
	   (fname (input-port-name (the-port)))
	   (location (input-port-position (the-port))))))))

;*---------------------------------------------------------------------*/
;*    error-msg ...                                                    */
;*---------------------------------------------------------------------*/
(define (error-msg c port)
   (string-append "{" (string c) "}" (read-line port)))

;*---------------------------------------------------------------------*/
;*    *m3u-song-grammar* ...                                           */
;*---------------------------------------------------------------------*/
(define *m3u-song-grammar*
   (regular-grammar ()
      ("#EXTINF:"
       (let ((len (read/rp *m3u-song-length-grammar* (the-port))))
	  (if (integer? len)
	      (let* ((name (read/rp *m3u-song-name-grammar* (the-port) "name"))
		     (file (read/rp *m3u-song-name-grammar* (the-port) "file")))
		 (list file name len))
	      (let* ((name (read/rp *m3u-song-name-grammar* (the-port) "name"))
		     (file (read/rp *m3u-song-name-grammar* (the-port) "file")))
		 (list file name -1)))))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (raise
	       (instantiate::&io-parse-error
		  (proc 'read-m3u)
		  (msg "Illegal song")
		  (obj (error-msg (the-failure) (the-port)))
		  (fname (input-port-name (the-port)))
		  (location (input-port-position (the-port))))))))))

;*---------------------------------------------------------------------*/
;*    *m3u-song-length-grammar* ...                                    */
;*---------------------------------------------------------------------*/
(define *m3u-song-length-grammar*
   (regular-grammar ()
      ((: (+ digit) #\,)
       (string->integer (the-substring 0 -1)))
      (else
       (let ((c (the-failure)))
	  (unless (eof-object? c)
	     (rgc-buffer-unget-char (the-port) (char->integer c)))
	  #f))))
 
;*---------------------------------------------------------------------*/
;*    *m3u-song-name-grammar* ...                                      */
;*---------------------------------------------------------------------*/
(define *m3u-song-name-grammar*
   (regular-grammar (kind)
      ((: (+ all) "\r\n")
       (the-substring 0 -2))
      ((: (+ all) "\n")
       (the-substring 0 -1))
      (else
       (raise
	(instantiate::&io-parse-error
	   (proc 'read-m3u)
	   (msg (format "Illegal song name ~a" kind))
	   (obj (error-msg (the-failure) (the-port)))
	   (fname (input-port-name (the-port)))
	   (location (input-port-position (the-port))))))))
