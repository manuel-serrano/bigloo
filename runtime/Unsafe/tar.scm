;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/tar.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 23 17:07:04 2006                          */
;*    Last change :  Mon Jun 23 12:54:47 2014 (serrano)                */
;*    Copyright   :  2006-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Read TAR files (rfc1505)                                         */
;*    -------------------------------------------------------------    */
;*    Based on Chicken's tar implementation (Felix L. Winkelmann).     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __tar
   
   (import  __error
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r5_control_features_6_4
	    __object
	    __rgc)
   
   (use     __type
	    __bigloo
	    __param
	    __tvector
	    __structure
	    __tvector
	    __bit
	    __date
	    __os
	    __bexit
	    __thread
	    __bignum
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __evenv)
   
   (export (class tar-header
	      (name::bstring read-only)
	      (mode::long read-only)
	      (uid::long read-only)
	      (gid::long read-only)
	      (size::elong read-only)
	      (mtime::date read-only)
	      (checksum::long read-only)
	      (type::symbol read-only)
	      (linkname::bstring read-only)
	      (magic::bstring read-only)
	      (uname::bstring read-only)
	      (gname::bstring read-only)
	      (devmajor::long read-only)
	      (devminor::long read-only))
	   
	   (tar-read-header #!optional (port (current-input-port)))
	   (tar-read-block ::obj #!optional (p (current-input-port)))
	   (tar-round-up-to-record-size::long ::obj)
	   (untar ::obj #!key (directory (pwd)) file (files '()))))

;*---------------------------------------------------------------------*/
;*    tar-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (tar-error msg obj)
   (raise (instantiate::&io-parse-error
	     (proc 'tar)
	     (msg msg)
	     (obj obj))))

;*---------------------------------------------------------------------*/
;*    tar constants ...                                                */
;*---------------------------------------------------------------------*/
(define (tar-record-size) 512)
(define (tar-name-size) 100)
(define (tar-tunmlen) 32)
(define (tar-tgnmlen) 32)
(define (tar-tmagic) "ustar  ")
(define (tar-umagic) "ustar")
(define (tar-gnumagic) "GNUtar ")

;*---------------------------------------------------------------------*/
;*    tar-type-name ...                                                */
;*---------------------------------------------------------------------*/
(define (tar-type-name c)
   (case c
      ((#\null) 'oldnormal)
      ((#\0) 'normal)
      ((#\1) 'link)
      ((#\2) 'symlink)
      ((#\3) 'chr)
      ((#\4) 'blk)
      ((#\5) 'dir)
      ((#\6) 'fifo)
      ((#\7) 'contig)
      (else (tar-error "invalid file type" c))))

;*---------------------------------------------------------------------*/
;*    str->octal ...                                                   */
;*---------------------------------------------------------------------*/
(define (str->octal str #!optional (err #t))
   (or (string->integer str 8)
       (if err
	   (tar-error "invalid octal record item" str)
	   0)))

;*---------------------------------------------------------------------*/
;*    checksum ...                                                     */
;*---------------------------------------------------------------------*/
(define (checksum buf)
   (let* ((p (+fx (tar-name-size) 48))
	  (b2 (string-append
	       (substring buf 0 p)
	       "        "
	       (substring buf (+fx p 8) (string-length buf)))))
      (do ((i 0 (+fx 1 i))
	   (s 0 (+fx s (char->integer (string-ref b2 i)))))
	  ((>=fx i (tar-record-size)) s))))

;*---------------------------------------------------------------------*/
;*    tar-read-header ...                                              */
;*---------------------------------------------------------------------*/
(define (tar-read-header #!optional (port (current-input-port)))
   (unless (input-port? port)
      (bigloo-type-error 'tar-read-header "input-port" port))
   (let* ((ptr 0)
	  (data (read-chars (tar-record-size) port))
	  (len (string-length data)))
      (define (extract what size)
	 (let loop ((i 0))
	    (cond
	       ((>=fx i size)
		(tar-error
		 (format "no terminator for zero-terminated `~a' found" what)
		 size))
	       ((>=fx i len)
		(tar-error "corrupted tar file" port))
	       (else
		(let ((c (string-ref data (+fx ptr i))))
		   (if (char=? #\null c)
		       (let* ((nptr (+fx ptr i))
			      (sub (substring data ptr nptr)))
			  (set! ptr (+fx ptr size))
			  sub)
		       (loop (+fx 1 i))))))))
      (define (fetch)
	 (let ((c (string-ref data ptr)))
	    (set! ptr (+fx 1 ptr))
	    c))
      (let ((name (if (or (not (string? data)) (=fx (string-length data) 0))
		      ""
		      (extract 'name (tar-name-size)))))
	 (when (>fx (string-length name) 0)
	    (let* ((mode (str->octal (extract 'mode 8)))
		   (uid (str->octal (extract 'uid 8)))
		   (gid (str->octal (extract 'gid 8)))
		   (size (string->elong (extract 'size 12) 8))
		   (mtime (string->elong (extract 'mtime 12) 8))
		   (chksum (str->octal (extract 'chksum 8)))
		   (linkflag (fetch))
		   (linkname (extract 'linkname (tar-name-size)))
		   (magic (extract 'magic 8))
		   (uname (extract 'uname (tar-tunmlen)))
		   (gname (extract 'gname (tar-tgnmlen)))
		   (devmajor (str->octal (extract 'devmajor 8) #f))
		   (devminor (str->octal (extract 'devminor 8) #f)) 
		   (csum2 (checksum data)))
	       (cond
		  ((not (or (string=? (tar-tmagic) magic)
			    (string=? (tar-umagic) magic)
			    (string=? (tar-gnumagic) magic)))
		   (tar-error "invalid magic number" (string-for-read magic)))
		  ((not (=fx csum2 chksum))
		   (tar-error
		    (format "invalid checksum (expected: ~s)" chksum)
		    csum2))
		  (else
		   (instantiate::tar-header
		      (name name)
		      (mode mode)
		      (uid uid)
		      (gid gid)
		      (size size)
		      (mtime (seconds->date mtime))
		      (checksum chksum)
		      (type (tar-type-name linkflag))
		      (linkname linkname)
		      (magic magic)
		      (uname uname)
		      (gname gname)
		      (devmajor devmajor)
		      (devminor devminor)))))))))

;*---------------------------------------------------------------------*/
;*    tar-round-up-to-record-size ...                                  */
;*---------------------------------------------------------------------*/
(define (tar-round-up-to-record-size n)
   (if (fixnum? n)
       (*fx (tar-record-size)
	    (/fx (+fx n (-fx (tar-record-size) 1)) (tar-record-size)))
       (bigloo-type-error 'tar-round-up-to-record-size "long" n)))

;*---------------------------------------------------------------------*/
;*    tar-read-block ...                                               */
;*---------------------------------------------------------------------*/
(define (tar-read-block h #!optional (p (current-input-port)))
   (cond
      ((not (input-port? p))
       (bigloo-type-error 'tar-read-header "input-port" p))
      ((isa? h tar-header)
       (with-access::tar-header h (size name)
	  (let ((n (elong->fixnum size)))
	     (if (=fx n 0)
		 #f
		 (let ((s (read-chars n p)))
		    (if (<fx (string-length s) n)
			(error 'tar-read-block
			   "Illegal block"
			   name)
			(read-chars (-fx (tar-round-up-to-record-size n) n) p))
		    s)))))
      (else
       (bigloo-type-error 'tar-read-block "tar-header" h))))

;*---------------------------------------------------------------------*/
;*    rm-rf ...                                                        */
;*---------------------------------------------------------------------*/
(define (rm-rf path)
   (when (file-exists? path)
      (if (and (directory? path) (not (eq? (file-type path) 'link)))
	  (let ((files (directory->list path)))
	     (for-each (lambda (f) (rm-rf (make-file-name path f))) files)
	     (delete-directory path))
	  (delete-file path))))

;*---------------------------------------------------------------------*/
;*    untar ...                                                        */
;*---------------------------------------------------------------------*/
(define (untar ip #!key (directory (pwd)) file (files '()))
   (cond
      ((not (input-port? ip))
       (bigloo-type-error 'untar "input-port" ip))
      ((string? file)
       (untar-files ip (list file)))
      ((and (pair? files) (list? files) (every string? files))
       (untar-files ip files))
      (else
       (untar-directory ip (if (string? directory) directory (pwd))))))

;*---------------------------------------------------------------------*/
;*    untar-directory ...                                              */
;*---------------------------------------------------------------------*/
(define (untar-directory ip::input-port base::bstring)
   (unless (directory? base)
      (make-directories base))
   (let loop ((lst '()))
      (let ((h (tar-read-header ip)))
	 (if (not h)
	     (reverse! lst)
	     (with-access::tar-header h (type name)
		(case type
		   ((dir)
		    (let ((path (make-file-name base name)))
		       (rm-rf path)
		       (if (make-directories path)
			   (loop (cons path lst))
			   (raise
			      (instantiate::&io-error
				 (proc 'untar)
				 (msg "Cannot create directory")
				 (obj path))))))
		   ((normal)
		    (let* ((path (make-file-name base name))
			   (dir (dirname path)))
		       (when (and (file-exists? dir) (not (directory? dir)))
			  (delete-file dir))
		       (unless (file-exists? dir)
			  (make-directories dir)
			  (set! lst (cons dir lst)))
		       (with-output-to-file path
			  (lambda ()
			     (display (tar-read-block h ip))))
		       (loop (cons path lst))))
		   ((symlink)
		    (with-access::tar-header h (linkname)
		       (let ((path (make-file-name base name)))
			  (when (file-exists? path)
			     (delete-file path))
			  (make-symlink linkname path)
			  (loop (cons path lst)))))
		   (else
		    (raise
		       (instantiate::&io-parse-error
			  (proc 'untar)
			  (msg (format "Illegal file type `~a'" type))
			  (obj name))))))))))

;*---------------------------------------------------------------------*/
;*    untar-files ...                                                  */
;*---------------------------------------------------------------------*/
(define (untar-files ip::input-port files::pair-nil)
   (let loop ()
      (let ((h (tar-read-header ip)))
	 (when (isa? h tar-header)
	    (with-access::tar-header h (type name)
	       (case type
		  ((dir)
		   (loop))
		  ((normal)
		   (let ((b (tar-read-block h ip))
			 (n name))
		      (if (member n files)
			  b
			  (loop))))
		  (else
		   #f)))))))
