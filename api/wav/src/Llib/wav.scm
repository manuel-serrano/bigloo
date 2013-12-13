;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/wav/src/Llib/wav.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:30:32 2011                          */
;*    Last change :  Fri Dec 13 12:51:19 2013 (serrano)                */
;*    Copyright   :  2011-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo WAV facilities                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __wav_wav

   (option (set! *dlopen-init-gc* #t))
   
   (library multimedia)

   (export (class wavinfo::musicinfo
	      (audiofmt::symbol (default 'unknown))
	      (byterate::int (default -1))
	      (blockalign::int (default -1))
	      (chunksize::elong (default -1)))
   
	   (class &wav-error::&error)

	   (wav-header-size::elong)
	   (wav-parse-header ::bstring)))

;*---------------------------------------------------------------------*/
;*    wav-header-size ...                                              */
;*---------------------------------------------------------------------*/
(define (wav-header-size::elong) #e44)

;*---------------------------------------------------------------------*/
;*    integer->audio-format ...                                        */
;*---------------------------------------------------------------------*/
(define (integer->audio-format i)
   (case i
      ((#x1) 'pcm)
      (else 'unknown)))

;*---------------------------------------------------------------------*/
;*    read-wav-musicinfo ...                                           */
;*---------------------------------------------------------------------*/
(define (read-wav-musicinfo mm::mmap)
   (when (> (mmap-length mm) (wav-header-size))
      (with-handler
	 (lambda (e) #f)
	 (wav-parse-header (mmap-substring mm 0 (wav-header-size))))))

;*---------------------------------------------------------------------*/
;*    wav-parse-header ...                                             */
;*---------------------------------------------------------------------*/
(define (wav-parse-header buffer::bstring)
   
   (define (wav-error msg offset)
      (raise (instantiate::&wav-error
		(proc "wav-parse-header")
		(msg (format msg offset))
		(obj buffer))))
   
   (define (read-int16::int offset)
      (let ((l (char->integer (string-ref buffer offset)))
	    (h (char->integer (string-ref buffer (+fx offset 1)))))
	 (+fx l (bit-lsh h 8))))
   
   (define (read-int32::elong offset)
      (let ((l::elong (fixnum->elong (read-int16 offset)))
	    (h::elong (fixnum->elong (read-int16 (+fx offset 2)))))
	 (+elong l (bit-lshelong h 16))))
   
   ;; chunk descriptor
   (unless (substring-at? buffer "RIFF" 0)
      (wav-error "Illegal wav header at offset ~a" 0))
   ;; chunk size
   (let ((chunksize (read-int32 4)))
      (unless (>elong chunksize 0)
	 (wav-error "Bad wav header chunksize: ~a" chunksize))
      ;; WAVE tag
      (unless (substring-at? buffer "WAVE" 8)
	 (wav-error "Illegal wav header at offset ~a" 8))
      ;; fmt tag
      (unless (substring-at? buffer "fmt " 12)
	 (wav-error "Illegal wav header at offset ~a" 12))
      (let ((ssize::elong (read-int32 16)))
	 (unless (>elong ssize 0)
	    (wav-error "Bad wav header chunksize: ~a" ssize)))
      (let* ((audiofmt (read-int16 20))
	     (channels (read-int16 22))
	     (samplerate (read-int32 24))
	     (byterate (read-int32 28))
	     (blockalign (read-int16 32))
	     (bps (read-int16 34))
	     (totalsample (/elong chunksize blockalign))
	     (duration (/elong totalsample samplerate)))
	 ;; data tag
	 (unless (substring-at? buffer "data" 36)
	    (wav-error "Illegal wav header at offset ~a" 36))
	 (let ((csize (read-int32 40)))
	    (instantiate::wavinfo
	       (format "wav")
	       (duration (elong->fixnum duration))
	       (audiofmt (integer->audio-format audiofmt))
	       (channels channels)
	       (samplerate (elong->fixnum samplerate))
	       (byterate (elong->fixnum byterate))
	       (blockalign (elong->fixnum blockalign))
	       (bps bps)
	       (chunksize chunksize))))))

;*---------------------------------------------------------------------*/
;*    register the wav parser for file-musicinfo.                      */
;*---------------------------------------------------------------------*/
(register-musicinfo-reader! read-wav-musicinfo)
