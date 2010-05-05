;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/mpc.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul 30 16:23:00 2005                          */
;*    Last change :  Thu Mar 18 06:42:07 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MPC implementation                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-mpc

   (import __multimedia-music
	   __multimedia-music-event-loop)

   (export (class mpc::music
	      
	      (hello read-only (default #f))
	      (host read-only (default "localhost"))
	      (port read-only (default 6600))
	      (timeout read-only (default 1000893))
	      (prefix (default #f))

	      (%closed::bool (default #f))
	      (%mpd (default #f))
	      (%socket (default #f)))))

#;(define-expander with-lock
   (lambda (x e)
      (match-case x
	 ((?- ?mutex ?proc)
	  (match-case (cer x)
	     ((at ?name ?pos)
	      (e `(unwind-protect
		     (begin
			(tprint ">>> with-lock(mpc.scm): " ,mutex "=" (mutex-state ,mutex) " pos: " ,pos " " (current-thread))
			((@ with-lock  __thread) ,mutex ,proc))
		     (tprint "<<< with-lock: " ,mutex "=" (mutex-state ,mutex) " pos: " ,pos  " " (current-thread)))
		 e)))))))

;*---------------------------------------------------------------------*/
;*    with-timed-lock ...                                              */
;*---------------------------------------------------------------------*/
(define (with-timed-lock mutex thunk)
   (when (mutex-lock! mutex 1000)
      (unwind-protect
	 (thunk)
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    mutex-locked? ...                                                */
;*---------------------------------------------------------------------*/
(define (mutex-locked? m)
   (let ((th (current-thread))
	 (st (mutex-state m)))
      (or (and (not th) (eq? st #unspecified))
	  (and (thread? th) (eq? (mutex-state m) th)))))

;*---------------------------------------------------------------------*/
;*    mpc-skip-line ...                                                */
;*---------------------------------------------------------------------*/
(define (mpc-skip-line ip)
   (let ((grammar (regular-grammar ((xall (or (out #\Newline #\Return) #a000)))
		     ((: (+ xall) (or #\Newline #\Return))
		      #t)
		     ((: (+ xall) #\Return #\Newline)
		      #t)
		     ((+ xall)
		      #t)
		     ((or #\Newline #\Return (: #\Return #\Newline))
		      #t)
		     (else
		      (the-failure)))))
      (read/rp grammar ip)))

;*---------------------------------------------------------------------*/
;*    music-reset-error! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (music-reset-error! mpc::mpc)
   (with-access::mpc mpc (%closed %socket)
      (unless %closed
	 ;; closing the socket is the easiest way to reset the error
	 ;; and flush the pending outputs
	 (socket-close %socket)
	 (set! %socket #f)
	 (mpc-cmd mpc "clearerror" ok-parser))))

;*---------------------------------------------------------------------*/
;*    set-error! ...                                                   */
;*---------------------------------------------------------------------*/
(define (set-error! mpc status e)
   (with-access::mpc mpc (%socket)
      (with-access::musicstatus status (err state)
	 (set! state 'error)
	 (cond
	    ((&error? e)
	     (set! err (&error-msg e)))
	    ((&exception? e)
	     (set! err (with-error-to-string (lambda () (exception-notify e)))))
	    (else
	     (set! err e)))
	 (when (socket? %socket)
	    (socket-close %socket)
	    (set! %socket #f)))))

;*---------------------------------------------------------------------*/
;*    exec ...                                                         */
;*---------------------------------------------------------------------*/
(define (exec mpc::mpc cmd::bstring)
   (with-access::mpc mpc (%mutex)
      [assert (cmd mpc) (mutex-locked? (mpc-%mutex mpc))]
      (let ((po (socket-output (mpc-%socket mpc))))
	 (display-string cmd po)
	 (newline po)
	 (flush-output-port po)
	 #t)))

;*---------------------------------------------------------------------*/
;*    mpc-cmd ...                                                      */
;*    -------------------------------------------------------------    */
;*    This function may raise exceptions that must be handled          */
;*    by the called.                                                   */
;*    -------------------------------------------------------------    */
;*    The function mpc-cmd execute an mpc commands. If the connection  */
;*    is not established with the server, it first creates it.         */
;*    If the connection is closed by the server, it tries to re-open   */
;*    it first.                                                        */
;*---------------------------------------------------------------------*/
(define (mpc-cmd mpc::mpc cmd::bstring parser::procedure)
   
   (define (ack-parser mpc)
      (with-access::mpc mpc (hello %socket %mpd %status)
	 (when (string? hello)
	    (let ((po (socket-output %socket)))
	       (display hello po)
	       (flush-output-port po)))
	 (let ((pi (socket-input %socket)))
	    (let ((l (read-line pi)))
	       (if (and (string? l) (substring-at? l "OK MPD" 0))
		   (set! %mpd (substring l 6 (string-length l)))
		   (set-error! mpc
			       %status
			       (format "Illegal MPC acknowledge: ~a" l)))))))
   
   (define (init-socket! mpc)
      (with-access::mpc mpc (host port timeout %socket)
	 (set! %socket (make-client-socket host port :timeout timeout))
	 (input-port-timeout-set! (socket-input %socket) timeout)))

   (with-access::mpc mpc (%socket %status host port)
      [assert (cmd mpc) (mutex-locked? (mpc-%mutex mpc))]
      ;; the player is already closed
      (unless (music-closed? mpc)
	 (set! _c_ (+fx _c_ 1))
	 (let retry ((count 3))
	    ;; the player is not connected yet
	    (unless %socket
	       (with-handler
		  (lambda (e)
		     (tprint "init-socket! error (" _c_ "), count=" count " cmd=" cmd " -> " e)
		     (raise (instantiate::&io-error
			       (proc 'mpc-cmpd)
			       (msg (format "~a:~a unreachable" host port))
			       (obj mpc))))
		  '(tprint "init-socket... (" _c_ ") cmd=" cmd)
		  (init-socket! mpc))
	       (with-handler
		  (lambda (e)
		     (tprint "ack-parser error (" _c_ "), count=" count " cmd=" cmd " -> " e)
		     (raise e))
		  '(tprint "ack-parser...(" _c_ ") cmd=" cmd)
		  (ack-parser mpc)))
	    ;; we can now emit our command if we have a socket
	    ;; (still possibly closed by the server)
	    (when %socket
	       '(tprint "exec...(" _c_ ") cmd=" cmd " time=" (current-date) " " (current-thread))
	       (let ((v (with-handler
			   (lambda (e)
			      (tprint "exec error (" _c_ "), count=" count " cmd=" cmd " -> " e)
			      (if (>fx count 0)
				  (begin
				     (set-error! mpc %status e)
				     (retry (-fx count 1)))
				  (raise e)))
			   (exec mpc cmd))))
		  '(tprint "parser...(" _c_ ") cmd=" cmd)
		  (parser mpc)))))))

(define _c_ 0)

;*---------------------------------------------------------------------*/
;*    parse-error-msg ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-error-msg fail port)
   (let ((s (cond
	       ((char? fail) (string fail))
	       ((eof-object? fail) "#<eof-object>")
	       (else (with-output-to-string (lambda () (display fail))))))
	 (l (read-line port)))
      (if (string? l)
	  (string-append "{" s "}" l)
	  s)))

;*---------------------------------------------------------------------*/
;*    *mpc-string-grammar* ...                                         */
;*---------------------------------------------------------------------*/
(define *mpc-string-grammar*
   (regular-grammar ()
      ((* blank)
       (ignore))
      ((: (out " \r\n") (* (out "\n")) #\Newline)
       (the-substring 0 -1))
      (else
       (raise
	(instantiate::&io-parse-error
	   (proc 'mpc)
	   (msg "Illegal string value")
	   (obj (parse-error-msg (the-failure) (the-port))))))))

;*---------------------------------------------------------------------*/
;*    *mpc-ignore-grammar* ...                                         */
;*---------------------------------------------------------------------*/
(define *mpc-ignore-grammar*
   (regular-grammar ()
      ((* blank)
       (ignore))
      ((: (* all) #\Newline)
       #unspecified)
      (else
       (raise
	(instantiate::&io-parse-error
	   (proc 'mpc)
	   (msg "Illegal ignore value")
	   (obj (parse-error-msg (the-failure) (the-port))))))))

;*---------------------------------------------------------------------*/
;*    *mpc-integer-grammar* ...                                        */
;*---------------------------------------------------------------------*/
(define *mpc-integer-grammar*
   (regular-grammar ()
      ((* blank)
       (ignore))
      ((: (+ digit) #\Newline)
       (the-fixnum))
      (else
       (raise
	(instantiate::&io-parse-error
	   (proc 'mpc)
	   (msg "Illegal integer")
	   (obj (parse-error-msg (the-failure) (the-port))))))))

;*---------------------------------------------------------------------*/
;*    ok-parser ...                                                    */
;*---------------------------------------------------------------------*/
(define (ok-parser mpc::mpc)
   [assert (mpc) (mutex-locked? (mpc-%mutex mpc))]
   (let ((l (read-line (socket-input (mpc-%socket mpc)))))
      (and (string? l) (substring-at? l "OK" 0))))
   
;*---------------------------------------------------------------------*/
;*    music-close ::mpc ...                                            */
;*---------------------------------------------------------------------*/
(define-method (music-close mpc::mpc)
   ;; close the associated socket
   (with-access::mpc mpc (%socket %closed %mutex)
      (let ((doclose (with-lock %mutex
			(lambda ()
			   (unless (music-closed? mpc)
			      ;; mark that we are closed
			      (set! %closed #t)
			      #t)))))
	 (when doclose
	    (call-next-method)
	    (with-lock %mutex
	       (lambda ()
		  (when (socket? %socket)
		     ;; tell the the server that we are down
		     (exec mpc "close")
		     ;; close the socket
		     (socket-close %socket)
		     (set! %socket #f))))))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::mpc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (music-closed? mpc::mpc)
   (with-access::mpc mpc (%closed)
      %closed))
   
;*---------------------------------------------------------------------*/
;*    music-reset! ::mpc ...                                           */
;*---------------------------------------------------------------------*/
(define-method (music-reset! mpc::mpc)
   (with-access::mpc mpc (%socket %mutex)
      (with-lock %mutex
	 (lambda ()
	    (when (socket? %socket)
	       (socket-close %socket)
	       (set! %socket #f))))))
   
;*---------------------------------------------------------------------*/
;*    music-playlist-get ::mpc ...                                     */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-get mpc::mpc)
   
   (define (qualify-path s)
      (if (or (substring-at? s "http:" 0) (substring-at? s "https:" 0))
	  s
	  (string-append (mpc-prefix mpc) s)))
   
   (define mpc-playlist-grammar
      (regular-grammar ()
	 ((: (+ digit) #\:)
	  (read/rp *mpc-string-grammar* (the-port)))
	 ("OK\n"
	  'ok)
	 (else
	  ;; there is an error, flush out everything and raises the error
	  (let ((exc (instantiate::&io-parse-error
			(proc 'mpc-playlist-get)
			(msg "Illegal playlist value")
			(obj (parse-error-msg (the-failure) (the-port))))))
	     (let loop ()
		(let ((l (read-line (the-port))))
		   (if (or (string-prefix? "OK" l) (string-prefix? "ACK" l))
		       (raise exc)
		       (loop))))))))
   
   (define (playlist-parser mpc)
      (let ((ip (socket-input (mpc-%socket mpc))))
	 (let loop ((ser '()))
	    (let ((l (read/rp mpc-playlist-grammar ip)))
	       (if (eq? l 'ok)
		   (map! qualify-path (reverse! ser))
		   (loop (cons l ser)))))))
   
   (with-lock (mpc-%mutex mpc)
      (lambda ()
	 (with-handler
	    (lambda (e)
	       (set-error! mpc (mpc-%status mpc) e)
	       '())
	    (mpc-cmd mpc "playlist" playlist-parser)))))
 
;*---------------------------------------------------------------------*/
;*    music-playlist-add! ::mpc ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-add! mpc::mpc s)
   (call-next-method)
   (with-access::mpc mpc (%mutex prefix)
      (with-timed-lock (mpc-%mutex mpc)
	 (lambda ()
	    (let ((s (if (and (string? prefix) (substring-at? s prefix 0))
			 (substring s (string-length prefix) (string-length s))
			 s)))
	       (mpc-cmd mpc (string-append "add \"" s "\"") ok-parser))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::mpc ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! mpc::mpc n)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (mpc-cmd mpc (string-append "delete " (integer->string n)) ok-parser))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::mpc ...                                  */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! mpc::mpc)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (mpc-cmd mpc "clear" ok-parser))))

;*---------------------------------------------------------------------*/
;*    music-update-status-sans-lock! ...                               */
;*---------------------------------------------------------------------*/
(define (music-update-status-sans-lock! mpc::mpc status::musicstatus)

   (define mpc-khz-grammar
      (regular-grammar ()
	 ((+ (in " \t"))
	  (ignore))
	 ((+ digit)
	  (let ((n (the-fixnum)))
	     (read/rp *mpc-ignore-grammar* (the-port))
	     n))
	 (else
	  (raise
	   (instantiate::&io-parse-error
	      (proc 'mpc-song)
	      (msg "Illegal khz")
	      (obj (parse-error-msg (the-failure) (the-port))))))))

   (define mpc-symbol-grammar
      (regular-grammar ()
	 ((* blank)
	  (ignore))
	 ((: (out " \r\n") (+ (out "\n")) #\Newline)
	  (the-subsymbol 0 -1))
	 (else
	  (raise
	   (instantiate::&io-parse-error
	      (proc 'mpc)
	      (msg "Illegal string value")
	      (obj (parse-error-msg (the-failure) (the-port))))))))

   (define mpc-value-grammar
      (regular-grammar ()
	 ((* blank)
	  (ignore))
	 ((: (* all) #\Newline)
	  (the-substring 0 -1))
	 (else
	  (raise
	   (instantiate::&io-parse-error
	      (proc 'mpc)
	      (msg "Illegal ignore value")
	      (obj (parse-error-msg (the-failure) (the-port))))))))

   (define mpc-time-grammar
      (regular-grammar (status)
	 ((* blank)
	  (ignore))
	 ((: (+ digit) ":")
	  (let* ((tm (the-fixnum))
		 (du (read/rp *mpc-integer-grammar* (the-port))))
	     (musicstatus-songpos-set! status tm)
	     (musicstatus-songlength-set! status du)))
	 (else
	  (raise
	   (instantiate::&io-parse-error
	      (proc 'mpc)
	      (msg "Illegal integer")
	      (obj (parse-error-msg (the-failure) (the-port))))))))   

   (define mpc-status-grammar
      (regular-grammar (status)
	 ("OK\n"
	  status)
	 ((: "s" (+ alpha) "g" #\:)
	  ;; song
	  (let ((k (the-keyword)))
	     (if (eq? k song:)
		 (let ((song (read/rp *mpc-integer-grammar* (the-port))))
		    (musicstatus-song-set! status song))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "s" (+ alpha) "d" #\:)
	  ;; songid
	  (let ((k (the-keyword)))
	     (if (eq? k songid:)
		 (let ((songid (read/rp *mpc-integer-grammar* (the-port))))
		    (musicstatus-songid-set! status songid))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "st" (+ alpha) #\:)
	  ;; state
	  (let ((k (the-keyword)))
	     (if (eq? k state:)
		 (let ((state (read/rp mpc-symbol-grammar (the-port))))
		    (musicstatus-state-set! status state))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "t" (+ alpha) #\:)
	  ;; time
	  (let ((k (the-keyword)))
	     (if (eq? k time:)
		 (read/rp mpc-time-grammar (the-port) status)
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "v" (+ alpha) #\:)
	  ;; volume
	  (let ((k (the-keyword)))
	     (if (eq? k volume:)
		 (let ((volume (read/rp *mpc-integer-grammar* (the-port)))) 
		    (musicstatus-volume-set! status volume))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "p" (+ alpha) "h" #\:)
	  ;; playlistlength
	  (let ((k (the-keyword)))
	     (if (eq? k playlistlength:)
		 (let ((plen (read/rp *mpc-integer-grammar* (the-port))))
		    (musicstatus-playlistlength-set! status plen))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "p" (+ alpha) "t" #\:)
	  ;; playlist
	  (let ((k (the-keyword)))
	     (if (eq? k playlist:)
		 (let ((pid (read/rp *mpc-integer-grammar* (the-port))))
		    (musicstatus-playlistid-set! status pid))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "e" (+ alpha) #\:)
	  ;; error
	  (let ((k (the-keyword)))
	     (if (eq? k error:)
		 (let ((err (read/rp mpc-value-grammar (the-port))))
		    (musicstatus-err-set! status err))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "b" (+ alpha) #\:)
	  ;; bitrate
	  (let ((k (the-keyword)))
	     (if (eq? k bitrate:)
		 (let ((bitrate (read (the-port))))
		    (musicstatus-bitrate-set! status bitrate))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "a" (+ alpha) #\:)
	  ;; audio
	  (let ((k (the-keyword)))
	     (if (eq? k audio:)
		 (let ((khz (read/rp mpc-khz-grammar (the-port))))
		    (musicstatus-khz-set! status khz))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "x" (+ alpha) #\:)
	  ;; xfade
	  (let ((k (the-keyword)))
	     (if (eq? k xfade:)
		 (let ((xfade (read/rp *mpc-integer-grammar* (the-port))))
		    (musicstatus-xfade-set! status xfade))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "ra" (+ alpha) #\:)
	  ;; random
	  (let ((k (the-keyword)))
	     (if (eq? k random:)
		 (let ((random (read/rp *mpc-integer-grammar* (the-port))))
		    (musicstatus-random-set! status (=fx random 1)))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: "re" (+ alpha) #\:)
	  ;; repeat
	  (let ((k (the-keyword)))
	     (if (eq? k repeat:)
		 (let ((repeat (read/rp *mpc-integer-grammar* (the-port))))
		    (musicstatus-repeat-set! status (=fx repeat 1)))
		 (read/rp *mpc-ignore-grammar* (the-port)))
	     (ignore)))
	 ((: (+ alpha) #\:)
	  (read/rp *mpc-ignore-grammar* (the-port))
	  (ignore))
	 ((: "ACK " (+ all))
	  (musicstatus-err-set! status (the-string))
	  (ignore))
	 ((+ #\Newline)
	  (ignore))
	 (else
	  ;; a parsing error
	  (musicstatus-song-set! status 0)
	  (musicstatus-songid-set! status 0)
	  (musicstatus-playlistlength-set! status 0)
	  (musicstatus-songpos-set! status 0)
	  (musicstatus-songlength-set! status 0)
	  (musicstatus-khz-set! status 0)
	  (musicstatus-bitrate-set! status 0)
	  (if (eof-object? (the-failure))
	      (begin
		 (musicstatus-state-set! status 'eof)
		 (unless (string? (musicstatus-err status))
		    (musicstatus-err-set! status "Illegal end-of-file")))
	      (begin
		 (musicstatus-state-set! status 'error)
		 (unless (string? (musicstatus-err status))
		    (musicstatus-err-set! status "Parsing error")))))))
   
   (define (parse ip status)
      (with-access::musicstatus status (songpos songlength bitrate khz err)
	 ;; reset the properties that are optional in mpd status report
	 (set! songpos 0)
	 (set! songlength 0)
	 (set! bitrate 0)
	 (set! khz 0)
	 (set! err #f))
      (read/rp mpc-status-grammar ip status)
      status)
   
   (define (status-parser mpc)
      (with-access::mpc mpc (%socket)
	 ;; parse the result of the status command
	 (let ((status (parse (socket-input %socket) status)))
	    (with-access::musicstatus status (state)
	       (when (eq? state 'eof)
		  (socket-close %socket)
		  (set! %socket #f)
		  #t)))))

   ;; don't protect with a handler because it is assumed that the
   ;; handler has been set by the caller
   (mpc-cmd mpc "status" status-parser))

;*---------------------------------------------------------------------*/
;*    music-update-status! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (music-update-status! mpc::mpc status::musicstatus)
   (with-lock (mpc-%mutex mpc)
      (lambda ()
	 (with-access::musicstatus status (state)
	    (unless (eq? state 'eof)
	       (with-handler
		  (lambda (e)
		     (set-error! mpc status e))
		  ;; first ping to check if the connection is still open
		  (let loop ((retry #t))
		     (cond
			((mpc-cmd mpc "ping" ok-parser)
			 ;; it is still open
			 (music-update-status-sans-lock! mpc status))
			(retry
			 ;; it is apparently closed, try to re-open it once
			 (music-reset-error! mpc)
			 (loop #f))))))))))

;*---------------------------------------------------------------------*/
;*    music-status ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (music-status mpc::mpc)
   (with-access::music mpc (%status)
      (music-update-status! mpc %status)
      %status))

;*---------------------------------------------------------------------*/
;*    music-song ::mpc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (music-song mpc::mpc)

   (define mpc-song-grammar
      (regular-grammar ()
	 ((: (+ alpha) #\:)
	  (let ((k (the-keyword)))
	     (case k
		((file:)
		 (cons k (read/rp *mpc-string-grammar* (the-port))))
		((Title:)
		 (cons title: (read/rp *mpc-string-grammar* (the-port))))
		((Name:)
		 (cons name: (read/rp *mpc-string-grammar* (the-port))))
		((Id:)
		 (cons id: (read/rp *mpc-integer-grammar* (the-port))))
		((Pos:)
		 (cons pos: (read/rp *mpc-integer-grammar* (the-port))))
		((Track:)
		 (cons track: (read/rp *mpc-integer-grammar* (the-port))))
		((Time:)
		 (cons time: (read/rp *mpc-integer-grammar* (the-port))))
		(else
		 (read/rp *mpc-ignore-grammar* (the-port))
		 (ignore)))))
	 ("OK\n"
	  'ok)
	 (else
	  (raise
	   (instantiate::&io-parse-error
	      (proc 'mpc-song)
	      (msg "Illegal song status")
	      (obj (parse-error-msg (the-failure) (the-port))))))))
   
   (define (music-song-parser mpc)
      (let ((ip (socket-input (mpc-%socket mpc))))
	 (let loop ((ser '()))
	    (let ((l (read/rp mpc-song-grammar ip)))
	       (if (eq? l 'ok)
		   (reverse! ser)
		   (loop (cons l ser)))))))
   
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (with-handler
	    (lambda (e)
	       (set-error! mpc (mpc-%status mpc) e)
	       0)
	    (mpc-cmd mpc "currentsong" music-song-parser)))))

;*---------------------------------------------------------------------*/
;*    music-time ::mpc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (music-songpos::int mpc::mpc)
   
   (define mpc-ellapsed-grammar
      (regular-grammar ()
	 ((* blank)
	  (ignore))
	 ((: (+ digit) ":")
	  (let ((v (the-fixnum)))
	     (read/rp *mpc-integer-grammar* (the-port))
	     v))
	 (else
	  (raise
	   (instantiate::&io-parse-error
	      (proc 'mpc)
	      (msg "Illegal integer")
	      (obj (parse-error-msg (the-failure) (the-port))))))))

   (define mpc-status-time-grammar
      (regular-grammar ()
	 ((: (+ alpha) #\:)
	  (let ((k (the-keyword)))
	     (case k
		((time:)
		 (read/rp mpc-ellapsed-grammar (the-port)))
		(else
		 (read/rp *mpc-ignore-grammar* (the-port))
		 (ignore)))))
	 ("OK\n"
	  'ok)
	 (else
	  (raise
	   (instantiate::&io-parse-error
	      (proc 'mpc-time)
	      (msg "Illegal status")
	      (obj (parse-error-msg (the-failure) (the-port))))))))

   (define (music-time-parser mpc)
      (let ((ip (socket-input (mpc-%socket mpc))))
	 (let loop ((tm 0))
	    (let ((l (read/rp mpc-status-time-grammar ip)))
	       (cond
		  ((integer? l)
		   (loop l))
		  ((eq? l 'ok)
		   tm)
		  (else
		   (loop tm)))))))
   
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (with-handler
	    (lambda (e)
	       (set-error! mpc (mpc-%status mpc) e)
	       0)
	    (mpc-cmd mpc "status" music-time-parser)))))

;*---------------------------------------------------------------------*/
;*    music-meta ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-meta mpc::mpc)

   (define mpc-meta-grammar
      (regular-grammar ()
	 ((bol (: (+ (out #\: #\Newline)) #\:))
	  (let* ((k (the-substring 0 -1))
		 (s (string->symbol (string-downcase! k)))
		 (v (read/rp *mpc-string-grammar* (the-port))))
	     (cons (cons s v) (ignore))))
	 ("OK\n"
	  '())))
	 
   (define (currentsong-parser mpc)
      (let ((ip (socket-input (mpc-%socket mpc))))
	 (read/rp mpc-meta-grammar ip)))
   
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (with-handler
	    (lambda (e)
	       (tprint "e=" e)
	       (set-error! mpc (mpc-%status mpc) e)
	       '())
	    (mpc-cmd mpc "currentsong" currentsong-parser)))))

;*---------------------------------------------------------------------*/
;*    music-play ::mpc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (music-play mpc::mpc . song)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (let ((cmd (if (null? song) "play" (format "play ~a" (car song)))))
	    (mpc-cmd mpc cmd ok-parser)))))

;*---------------------------------------------------------------------*/
;*    music-seek ::mpc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (music-seek mpc::mpc ntime::obj . nsong)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (with-handler
	    (lambda (e)
	       (set-error! mpc (music-%status mpc) e))
	    (music-update-status-sans-lock! mpc (music-%status mpc)))
	 (with-access::musicstatus (music-%status mpc) (song songpos)
	    (let ((song (if (null? nsong) song (car nsong)))
		  (time (if (fixnum? ntime)
			    ntime
			    (+ (if (pair? songpos) (car songpos) songpos)
			       (flonum->fixnum ntime)))))
	       (mpc-cmd mpc (format "seek ~a ~a" song time) ok-parser))))))

;*---------------------------------------------------------------------*/
;*    music-stop ::mpc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (music-stop mpc::mpc)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (mpc-cmd mpc "stop" ok-parser)
	 (mpc-cmd mpc "clearerror" ok-parser))))

;*---------------------------------------------------------------------*/
;*    music-pause ::mpc ...                                            */
;*---------------------------------------------------------------------*/
(define-method (music-pause mpc::mpc)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (mpc-cmd mpc "pause" ok-parser))))

;*---------------------------------------------------------------------*/
;*    music-next ::mpc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (music-next mpc::mpc)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (mpc-cmd mpc "next" ok-parser))))

;*---------------------------------------------------------------------*/
;*    music-prev ::mpc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (music-prev mpc::mpc)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (mpc-cmd mpc "previous" ok-parser))))

;*---------------------------------------------------------------------*/
;*    music-crossfade ::mpc ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-crossfade mpc::mpc sec::int)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (let ((cmd (string-append "crossfade " (integer->string sec))))
	    (mpc-cmd mpc cmd ok-parser)))))

;*---------------------------------------------------------------------*/
;*    music-random-set! ::mpc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-random-set! mpc::mpc flag::bool)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (mpc-cmd mpc (if "random 1" "random 0") ok-parser))))

;*---------------------------------------------------------------------*/
;*    music-repeat-set! ::mpc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-repeat-set! mpc::mpc flag::bool)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (mpc-cmd mpc (if "repeat 1" "repeat 0") ok-parser))))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::mpc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get mpc::mpc)
   
   (define mpc-status-volume-grammar
      (regular-grammar ()
	 ((: (+ alpha) #\:)
	  (let ((k (the-keyword)))
	     (case k
		((volume:)
		 (read/rp *mpc-integer-grammar* (the-port)))
		(else
		 (read/rp *mpc-ignore-grammar* (the-port))
		 (ignore)))))
	 ("OK\n"
	  'ok)
	 (else
	  (raise
	   (instantiate::&io-parse-error
	      (proc 'mpc-volume)
	      (msg "Illegal status")
	      (obj (parse-error-msg (the-failure) (the-port))))))))

   (define (get-volume-parser mpc)
      (let ((ip (socket-input (mpc-%socket mpc))))
	 (let loop ((tm 0))
	    (let ((l (read/rp mpc-status-volume-grammar ip)))
	       (cond
		  ((integer? l)
		   (loop l))
		  ((eq? l 'ok)
		   tm)
		  (else
		   (loop tm)))))))
   
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (with-handler
	    (lambda (e)
	       (set-error! mpc (mpc-%status mpc) e)
	       0)
	    (mpc-cmd mpc "status" get-volume-parser)))))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::mpc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! mpc::mpc vol)
   (with-timed-lock (mpc-%mutex mpc)
      (lambda ()
	 (let ((cmd (string-append "setvol " (integer->string vol))))
	    (mpc-cmd mpc cmd ok-parser)))))
   
