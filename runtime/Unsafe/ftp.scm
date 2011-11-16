;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/ftp.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Cyprien Nicolas                                   */
;*    Creation    :  Wed Aug 18 14:30:52 2010                          */
;*    Last change :  Mon Nov 14 11:42:20 2011 (serrano)                */
;*    Copyright   :  2010-11 Cyprien Nicolas, Manuel Serrano           */
;*    -------------------------------------------------------------    */
;*    FTP client implementation.                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ftp

   (import __error
	   __object)
   
   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __thread
	   __bit
	   __bignum
	   __r4_numbers_6_5
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_numbers_6_5_flonum_dtoa
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_vectors_6_8
	   __r4_control_features_6_9
	   __r4_pairs_and_lists_6_3
	   __r4_characters_6_6
	   __r4_equivalence_6_2
	   __r4_strings_6_7
	   __r4_ports_6_10_1
	   __r4_input_6_10_2
	   __r4_output_6_10_3
	   __r5_control_features_6_4
	   __foreign
	   __evenv
	   __os
	   __structure
	   __param
	   __socket
	   __rgc
	   __url)

   (export (abstract-class %ftp
	      (cmd::obj (default #f))
	      (dtp::obj (default #f))
	      (passive?::bool (default #t)))

	   (class ftp::%ftp
	      (host::bstring (default "localhost"))
	      (port::bint (default 21))
	      (motd::bstring (default ""))
	      (user::bstring (default "anonymous"))
	      (pass::bstring (default "foo@bar.net"))
	      (acct::bstring (default "")))
	   (class &ftp-error::&error)
	   (class &ftp-parse-error::&io-parse-error)

	   (ftp-connect::bool ::ftp #!optional (timeout 0))
	   
	   ;; Access control commands
	   (ftp-logout::bool ::ftp)
	   (ftp-cd::bool ::ftp ::bstring)
	   (ftp-cd-parent::bool ::ftp)
	   (ftp-mount::bool ::ftp ::bstring)
	   (ftp-reinitialize::bool ::ftp)
	   
	   ;; Transfer parameter commands
	   (ftp-data-port::bool ::ftp ::bstring ::bint)
	   (ftp-passive::bool ::ftp)
	   (ftp-data-type::bool ::ftp ::symbol . local-byte-size)
	   (ftp-file-structure::bool ::ftp . structure)
	   (ftp-transfer-mode::bool ::ftp . mode)
	   
	   ;; FTP service commands
	   (ftp-retrieve::obj ::ftp ::bstring)
	   (ftp-store::bool ::ftp ::bstring dest-or-false)
	   (ftp-append::bool ::ftp ::bstring ::bstring)
	   (ftp-allocate::bool ::ftp ::bint . page-or-record-size)
	   (ftp-restart::bool ::ftp ::bint)
	   (ftp-rename-file::bool ::ftp ::bstring ::bstring)
	   (ftp-abort::bool ::ftp)
	   (ftp-delete::bool ::ftp ::bstring)
	   (ftp-rmdir::bool ::ftp ::bstring)
	   (ftp-pwd::bstring ::ftp)
	   (ftp-list::pair-nil ::ftp . path)
	   (ftp-name-list::pair-nil ::ftp . path)
	   (ftp-site-parameters::bstring ::ftp . param)
	   (ftp-system::bstring ::ftp)
	   (ftp-status::bstring ::ftp . path)
	   (ftp-help::bstring ::ftp . topic)
	   (ftp-noop::bool ::ftp)

	   ;; unified IO api
	   (open-input-ftp-file ::bstring #!optional (bufinfo #t) (timeout 1000000))
	   (ftp-directory->list ::ftp ::bstring)
	   (ftp-directory->path-list ::ftp ::bstring)
	   (ftp-make-directory::bool ::ftp ::bstring)
	   (ftp-copy-file ::ftp ::bstring ::bstring)
	   (ftp-put-file ::ftp ::bstring)))

;*---------------------------------------------------------------------*/
;*    object-print ::ftp ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-print o::ftp p print-slot)
   (with-access::ftp o (host user dtp passive?)
      (display "#<%ftp host=" p)
      (print-slot host p)
      (display " user=" p)
      (print-slot user p)
      (display " data-transfer-port=" p)
      (print-slot dtp p)
      (display " passive-mode=" p)
      (display passive? p)
      (display ">" p)))

;*---------------------------------------------------------------------*/
;*    %ftp-send-cmd ...                                                */
;*---------------------------------------------------------------------*/
(define (%ftp-send-cmd ftp . msgs)
   (with-access::%ftp ftp (cmd)
      (let ((op (socket-output cmd)))
	 (fprintf op "~l\r\n" msgs)
	 (flush-output-port op))))

;*---------------------------------------------------------------------*/
;*    %ftp-read-cmd ...                                                */
;*---------------------------------------------------------------------*/
(define (%ftp-read-cmd ftp)
   (with-access::%ftp ftp (cmd)
      (let* ((ip (socket-input cmd))
	     (msg (read-line ip)))
	 (if (eof-object? msg)
	     (values 999 "EOF" #f)
	     (let ((code (string->number (substring msg 0 3)))
		   (mesg (substring msg 4))
		   (more? (char=? #\- (string-ref msg 3))))
		(when more?
		   (let loop ((msg (read-line ip)))
		      (unless (eof-object? msg)
			 (string-case msg
			    ((bol (: (= 3 digit) #\- (* all)))
			     (set! mesg (string-append mesg "\n" (the-substring 4 (the-length))))
			     (loop (read-line ip)))
			    ((bol (: (= 3 digit) space (* all)))
			     (let ((code1 (string->number (the-substring 0 3)))
				   (mesg1 (the-substring 4 (the-length))))
				(set! mesg (string-append mesg "\n" mesg1))
				(unless (= code code1)
				   (loop (read-line ip)))))
			    ((bol (: space (* all)))
			     (set! mesg (string-append mesg "\n" (the-substring 1 (the-length))))
			     (loop (read-line ip)))
			    ((bol (* all))
			     (set! mesg (string-append mesg "\n" (the-string)))
			     (loop (read-line ip)))
			    (else
			     (raise (instantiate::&ftp-parse-error
				       (proc "%ftp-read-cmd")
				       (msg  "Unrecognized output format")
				       (obj  msg))))))))
		(values code mesg))))))

;*---------------------------------------------------------------------*/
;*    %ftp-read-dtp ...                                                */
;*---------------------------------------------------------------------*/
(define (%ftp-read-dtp ftp what)
   (with-access::%ftp ftp (dtp)
      (let ((ip (socket-input dtp)))
	 (case what
	    ;; Read several lines from the dtp
	    ((list) 
	     (read-lines ip))
	    ((port)
	     ip)
	    (else
	     (raise (instantiate::&ftp-error
		       (proc "%ftp-read-dtp")
		       (msg  "Dunno what to read")
		       (obj what))))))))

;*---------------------------------------------------------------------*/
;*    %ftp-engine-cmd ...                                              */
;*---------------------------------------------------------------------*/
(define (%ftp-engine-cmd ftp cmd . cmds)
   (with-access::ftp ftp ((ftpcmd cmd))
      (unless (socket? ftpcmd)
	 (error "ftp" "Socket not connected" cmd)))
   (when cmd (apply %ftp-send-cmd ftp cmd cmds))
   (multiple-value-bind (code mesg)
      (%ftp-read-cmd ftp)
      (bind-exit (escape)
	 (define (close x)
	    (%ftp-close ftp)
	    (escape x))
	 (with-access::ftp ftp (motd user pass acct)
	    (case code
	       ;; Positive Preliminary reply
	       ((110) ; Restart marker reply.
		;; In this case, the text is exact and not left to the
		;; particular implementation; it must read:
		;;     MARK yyyy = mmmm
		;; Where yyyy is User-process data stream marker, and mmmm
		;; server's equivalent marker
		;; (note the spaces between markers and "=").
		#f)
	       ((120)
		;; Service ready in nnn minutes.
		#f)
	       ((125)
		;; Data connection already open; transfer starting.
		#f)
	       ((150)
		;; File status okay; about to open data connection.
		(let ((res (%ftp-read-dtp ftp
					  (cond
					     ((string=? cmd "RETR") 'port)
					     (else 'list)))))
		   (%ftp-engine-cmd ftp #f)
		   res))
	       
	       ;; Positive Completion reply
	       ((200)
		;; Command okay.
		#t)
	       ((202)
		;; Command not implemented, superfluous at this site.
		#t)
	       ((211)
		;; System status, or system help reply.
		mesg)
	       ((212)
		;; Directory status.
		mesg)
	       ((213)
		;; File status.
		mesg)
	       ((214)
		;; Help message.
		;; On how to use the server or the meaning of a particular
		;; non-standard command.  This reply is useful only to the
		;; human user.
		mesg)
	       ((215)
		;; NAME system type.
		;; Where NAME is an official system name from the list in the
		;; Assigned Numbers document.
		mesg)
	       ((220)
		;; Service ready for new user.
		(set! motd mesg)
		(%ftp-engine-cmd ftp "USER" user))
	       ((221)
		;; Service closing control connection.
		;; Logged out if appropriate.
		#t)
	       ((225)
		;; Data connection open; no transfer in progress.
		#t)
	       ((226)
		;; Closing data connection.
		;; Requested file action successful (for example, file
		;; transfer or file abort).
		(unless (string-contains-ci mesg " ABOR ")
		   (%ftp-dtp-pasv-setup ftp)))
	       ((227)
		;; Entering Passive Mode (h1,h2,h3,h4,p1,p2).
		(multiple-value-bind (host port)
		   (ftpport->hostport
                    (map string->number
                         (string-split
			    (substring mesg 1 (-fx (string-length mesg) 1)) ",")))
		   (%ftp-dtp-pasv-init ftp host port)
		   (%ftp-dtp-pasv-setup ftp)))
	       ((230)
		;; User logged in, proceed.
		#t)
	       ((250)
		;; Requested file action okay, completed.
		#t)
	       ((257)
		;; "PATHNAME" created.
		#t)
	       
	       ;; Positive Intermediate reply
	       ((331)
		;; User name okay, need password.
		(%ftp-engine-cmd ftp "PASS" pass))
	       ((332)
		;; Need account for login.
		(%ftp-engine-cmd ftp "ACCT" acct))
	       ((350)
		;; Requested file action pending further information.
		#t)
	       
	       ;; RNFR
	       ;; Transient Negative Completion reply
	       ((421)
		;; Service not available, closing control connection.
		;; This may be a reply to any command if the service knows
		;; it must shut down.
		#f)
	       ((425)
		;; Can't open data connection.
		(%ftp-dtp-init ftp)
		(apply %ftp-engine-cmd ftp cmd cmds))
	       ((426)
		;; Connection closed; transfer aborted.
		#f)
	       ((450)
		;; Requested file action not taken.
		;; File unavailable (e.g., file busy).
		#f)
	       ((451)
		;; Requested action aborted: local error in processing.
		#f)
	       ((452)
		;; Requested action not taken.
		;; Insufficient storage space in system.
		#f)
	       
	       ;; Permament Negative Completion reply
	       ((500)
		;; Syntax error, command unrecognized.
		;; This may include errors such as command line too long.
		#f)
	       ((501)
		;; Syntax error in parameters or arguments.
		#f)
	       ((502)
		;; Command not implemented.
		#f)
	       ((503)
		;; Bad sequence of commands.
		#f)
	       ((504)
		;; Command not implemented for that parameter.
		#f)
	       ((530)
		;; Not logged in.
		#f)
	       ((532)
		;; Need account for storing files.
		#f)
	       ((550)
		;; Requested action not taken.
		;; File unavailable (e.g., file not found, no access).
		#f)
	       ((551)
		;; Requested action aborted: page type unknown.
		#f)
	       ((552)
		;; Requested file action aborted. Exceeded storage
		;; allocation (for current directory or dataset).
		#f)
	       ((553)
		;; Requested action not taken; File name not allowed.
		#f)
	       ((999)
		;; EOF...
		(close "Remote host closed socket"))
	       (else
		(error "ftp" "Unknown return code" code)))))))

;*---------------------------------------------------------------------*/
;*    %ftp-close ...                                                   */
;*---------------------------------------------------------------------*/
(define (%ftp-close ftp)
   (unwind-protect
      (%ftp-close-cmd ftp)
      (%ftp-close-dtp ftp)))

;*---------------------------------------------------------------------*/
;*    %ftp-close-cmd ...                                               */
;*---------------------------------------------------------------------*/
(define (%ftp-close-cmd ftp)
   (with-access::%ftp ftp (cmd)
      (when (socket? cmd)
	 (socket-close cmd))))

;*---------------------------------------------------------------------*/
;*    %ftp-close-dtp ...                                               */
;*---------------------------------------------------------------------*/
(define (%ftp-close-dtp ftp)
   (with-access::%ftp ftp (passive? dtp)
      (when (socket? dtp)
	 (if passive?
	     (socket-close dtp)
	     (socket-shutdown dtp)))))

;*---------------------------------------------------------------------*/
;*    ftp-connect-to ...                                               */
;*---------------------------------------------------------------------*/
(define (ftp-connect-to #!key (host::bstring "localhost") (port::bint 21))
   (ftp-connect (instantiate::ftp (host host) (port port))))

;*---------------------------------------------------------------------*/
;*    ftp-connect ...                                                  */
;*---------------------------------------------------------------------*/
(define (ftp-connect ftp::ftp #!optional (timeout 0))
   (with-access::ftp ftp (host port cmd)
      (set! cmd (make-client-socket host port :timeout timeout))
      (%ftp-engine-cmd ftp #f)))

;*---------------------------------------------------------------------*/
;*    %ftp-dtp-init ...                                                */
;*---------------------------------------------------------------------*/
(define (%ftp-dtp-init ftp)
   (with-access::%ftp ftp (passive? dtp)
      (if passive?
	  (%ftp-engine-cmd ftp "PASV")
	  (raise (instantiate::&ftp-error
		    (proc "ftp-data-port")
		    (msg  "PORT functionality is not yet implemented")
		    (obj  ftp))))))

;*---------------------------------------------------------------------*/
;*    %ftp-dtp-pasv-init ...                                           */
;*---------------------------------------------------------------------*/
(define (%ftp-dtp-pasv-init ftp host port)
   (with-access::%ftp ftp (dtp)
      (%ftp-close-dtp ftp)
      (set! dtp (list host port))))

;*---------------------------------------------------------------------*/
;*    %ftp-dtp-pasv-setup ...                                          */
;*---------------------------------------------------------------------*/
(define (%ftp-dtp-pasv-setup ftp)
   (with-access::%ftp ftp (dtp)
      (bind-exit (escape)
	 (multiple-value-bind (host port)
	    (cond
	       ((socket? dtp)
		(values (socket-host-address dtp)
			(socket-port-number dtp)))
	       ((pair? dtp)
		(values (car dtp) (cadr dtp)))
	       (else
		(escape #f)))
	    (set! dtp (make-client-socket host port))
	    (socket? dtp)))))

;*---------------------------------------------------------------------*/
;*    hostport->ftpport ...                                            */
;*---------------------------------------------------------------------*/
(define (hostport->ftpport hostname::bstring port::bint)
   (let* ((addr (host hostname)))
      (append (string-split addr ".")
	      (list (quotient port 256) (remainder port 256)))))

;*---------------------------------------------------------------------*/
;*    ftpport->hostport ...                                            */
;*---------------------------------------------------------------------*/
(define (ftpport->hostport plist::pair-nil)
   (let ((addr (format "~a.~a.~a.~a"
		       (list-ref plist 0)
		       (list-ref plist 1)
		       (list-ref plist 2)
		       (list-ref plist 3)))
	 (port (+ (* (list-ref plist 4) 256) (list-ref plist 5))))
      (values addr port)))

;*---------------------------------------------------------------------*/
;*    ftp-logout ...                                                   */
;*---------------------------------------------------------------------*/
(define (ftp-logout ftp)
   (%ftp-engine-cmd ftp "QUIT"))

;*---------------------------------------------------------------------*/
;*    ftp-cd ...                                                       */
;*---------------------------------------------------------------------*/
(define (ftp-cd ftp directory)
   (%ftp-engine-cmd ftp "CWD" directory))

;*---------------------------------------------------------------------*/
;*    ftp-cd-parent ...                                                */
;*---------------------------------------------------------------------*/
(define (ftp-cd-parent ftp)
   (%ftp-engine-cmd ftp "CDUP"))

;*---------------------------------------------------------------------*/
;*    ftp-mount ...                                                    */
;*---------------------------------------------------------------------*/
(define (ftp-mount ftp mount-point)
   (%ftp-engine-cmd ftp "SMNT" mount-point))

;*---------------------------------------------------------------------*/
;*    ftp-reinitialize ...                                             */
;*---------------------------------------------------------------------*/
(define (ftp-reinitialize ftp)
   (%ftp-engine-cmd ftp "REIN"))

;*---------------------------------------------------------------------*/
;*    ftp-data-port ...                                                */
;*---------------------------------------------------------------------*/
(define (ftp-data-port ftp host port)
   (raise (instantiate::&ftp-error
	     (proc "ftp-data-port")
	     (msg  "PORT functionality is not implemented")
	     (obj  ftp))))

;*---------------------------------------------------------------------*/
;*    ftp-passive ...                                                  */
;*---------------------------------------------------------------------*/
(define (ftp-passive ftp)
   (%ftp-engine-cmd ftp "PASV"))

;*---------------------------------------------------------------------*/
;*    ftp-data-type ...                                                */
;*---------------------------------------------------------------------*/
(define (ftp-data-type ftp type . local-byte-size)
   (case (string-ref (symbol->string! type) 0)
      ((#\a #\A)
       (%ftp-engine-cmd ftp "TYPE" "A"))
      ((#\i #\I)
       (%ftp-engine-cmd ftp "TYPE" "I"))
      (else
       (raise (instantiate::&ftp-parse-error
		 (proc "ftp-data-type")
		 (msg  "Illegal type value, should be on of '(ascii image)")
		 (obj  type))))))

;*---------------------------------------------------------------------*/
;*    ftp-file-structure ...                                           */
;*---------------------------------------------------------------------*/
(define (ftp-file-structure ftp . structure)
   (%ftp-engine-cmd ftp "STRU" "F"))

;*---------------------------------------------------------------------*/
;*    ftp-transfer-mode ...                                            */
;*---------------------------------------------------------------------*/
(define (ftp-transfer-mode ftp . mode)
   (%ftp-engine-cmd ftp "MODE" "S"))

;*---------------------------------------------------------------------*/
;*    ftp-retrieve ...                                                 */
;*---------------------------------------------------------------------*/
(define (ftp-retrieve ftp filename)
   (%ftp-engine-cmd ftp "RETR" filename))

;*---------------------------------------------------------------------*/
;*    ftp-store ...                                                    */
;*---------------------------------------------------------------------*/
(define (ftp-store ftp source destination)
   (with-access::%ftp ftp (dtp)
      (let ((op (socket-output dtp)))
	 (and (file-exists? source)
	      (if destination
		  (%ftp-engine-cmd ftp "STOR" destination)
		  (%ftp-engine-cmd ftp "STOU"))
	      (send-file source op (file-size source) #e0)))))

;*---------------------------------------------------------------------*/
;*    ftp-append ...                                                   */
;*---------------------------------------------------------------------*/
(define (ftp-append ftp source destination)
   (with-access::%ftp ftp (dtp)
      (let ((op (socket-output dtp)))
	 (and (file-exists? source)
	      (%ftp-engine-cmd ftp "APPE" source destination)
	      (send-file source op (file-size source) #e0)))))

;*---------------------------------------------------------------------*/
;*    ftp-allocate ...                                                 */
;*---------------------------------------------------------------------*/
(define (ftp-allocate ftp size . page-or-record-size)
   (let ((prsize (and (pair? page-or-record-size) (car page-or-record-size))))
      (if prsize
	  (%ftp-engine-cmd ftp "ALLO" size "R" prsize)
	  (%ftp-engine-cmd ftp "ALLO" size))))

;*---------------------------------------------------------------------*/
;*    ftp-restart ...                                                  */
;*---------------------------------------------------------------------*/
(define (ftp-restart ftp position)
   (%ftp-engine-cmd ftp "REST" position))

;*---------------------------------------------------------------------*/
;*    ftp-rename-file ...                                              */
;*---------------------------------------------------------------------*/
(define (ftp-rename-file ftp from to)
   (and (%ftp-engine-cmd ftp "RNFR" from)
	(%ftp-engine-cmd ftp "RNTO" to)))

;*---------------------------------------------------------------------*/
;*    ftp-abort ...                                                    */
;*---------------------------------------------------------------------*/
(define (ftp-abort ftp)
   (%ftp-engine-cmd ftp "ABOR"))

;*---------------------------------------------------------------------*/
;*    ftp-delete ...                                                   */
;*---------------------------------------------------------------------*/
(define (ftp-delete ftp file)
   (%ftp-engine-cmd ftp "DELE" file))

;*---------------------------------------------------------------------*/
;*    ftp-rmdir ...                                                    */
;*---------------------------------------------------------------------*/
(define (ftp-rmdir ftp directory)
   (%ftp-engine-cmd ftp "RMD" directory))

;*---------------------------------------------------------------------*/
;*    ftp-make-directory ...                                           */
;*---------------------------------------------------------------------*/
(define (ftp-make-directory ftp directory)
   (%ftp-engine-cmd ftp "MKD" directory))

;*---------------------------------------------------------------------*/
;*    ftp-pwd ...                                                      */
;*---------------------------------------------------------------------*/
(define (ftp-pwd ftp)
   (%ftp-engine-cmd ftp "PWD"))

;*---------------------------------------------------------------------*/
;*    ftp-list ...                                                     */
;*---------------------------------------------------------------------*/
(define (ftp-list ftp . args)
   (apply %ftp-engine-cmd ftp "LIST" args))

;*---------------------------------------------------------------------*/
;*    ftp-name-list ...                                                */
;*---------------------------------------------------------------------*/
(define (ftp-name-list ftp . args)
   (apply %ftp-engine-cmd ftp "NLST" args))

;*---------------------------------------------------------------------*/
;*    ftp-site-parameters ...                                          */
;*---------------------------------------------------------------------*/
(define (ftp-site-parameters ftp . args)
   (apply %ftp-engine-cmd ftp "SITE" args))

;*---------------------------------------------------------------------*/
;*    ftp-system ...                                                   */
;*---------------------------------------------------------------------*/
(define (ftp-system ftp)
   (%ftp-engine-cmd ftp "SYST"))

;*---------------------------------------------------------------------*/
;*    ftp-status ...                                                   */
;*---------------------------------------------------------------------*/
(define (ftp-status ftp . args)
   (apply %ftp-engine-cmd ftp "STAT" args))

;*---------------------------------------------------------------------*/
;*    ftp-help ...                                                     */
;*---------------------------------------------------------------------*/
(define (ftp-help ftp . args)
   (let ((arg (and (pair? args) (car args))))
      (if arg
	  (%ftp-engine-cmd ftp "HELP" arg)
	  (%ftp-engine-cmd ftp "HELP"))))

;*---------------------------------------------------------------------*/
;*    ftp-noop ...                                                     */
;*---------------------------------------------------------------------*/
(define (ftp-noop ftp)
   (%ftp-engine-cmd ftp "NOOP"))

;*---------------------------------------------------------------------*/
;*    open-input-ftp-file ...                                          */
;*---------------------------------------------------------------------*/
(define (open-input-ftp-file string #!optional (bufinfo #t) (timeout 1000000))
   
   (define (parser ip status-code header clen tenc)
      (if (not (and (>=fx status-code 200) (<=fx status-code 299)))
	  (case status-code
	     ((401)
	      (raise (instantiate::&io-port-error
			(proc 'open-input-file)
			(msg "Cannot open URL, authentication required")
			(obj (string-append "http://" string)))))
	     ((404)
	      (raise (instantiate::&io-file-not-found-error
			(proc 'open-input-file)
			(msg "Cannot open URL")
			(obj (string-append "http://" string)))))
	     (else
	      (raise (instantiate::&io-port-error
			(proc 'open-input-file)
			(msg (format "Cannot open URL (~a)" status-code))
			(obj (string-append "http://" string))))))
	  (cond
	     ((not (input-port? ip))
	      (open-input-string ""))
	     (clen
	      (input-port-fill-barrier-set! ip (elong->fixnum clen))
	      ($input-port-length-set! ip clen)
	      ip)
	     (else
	      ip))))
   
   (multiple-value-bind (protocol login host port abspath)
      (url-sans-protocol-parse string "ftp")
      (let* ((i (when (string? login) (string-index login #\:)))
	     (ftp (instantiate::ftp
		     (host host)
		     (user (cond
			      (i (substring login 0 i))
			      ((string? login) login)
			      (else "anonymous")))
		     (pass (cond
			      (i (substring login (+fx i 1)))
			      (else "foo@bar.net"))))))
	 (when (ftp-connect ftp timeout)
	    (let ((pi (ftp-retrieve ftp abspath)))
	       (when (input-port? pi)
		  (input-port-close-hook-set! pi (lambda (v) (%ftp-close ftp)))
		  pi))))))
   
;*---------------------------------------------------------------------*/
;*    ftp-directory->list ...                                          */
;*---------------------------------------------------------------------*/
(define (ftp-directory->list ftp dir)
   (let ((len (string-length dir))
	 (lst (ftp-name-list ftp dir)))
      (cond
	 ((and (pair? lst) (pair? (cdr lst)))
	  (map (lambda (file) (substring file (+fx len 1))) lst))
	 ((null? lst)
	  '())
	 ((string=? (car lst) dir)
	  dir)
	 (else
	  (substring (car lst) (+fx len 1))))))

;*---------------------------------------------------------------------*/
;*    ftp-directory->path-list ...                                     */
;*---------------------------------------------------------------------*/
(define (ftp-directory->path-list ftp dir)
   (ftp-name-list ftp dir))

;*---------------------------------------------------------------------*/
;*    ftp-copy-file ...                                                */
;*---------------------------------------------------------------------*/
(define (ftp-copy-file ftp from dst)
   (let ((r (ftp-retrieve ftp from)))
      (when (input-port? r)
	 (with-output-to-file dst
	    (lambda ()
	       (display (read-string r))
	       #t)))))

;*---------------------------------------------------------------------*/
;*    ftp-put-file ...                                                 */
;*---------------------------------------------------------------------*/
(define (ftp-put-file ftp path)
   (ftp-store ftp path #t))
