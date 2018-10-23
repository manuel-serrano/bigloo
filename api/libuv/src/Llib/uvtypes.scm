;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/libuv/src/Llib/uvtypes.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 11:55:29 2014                          */
;*    Last change :  Tue Oct 23 11:30:19 2018 (serrano)                */
;*    Copyright   :  2014-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV types                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_types

   (include "uv.sch")

   (extern (export uv-handle-type-symbol "bgl_uv_handle_type_symbol")
	   (export uv-new-file "bgl_uv_new_file")
	   (export uv-guess-handle "bgl_uv_guess_handle"))
   
   (export (abstract-class %Uv
	      (%uv-init))

	   (class UvHandle::%Uv
	      ($builtin::$uv_handle_t (default $uv_handle_nil))
	      (%onclose (default #f))
	      (%gcmarkshead::pair-nil (default '()))
	      (%gcmarkstail::pair-nil (default '()))
	      (closed::bool (default #f)))

	   (class UvLoop::UvHandle
	      (%mutex::mutex read-only (default (make-mutex))))

	   (abstract-class UvWatcher::UvHandle
	      (loop::UvLoop read-only)
	      (cb::procedure (default list)))

	   (class UvStream::UvHandle
	      (loop::UvLoop read-only)
	      (%alloc::obj (default #f))
	      (%offset::obj (default 0))
	      (%proca (default #f))
	      (%procc (default #f))
	      (%callback (default #f)))

	   (class UvTcp::UvStream)
	   
	   (class UvUdp::UvStream
	      (%procm::pair-nil (default '())))
	   
	   (class UvPipe::UvStream
	      (ipc::bool read-only))
	   
	   (class UvTty::UvStream
	      (fd::int read-only)
	      (readable::bool read-only))
	   
	   (class UvTimer::UvWatcher
	      (repeat::uint64 (default #u64:0))
	      (ref::bool (default #t)))

	   (class UvIdle::UvWatcher)

	   (class UvAsync::UvWatcher)

	   (class UvPoll::UvWatcher
	      (fd::int read-only))

	   (class UvFile
	      (fd::int read-only)
	      (path::bstring read-only))

	   (class UvFsEvent::UvWatcher)

	   (class UvFsPoll::UvWatcher)

	   (class UvCheck::UvWatcher)

	   (class UvProcess::UvHandle
	      ($onexit (default #unspecified))
	      (pid::int
		 (get (lambda (this)
			 (with-access::UvProcess this ($builtin)
			    ($uv-process-pid ($uv-process-t $builtin)))))
		 read-only))

	   (class UvProcessOptions::%Uv
	      ($builtin::$uv_process_options_t read-only
		 (default ($uv-process-options-new)))
	      (file
		 (get (lambda (this)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-file $builtin))))
		 (set (lambda (this val)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-file-set! $builtin val)
			    #unspecified))))
	      (args
		 (get (lambda (this)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-args $builtin))))
		 (set (lambda (this val)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-args-set! $builtin val)
			    #unspecified))))
	      (env
		 (get (lambda (this)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-env $builtin))))
		 (set (lambda (this val)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-env-set! $builtin val)
			    #unspecified))))
	      (cwd
		 (get (lambda (this)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-cwd $builtin))))
		 (set (lambda (this val)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-cwd-set! $builtin val)
			    #unspecified))))
	      (flags
		 (get (lambda (this)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-flags $builtin))))
		 (set (lambda (this val)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-flags-set! $builtin val)
			    #unspecified))))
	      (uid
		 (get (lambda (this)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-uid $builtin))))
		 (set (lambda (this val)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-uid-set! $builtin val)
			    #unspecified))))
	      (gid
		 (get (lambda (this)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-gid $builtin))))
		 (set (lambda (this val)
			 (with-access::UvProcessOptions this ($builtin)
			    ($uv-process-options-gid-set! $builtin val)
			    #unspecified)))))

	   (class UvWork::%Uv
	      ($builtin::$uv_work_t (default $uv_work_nil))
	      (%work-cb::procedure read-only)
	      (%after-cb::procedure read-only))
	   
	   (generic %uv-init ::%Uv)

	   (uv-version::string)
	   (uv-id::int ::UvHandle)

	   (uv-new-file::UvFile ::int ::bstring)
	   (uv-strerror::bstring ::int)
	   (uv-err-name::bstring ::int)

	   (uv-handle-type-symbol ::int)
	   (uv-guess-handle::symbol ::int)
	   (inline uv-push-gcmark! ::UvHandle o)
	   (uv-pop-gcmark! ::UvHandle o)
	   (uv-gcmarks-empty?::bool o::UvHandle))

   (extern (export uv-pop-gcmark! "bgl_uv_pop_gcmark")))

;*---------------------------------------------------------------------*/
;*    uv-version ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-version)
   ($uv-version))

;*---------------------------------------------------------------------*/
;*    uv-id ...                                                        */
;*---------------------------------------------------------------------*/
(define (uv-id o::UvHandle)
   (with-access::UvHandle o ($builtin)
      ($uv-handle->integer $builtin)))
   
;*---------------------------------------------------------------------*/
;*    %uv-init ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (%uv-init o)
   o)

;*---------------------------------------------------------------------*/
;*    object-print ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-print obj::UvHandle port print-slot::procedure)
   
   (define (class-field-write/display field)
      (let* ((name (class-field-name field))
	     (get-value (class-field-accessor field))
	     (s (symbol->string! name)))
	 (unless (memq (string-ref s 0) '(#\$ #\%))
	    (display " [" port)
	    (display name port)
	    (display #\: port)
	    (display #\space port)
	    (print-slot (get-value obj) port)
	    (display #\] port))))

   (let* ((class (object-class obj))
	  (class-name (class-name class))
	  (fields (class-all-fields class))
	  (len (vector-length fields)))
      (display "#|" port)
      (display class-name port)
      (display " (0x" port)
      (display (integer->string (uv-id obj) 16) port)
      (display ")" port)
      (if (nil? obj)
	  (display " nil|" port)
	  (let loop ((i 0))
	     (if (=fx i len)
		 (display #\| port)
		 (begin
		    (class-field-write/display (vector-ref-ur fields i))
		    (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    uv-new-file ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-new-file::UvFile fd::int path::bstring)
   (instantiate::UvFile
      (fd fd)
      (path path)))

;*---------------------------------------------------------------------*/
;*    uv-strerror ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-strerror errno)
   (let ((s::string ($uv-strerror errno)))
      (if ($string-nilp s)
	  ""
	  s)))

;*---------------------------------------------------------------------*/
;*    uv-err-name ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-err-name errno)
   (let ((s::string ($uv-err-name errno)))
      (if ($string-nilp s)
	  ""
	  s)))

;*---------------------------------------------------------------------*/
;*    uv-guess-handle ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-guess-handle fd::int)
   (uv-handle-type-symbol ($uv-guess-handle fd)))

;*---------------------------------------------------------------------*/
;*    uv-handle-type-symbol ...                                        */
;*---------------------------------------------------------------------*/
(define (uv-handle-type-symbol r)
   (cond
      ((=fx r $uv-handle-tcp) 'TCP)
      ((=fx r $uv-handle-tty) 'TTY)
      ((=fx r $uv-handle-udp) 'UDP)
      ((=fx r $uv-handle-pipe) 'PIPE)
      ((=fx r $uv-handle-file) 'FILE)
      ((=fx r $uv-handle-unknown) 'UNKNOWN)
      (else 'UNDEFINED)))

;*---------------------------------------------------------------------*/
;*    uv-push-gcmark! ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (uv-push-gcmark! o::UvHandle val)
   (with-access::UvHandle o (%gcmarkshead %gcmarkstail)
      (if (null? %gcmarkstail)
	  (begin
	     (set! %gcmarkshead (cons val '()))
	     (set! %gcmarkstail %gcmarkshead))
	  (begin
	     (set-cdr! %gcmarkstail (cons val '()))
	     (set! %gcmarkstail (cdr %gcmarkstail))))
      [assert (val) (memq val %gcmarkshead)]))

;*---------------------------------------------------------------------*/
;*    uv-pop-gcmark! ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-pop-gcmark! o::UvHandle val)
   (with-access::UvHandle o (%gcmarkshead %gcmarkstail)
      (when (pair? %gcmarkshead)
	 (if (eq? val (car %gcmarkshead))
	     (if (eq? %gcmarkshead %gcmarkstail)
		 (begin
		    (set! %gcmarkshead '())
		    (set! %gcmarkstail '()))
		 (set! %gcmarkshead (cdr %gcmarkshead)))
	     (let loop ((p %gcmarkshead))
		(let ((n (cdr p)))
		   (when (pair? n)
		      (if (eq? (car n) val)
			  (begin
			     (when (eq? n %gcmarkstail)
				(set! %gcmarkstail p))
			     (set-cdr! p (cdr n)))
			  (loop n)))))))))

;*---------------------------------------------------------------------*/
;*    uv-gcmarks-empty? ...                                            */
;*---------------------------------------------------------------------*/
(define (uv-gcmarks-empty? o::UvHandle)
   (with-access::UvHandle o (%gcmarkshead)
      (null? %gcmarkshead)))
