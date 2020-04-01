;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/avahi/src/Llib/avahi.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:30:32 2011                          */
;*    Last change :  Fri Dec 13 12:00:32 2013 (serrano)                */
;*    Copyright   :  2011-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo binding for AVAHI                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __avahi_avahi

   (option (set! *dlopen-init-gc* #t))
   
   (library pthread)
    
   (include
      "avahi.sch")
   
   (extern
      (export avahi-error
	 "bgl_avahi_error")
      (export avahi-client-state->symbol
	 "bgl_avahi_client_state_to_symbol")
      (export avahi-entry-group-state->symbol
	 "bgl_avahi_entry_group_state_to_symbol")
      (export avahi-browser-event->symbol
	 "bgl_avahi_browser_event_to_symbol")
      (export avahi-resolver-event->symbol
	 "bgl_avahi_resolver_event_to_symbol")
      (export avahi-if-index->symbol
	 "bgl_avahi_if_index_to_symbol")
      (export avahi-protocol->symbol
	 "bgl_avahi_protocol_to_symbol")
      (export avahi-symbol->protocol
	 "bgl_avahi_symbol_to_protocol")
      (export avahi-lookup-flags->symbol
	 "bgl_avahi_lookup_flags_to_symbol")
      (export avahi-lookup-result-flags->symbol
	 "bgl_avahi_lookup_result_flags_to_symbol")
      (export %avahi-lock! "bgl_avahi_lock")
      (export %avahi-unlock! "bgl_avahi_unlock")
      (export %avahi-signal "bgl_avahi_signal"))
   
   (export
      (class &avahi-error::&error
	 (errno::int read-only (default 0)))
      (class &avahi-collision-error::&avahi-error)
      
      (abstract-class avahi-object
	 (avahi-init))

      (abstract-class avahi-poll::avahi-object
	 ($ctype::int (default -1))
	 (%procs::pair-nil (default '())))
      
      (class avahi-simple-poll::avahi-poll
	 ($builtin::$avahi-simple-poll read-only (default ($avahi-simple-poll-nil))))
      
      (class avahi-threaded-poll::avahi-poll
	 ($builtin::$avahi-threaded-poll read-only (default ($avahi-threaded-poll-nil))))
      
      (class avahi-client::avahi-object
	 ($builtin::$avahi-client read-only (default ($avahi-client-nil)))
	 (poll::avahi-poll read-only)
	 (flags::symbol read-only (default 'none))
	 (proc::procedure read-only)
	 (%groups::pair-nil (default '()))
	 (%browsers::pair-nil (default '()))
	 (resolvers::pair-nil (default '()))
	 (version::bstring
	    read-only
	    (get (lambda (o::avahi-client)
		    (if ($avahi-client-nil? (-> o $builtin))
			"nil"
			($avahi-client-get-version-string (-> o $builtin))))))
	 (hostname::bstring
	    (get (lambda (o::avahi-client)
		    (if ($avahi-client-nil? (-> o $builtin))
			"nil"
			($avahi-client-get-host-name (-> o $builtin)))))
	    (set (lambda (o::avahi-client v::bstring)
		    (unless ($avahi-client-nil? (-> o $builtin))
		       ($avahi-client-set-host-name (-> o $builtin) v)))))
	 (domainname::bstring
	    read-only
	    (get (lambda (o::avahi-client)
		    (if ($avahi-client-nil? (-> o $builtin))
			"nil"
			($avahi-client-get-domain-name (-> o $builtin))))))
	 (hostname-fqdn::bstring
	    read-only
	    (get (lambda (o::avahi-client)
		    (if ($avahi-client-nil? (-> o $builtin))
			"nil"
			($avahi-client-get-host-name-fqdn (-> o $builtin))))))
	 (state::symbol
	    read-only
	    (get (lambda (o::avahi-client)
		    (if ($avahi-client-nil? (-> o $builtin))
			'nil
			(avahi-client-state->symbol
			   ($avahi-client-get-state (-> o $builtin))))))))
	    
      
      (class avahi-entry-group::avahi-object
	 ($builtin::$avahi-entry-group read-only (default ($avahi-entry-group-nil)))
	 (client::avahi-client read-only)
	 (proc::procedure read-only))

      (class avahi-service-browser::avahi-object
	 ($builtin::$avahi-service-browser read-only (default ($avahi-service-browser-nil)))
	 (client::avahi-client read-only)
	 (proc::procedure read-only)
	 (type::bstring read-only)
	 (domain::bstring read-only (default "")))

      (class avahi-service-type-browser::avahi-object
	 ($builtin::$avahi-service-type-browser read-only (default ($avahi-service-browser-nil)))
	 (client::avahi-client read-only)
	 (proc::procedure read-only)
	 (domain::bstring read-only (default "")))

      (class avahi-domain-browser::avahi-object
	 ($builtin::$avahi-domain-browser read-only (default ($avahi-service-browser-nil)))
	 (client::avahi-client read-only)
	 (proc::procedure read-only)
	 (btype::symbol read-only (default 'avahi-domain-browser-browse))
	 (domain::bstring read-only (default "")))
      
      (class avahi-service-resolver::avahi-object
	 ($builtin::$avahi-service-resolver read-only (default ($avahi-service-resolver-nil)))
	 (client::avahi-client read-only)
	 (proc::procedure read-only)
	 (interface::$avahi-if-index read-only)
	 (protocol::symbol read-only)
	 (name::bstring read-only)
	 (type::bstring read-only)
	 (domain::bstring read-only (default "")))

      (generic avahi-init ::avahi-object)
      
      (%avahi-thread-init!)
      (%avahi-lock!)
      (%avahi-unlock!)
      (%avahi-signal)
      
      (avahi-error ::string ::string ::obj ::int)
      
      (avahi-simple-poll-close ::avahi-simple-poll)
      (avahi-simple-poll-loop ::avahi-simple-poll)
      (avahi-simple-poll-quit ::avahi-simple-poll)
      (avahi-simple-poll-timeout ::avahi-simple-poll ::long ::procedure)
      
      (avahi-threaded-poll-close ::avahi-threaded-poll)
      (avahi-threaded-poll-loop ::avahi-threaded-poll)
      (avahi-threaded-poll-lock! ::avahi-threaded-poll)
      (avahi-threaded-poll-unlock! ::avahi-threaded-poll)
      (avahi-threaded-poll-quit ::avahi-threaded-poll)
      (avahi-threaded-poll-timeout ::avahi-threaded-poll ::long ::procedure)
      
      (avahi-client-close ::avahi-client)
      (avahi-client-error-message::bstring ::avahi-client)
      (avahi-client-state->symbol::symbol ::$avahi-client-state)
      (avahi-browser-event->symbol::symbol ::$avahi-browser-event)
      (avahi-resolver-event->symbol::symbol ::$avahi-resolver-event)
      (avahi-if-index->symbol::symbol ::$avahi-if-index)
      (avahi-protocol->symbol::symbol ::$avahi-protocol)
      (avahi-symbol->protocol::$avahi-protocol ::symbol)
      (avahi-lookup-flags->symbol ::$avahi-lookup-flags)      
      (avahi-lookup-result-flags->symbol ::$avahi-lookup-result-flags)      
      
      (avahi-service-browser-close ::avahi-service-browser)
      (avahi-service-resolver-close ::avahi-service-resolver)
      (avahi-domain-browser-close ::avahi-domain-browser)
      
      (avahi-entry-group-close ::avahi-entry-group)
      (avahi-entry-group-state->symbol::symbol ::$avahi-entry-group-state)
      (avahi-entry-group-empty?::bool ::avahi-entry-group)
      (avahi-entry-group-commit ::avahi-entry-group)
      (avahi-entry-group-reset! ::avahi-entry-group)
      (avahi-entry-group-add-service! ::avahi-entry-group
	 #!key name type domain host port subtype
	 #!rest txt)
      
      (avahi-alternative-host-name::bstring ::bstring)
      (avahi-alternative-service-name::bstring ::bstring))

   (pragma
      (%avahi-lock! no-trace)
      (%avahi-unlock! no-trace)
      (%avahi-signal no-trace)))

;*---------------------------------------------------------------------*/
;*    *avahi-mutex* ...                                                */
;*---------------------------------------------------------------------*/
(define *avahi-mutex* (make-mutex 'avahi))
(define *avahi-condv* (make-condition-variable 'avahi))

;*---------------------------------------------------------------------*/
;*    *avahi-thread* ...                                               */
;*---------------------------------------------------------------------*/
(define *avahi-thread* #unspecified)

;*---------------------------------------------------------------------*/
;*    avahi-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (avahi-error proc msg obj errno)
   (raise
      (instantiate::&avahi-error
	 (proc proc)
	 (obj obj)
	 (msg msg)
	 (errno errno))))

;*---------------------------------------------------------------------*/
;*    %avahi-lock! ...                                                 */
;*---------------------------------------------------------------------*/
(define (%avahi-lock!)
   (mutex-lock! *avahi-mutex*))

;*---------------------------------------------------------------------*/
;*    %avahi-unlock! ...                                               */
;*---------------------------------------------------------------------*/
(define (%avahi-unlock!)
   (mutex-unlock! *avahi-mutex*))

;*---------------------------------------------------------------------*/
;*    %avahi-signal ...                                                */
;*---------------------------------------------------------------------*/
(define (%avahi-signal)
   (condition-variable-signal! *avahi-condv*))

;*---------------------------------------------------------------------*/
;*    *avahi-gc-roots* ...                                             */
;*    -------------------------------------------------------------    */
;*    The *avahi-gc-roots list is used to mark Bigloo objects          */
;*    to prevent the GC to collect them while only held by avahi.      */
;*---------------------------------------------------------------------*/
(define *avahi-gc-roots* '())
(define *avahi-gc-mutex* (make-mutex))
   
;*---------------------------------------------------------------------*/
;*    avahi-gc-mark! ...                                               */
;*---------------------------------------------------------------------*/
(define (avahi-gc-mark! obj)
   (synchronize *avahi-gc-mutex*
      (set! *avahi-gc-roots* (cons obj *avahi-gc-roots*))))

;*---------------------------------------------------------------------*/
;*    avahi-gc-unmark! ...                                             */
;*---------------------------------------------------------------------*/
(define (avahi-gc-unmark! o)
   (synchronize *avahi-gc-mutex*
      (set! *avahi-gc-roots* (delete! o *avahi-gc-roots*))))

;*---------------------------------------------------------------------*/
;*    %avahi-thread-init! ...                                          */
;*---------------------------------------------------------------------*/
(define (%avahi-thread-init!)
   (unless (isa? *avahi-thread* thread)
      ;; the thread in charge of executing all callbacks
      (let ((lk (make-mutex))
	    (cv (make-condition-variable)))
	 (set! *avahi-thread*
	    (instantiate::pthread
	       (name "avahi")
	       (body (lambda ()
			(synchronize lk
			   (condition-variable-signal! cv))
			(let loop ()
			   (synchronize *avahi-mutex*
			      (condition-variable-wait!
				 *avahi-condv* *avahi-mutex*))
			   ($avahi-invoke-callbacks)
			   (loop))))))
	 (synchronize lk
	    (thread-start! *avahi-thread*)
	    (condition-variable-wait! cv lk)))))

;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-object ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (avahi-init o::avahi-object)
   o)

;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-simple-poll ...                               */
;*---------------------------------------------------------------------*/
(define-method (avahi-init o::avahi-simple-poll)
   (avahi-gc-mark! o)
   ($bgl-avahi-simple-poll-new o)
   (with-access::avahi-simple-poll o ($ctype)
      (set! $ctype 1))
   o)

;*---------------------------------------------------------------------*/
;*    avahi-simple-poll-close ...                                      */
;*---------------------------------------------------------------------*/
(define (avahi-simple-poll-close o::avahi-simple-poll)
   ($bgl-avahi-simple-poll-close o)
   (avahi-gc-unmark! o)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    avahi-simple-poll-loop ...                                       */
;*---------------------------------------------------------------------*/
(define (avahi-simple-poll-loop o::avahi-simple-poll)
   (with-access::avahi-simple-poll o ($builtin)
      ($avahi-simple-poll-loop $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    avahi-simple-poll-timeout ...                                    */
;*---------------------------------------------------------------------*/
(define (avahi-simple-poll-timeout o::avahi-simple-poll t::long proc::procedure)
   (if (correct-arity? proc 0)
       (with-access::avahi-simple-poll o ($builtin %procs)
	  (set! %procs (cons proc %procs))
	  ($bgl-avahi-simple-poll-timeout $builtin t proc))
       (avahi-error "avahi-simple-poll-timeout" "Illegal callback" proc
	  $avahi-err-invalid-object)))

;*---------------------------------------------------------------------*/
;*    avahi-simple-poll-quit ...                                       */
;*---------------------------------------------------------------------*/
(define (avahi-simple-poll-quit o::avahi-simple-poll)
   (with-access::avahi-simple-poll o ($builtin)
      ($avahi-simple-poll-quit $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-threaded-poll ...                             */
;*---------------------------------------------------------------------*/
(define-method (avahi-init o::avahi-threaded-poll)
   (avahi-gc-mark! o)
   ($bgl-avahi-threaded-poll-new o)
   (with-access::avahi-threaded-poll o ($ctype)
      (set! $ctype 2))
   (%avahi-thread-init!)
   o)

;*---------------------------------------------------------------------*/
;*    avahi-threaded-poll-close ...                                    */
;*---------------------------------------------------------------------*/
(define (avahi-threaded-poll-close o::avahi-threaded-poll)
   ($bgl-avahi-threaded-poll-close o)
   (avahi-gc-unmark! o)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    avahi-threaded-poll-loop ...                                     */
;*---------------------------------------------------------------------*/
(define (avahi-threaded-poll-loop o::avahi-threaded-poll)
   (with-access::avahi-threaded-poll o ($builtin)
      ($avahi-threaded-poll-loop $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    avahi-threaded-poll-lock! ...                                    */
;*---------------------------------------------------------------------*/
(define (avahi-threaded-poll-lock! o::avahi-threaded-poll)
   (with-access::avahi-threaded-poll o ($builtin)
      ($avahi-threaded-poll-lock $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    avahi-threaded-poll-unlock! ...                                  */
;*---------------------------------------------------------------------*/
(define (avahi-threaded-poll-unlock! o::avahi-threaded-poll)
   (with-access::avahi-threaded-poll o ($builtin)
      ($avahi-threaded-poll-unlock $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    avahi-threaded-poll-timeout ...                                  */
;*---------------------------------------------------------------------*/
(define (avahi-threaded-poll-timeout o::avahi-threaded-poll t::long proc::procedure)
   (if (correct-arity? proc 0)
       (with-access::avahi-threaded-poll o ($builtin %procs)
	  (set! %procs (cons proc %procs))
	  ($bgl-avahi-threaded-poll-timeout $builtin t proc))
       (avahi-error "avahi-threaded-poll-timeout" "Illegal callback" proc
	  $avahi-err-invalid-object)))

;*---------------------------------------------------------------------*/
;*    avahi-threaded-poll-quit ...                                     */
;*---------------------------------------------------------------------*/
(define (avahi-threaded-poll-quit o::avahi-threaded-poll)
   (with-access::avahi-threaded-poll o ($builtin)
      ($avahi-threaded-poll-quit $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-client ...                                    */
;*---------------------------------------------------------------------*/
(define-method (avahi-init o::avahi-client)
   (with-access::avahi-client o (proc)
      (if (correct-arity? proc 2)
	  (begin
	     (avahi-gc-mark! o)
	     ($bgl-avahi-client-new o))
	  (avahi-error "avahi-client-init" "Illegal callback" proc
	     $avahi-err-invalid-object))))

;*---------------------------------------------------------------------*/
;*    avahi-client-close ...                                           */
;*---------------------------------------------------------------------*/
(define (avahi-client-close o::avahi-client)
   ($bgl-avahi-client-close o)
   (with-access::avahi-client o (%groups %browsers resolvers)
      (set! %groups '())
      (set! %browsers '())
      (set! resolvers '()))
   (avahi-gc-unmark! o))

;*---------------------------------------------------------------------*/
;*    avahi-client-error-message ...                                   */
;*---------------------------------------------------------------------*/
(define (avahi-client-error-message o::avahi-client)
   (with-access::avahi-client o ($builtin)
      ($avahi-strerror ($avahi-client-errno $builtin))))

;*---------------------------------------------------------------------*/
;*    avahi-client-state->symbol ...                                   */
;*---------------------------------------------------------------------*/
(define (avahi-client-state->symbol state::$avahi-client-state)
   (cond
      ((=fx state $avahi-client-state-registering)
       'avahi-client-registering)
      ((=fx state $avahi-client-state-runnning)
       'avahi-client-running)
      ((=fx state $avahi-client-state-collision)
       'avahi-client-collision)
      ((=fx state $avahi-client-state-failure)
       'avahi-client-failure)
      ((=fx state $avahi-client-state-connecting)
       'avahi-client-connecting)
      (else
       (avahi-error "avahi-client-state->symbol" "Unknown state" state
	  $avahi-err-invalid-object))))

;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-entry-group ...                               */
;*---------------------------------------------------------------------*/
(define-method (avahi-init o::avahi-entry-group)
   (with-access::avahi-entry-group o (proc client)
      (if (correct-arity? proc 2)
	  (begin
	     (with-access::avahi-client client (%groups)
		(set! %groups (cons o %groups)))
	     ($bgl-avahi-entry-group-new o))
	  (avahi-error "avahi-entry-group" "Illegal callback" proc
	     $avahi-err-invalid-object))))

;*---------------------------------------------------------------------*/
;*    avahi-entry-group-close ...                                      */
;*---------------------------------------------------------------------*/
(define (avahi-entry-group-close o::avahi-entry-group)
   ($bgl-avahi-entry-group-close o)
   (with-access::avahi-entry-group o ($builtin client)
      (with-access::avahi-client client (%groups)
	 (set! %groups (remq! o %groups)))))

;*---------------------------------------------------------------------*/
;*    avahi-entry-group-empty? ...                                     */
;*---------------------------------------------------------------------*/
(define (avahi-entry-group-empty? o::avahi-entry-group)
   (with-access::avahi-entry-group o ($builtin)
      ($avahi-entry-group-empty? $builtin)))

;*---------------------------------------------------------------------*/
;*    avahi-entry-group-commit ...                                     */
;*---------------------------------------------------------------------*/
(define (avahi-entry-group-commit o::avahi-entry-group)
   (with-access::avahi-entry-group o ($builtin)
      (let ((n ($avahi-entry-group-commit $builtin)))
	 (unless (>=fx n 0)
	    (avahi-error "avahi-entry-group" ($avahi-strerror n) o n)))))

;*---------------------------------------------------------------------*/
;*    avahi-entry-group-reset! ...                                     */
;*---------------------------------------------------------------------*/
(define (avahi-entry-group-reset! o::avahi-entry-group)
   (with-access::avahi-entry-group o ($builtin client)
      (with-access::avahi-client client (%groups)
	 (set! %groups (remq! o %groups)))
      (let ((n ($avahi-entry-group-reset $builtin)))
	 (unless (>=fx n 0)
	    (avahi-error "avahi-entry-group" ($avahi-strerror n) o n)))))

;*---------------------------------------------------------------------*/
;*    avahi-entry-group-add-service! ...                               */
;*---------------------------------------------------------------------*/
(define (avahi-entry-group-add-service! o::avahi-entry-group
	   #!key name type domain host port subtype
	   #!rest txt)
   
   (define (check-error n)
      (when (<fx n 0)
	 (raise
	    (if (=fx n $avahi-err-collision)
		(instantiate::&avahi-collision-error
		   (proc "avahi-entry-group-add-service")
		   (msg ($avahi-strerror n))
		   (obj o)
		   (errno n))
		(instantiate::&avahi-error
		   (proc "avahi-entry-group-add-service")
		   (msg ($avahi-strerror n))
		   (obj o)
		   (errno n))))))
   
   (with-access::avahi-entry-group o ($builtin)
      (cond
	 (subtype
	  ;;; add-service-subtype
	  (check-error
	     ($avahi-entry-group-add-service-subtype $builtin
		$avahi-if-unspec
		$avahi-proto-unspec
		$avahi-publish-none
		name
		type
		(or domain ($string-null))
		subtype)))
	 ((pair? txt)
	  ;;; add-service-subtype
	  (let ((l::$avahi-string-list ($bgl-avahi-list->string-list txt)))
	     (unwind-protect
		(check-error
		   ($avahi-entry-group-add-service-strlst $builtin
		      $avahi-if-unspec
		      $avahi-proto-unspec
		      $avahi-publish-none
		      name
		      type
		      (or domain ($string-null))
		      (or host ($string-null))
		      port
		      l))
		($avahi-string-list-free l))))
	 (else
	  (check-error
	     ($avahi-entry-group-add-service $builtin
		$avahi-if-unspec
		$avahi-proto-unspec
		$avahi-publish-none
		name
		type
		(or domain ($string-null))
		(or host ($string-null))
		port
		($string-null)))))))
	   
;*---------------------------------------------------------------------*/
;*    avahi-entry-group-state->symbol ...                              */
;*---------------------------------------------------------------------*/
(define (avahi-entry-group-state->symbol state::$avahi-entry-group-state)
   (cond
      ((=fx state $avahi-entry-group-state-uncommited)
       'avahi-entry-group-uncommited)
      ((=fx state $avahi-entry-group-state-registering)
       'avahi-entry-group-registering)
      ((=fx state $avahi-entry-group-state-established)
       'avahi-entry-group-established)
      ((=fx state $avahi-entry-group-state-collision)
       'avahi-entry-group-collision)
      ((=fx state $avahi-entry-group-state-failure)
       'avahi-entry-group-failure)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-entry-group-state->symbol")
	     (msg "Unknown state")
	     (obj state)
	     (errno $avahi-err-invalid-object))))))

;*---------------------------------------------------------------------*/
;*    avahi-alternative-host-name ...                                  */
;*---------------------------------------------------------------------*/
(define (avahi-alternative-host-name name)
   ($avahi-alternative-host-name name))

;*---------------------------------------------------------------------*/
;*    avahi-alternative-service-name ...                               */
;*---------------------------------------------------------------------*/
(define (avahi-alternative-service-name name)
   ($avahi-alternative-service-name name))

;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-service-browser ...                           */
;*---------------------------------------------------------------------*/
(define-method (avahi-init o::avahi-service-browser)
   (with-access::avahi-service-browser o (proc client)
      (if (correct-arity? proc 8)
	  (begin
	     (with-access::avahi-client client (%browsers)
		(set! %browsers (cons o %browsers)))
	     ($bgl-avahi-service-browser-new o))
	  (avahi-error "avahi-service-browser" "Illegal callback" proc
	     $avahi-err-invalid-object))))

;*---------------------------------------------------------------------*/
;*    avahi-service-browser-close ...                                  */
;*---------------------------------------------------------------------*/
(define (avahi-service-browser-close o::avahi-service-browser)
   ($bgl-avahi-service-browser-close o)
   (with-access::avahi-service-browser o (client)
      (with-access::avahi-client client (%browsers)
	 (set! %browsers (remq! o %browsers))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-service-type-browser ...                      */
;*---------------------------------------------------------------------*/
(define-method (avahi-init o::avahi-service-type-browser)
   (with-access::avahi-service-type-browser o (proc)
      (if (correct-arity? proc 7)
	  (begin
	     (avahi-gc-mark! o)
	     ($bgl-avahi-service-type-browser-new o))
	  (avahi-error "avahi-service-type-browser" "Illegal callback" proc
	     $avahi-err-invalid-object))))

;*---------------------------------------------------------------------*/
;*    avahi-service-type-browser-close ...                             */
;*---------------------------------------------------------------------*/
(define (avahi-service-type-browser-close o::avahi-service-type-browser)
   ($bgl-avahi-service-type-browser-close o)
   (avahi-gc-unmark! o)
   #unspecified)
   
;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-domain-browser ...                            */
;*---------------------------------------------------------------------*/
(define-method (avahi-init o::avahi-domain-browser)
   (with-access::avahi-domain-browser o (proc btype)
      (if (correct-arity? proc 5)
	  (begin
	     (avahi-gc-mark! o)
	     ($bgl-avahi-domain-browser-new o (avahi-symbol->domain-browser-type btype)))
	  (avahi-error "avahi-domain-browser" "Illegal callback" proc
	     $avahi-err-invalid-object))))

;*---------------------------------------------------------------------*/
;*    avahi-domain-browser-close ...                                   */
;*---------------------------------------------------------------------*/
(define (avahi-domain-browser-close o::avahi-domain-browser)
   ($bgl-avahi-domain-browser-close o)
   (avahi-gc-unmark! o)
   #unspecified)
   
;*---------------------------------------------------------------------*/
;*    avahi-init ::avahi-service-resolver ...                          */
;*---------------------------------------------------------------------*/
(define-method (avahi-init o::avahi-service-resolver)
   (with-access::avahi-service-resolver o (proc client)
      (if (correct-arity? proc 12)
	  (begin
	     (with-access::avahi-client client (resolvers)
		(set! resolvers (cons o resolvers)))
	     ($bgl-avahi-service-resolver-new o))
	  (avahi-error "avahi-service-resolver" "Illegal callback" proc
	     $avahi-err-invalid-object))))

;*---------------------------------------------------------------------*/
;*    avahi-service-resolver-close ...                                 */
;*---------------------------------------------------------------------*/
(define (avahi-service-resolver-close o::avahi-service-resolver)
   ($bgl-avahi-service-resolver-close o)
   (with-access::avahi-service-resolver o (client)
      (with-access::avahi-client client (resolvers)
	 (set! resolvers (remq! o resolvers))))
   #unspecified)
   
;*---------------------------------------------------------------------*/
;*    avahi-browser-event->symbol ...                                  */
;*---------------------------------------------------------------------*/
(define (avahi-browser-event->symbol event::$avahi-browser-event)
   (cond
      ((=fx event $avahi-browser-new)
       'avahi-browser-new)
      ((=fx event $avahi-browser-remove)
       'avahi-browser-remove)
      ((=fx event $avahi-browser-cache-exhausted)
       'avahi-browser-cache-exhausted)
      ((=fx event $avahi-browser-all-for-now)
       'avahi-browser-all-for-now)
      ((=fx event $avahi-browser-failure)
       'avahi-browser-failure)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-browser-event->symbol")
	     (msg "Unknown event")
	     (obj event)
	     (errno $avahi-err-invalid-object))))))

;*---------------------------------------------------------------------*/
;*    avahi-resolver-event->symbol ...                                 */
;*---------------------------------------------------------------------*/
(define (avahi-resolver-event->symbol event::$avahi-resolver-event)
   (cond
      ((=fx event $avahi-resolver-found)
       'avahi-resolver-found)
      ((=fx event $avahi-resolver-failure)
       'avahi-resolver-failure)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-resolver-event->symbol")
	     (msg "Unknown event")
	     (obj event)
	     (errno $avahi-err-invalid-object))))))

;*---------------------------------------------------------------------*/
;*    avahi-if-index->symbol ...                                       */
;*---------------------------------------------------------------------*/
(define (avahi-if-index->symbol index::$avahi-if-index)
   (cond
      ((=fx index $avahi-if-unspec) 'avahi-if-unspec)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-if-index->symbol")
	     (msg "Unknown index")
	     (obj index)
	     (errno $avahi-err-invalid-object))))))

;*---------------------------------------------------------------------*/
;*    avahi-protocol->symbol ...                                       */
;*---------------------------------------------------------------------*/
(define (avahi-protocol->symbol proto::$avahi-protocol)
   (cond
      ((=fx proto $avahi-proto-inet)
       'avahi-proto-inet)
      ((=fx proto $avahi-proto-inet6)
       'avahi-proto-inet6)
      ((=fx proto $avahi-proto-unspec)
       'avahi-proto-unspec)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-protocol->symbol")
	     (msg "Unknown protocol")
	     (obj proto)
	     (errno $avahi-err-invalid-object))))))

;*---------------------------------------------------------------------*/
;*    avahi-symbol->protocol ...                                       */
;*---------------------------------------------------------------------*/
(define (avahi-symbol->protocol proto::symbol)
   (case proto
      ((avahi-proto-inet)
       $avahi-proto-inet)
      ((avahi-proto-inet6)
       $avahi-proto-inet6)
      ((avahi-proto-unspec)
       $avahi-proto-unspec)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-symbol->protocol")
	     (msg "Unknown protocol")
	     (obj proto)
	     (errno $avahi-err-invalid-object))))))

;*---------------------------------------------------------------------*/
;*    avahi-lookup-flags->symbol ...                                   */
;*---------------------------------------------------------------------*/
(define (avahi-lookup-flags->symbol flag::$avahi-lookup-flags)
   (cond
      ((=fx flag $avahi-lookup-no-txt)
       'avahi-lookup-no-txt)
      ((=fx flag $avahi-lookup-no-address)
       'avahi-lookup-no-address)
      ((=fx flag 0)
       'avahi-lookup-default)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-lookup-flags->symbol")
	     (msg "Unknown lookup-flag")
	     (obj flag)
	     (errno $avahi-err-invalid-object))))))

;*---------------------------------------------------------------------*/
;*    avahi-lookup-result-flags->symbol ...                            */
;*---------------------------------------------------------------------*/
(define (avahi-lookup-result-flags->symbol flag::$avahi-lookup-result-flags)
   (cond
      ((=fx flag $avahi-lookup-result-cached)
       'avahi-lookup-result-cached)
      ((=fx flag $avahi-lookup-result-wide-area)
       'avahi-lookup-result-wide-area)
      ((=fx flag $avahi-lookup-result-multicast)
       'avahi-lookup-result-multicast)
      ((=fx flag $avahi-lookup-result-local)
       'avahi-lookup-result-local)
      ((=fx flag $avahi-lookup-result-our-own)
       'avahi-lookup-result-our-own)
      ((=fx flag $avahi-lookup-result-static)
       'avahi-lookup-result-static)
      ((=fx flag 0)
       'avahi-lookup-result-default)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-lookup-result-flags->symbol")
	     (msg "Unknown lookup-result-flag")
	     (obj flag)
	     (errno $avahi-err-invalid-object))))))

;*---------------------------------------------------------------------*/
;*    avahi-symbol->domain-browser-type ...                            */
;*---------------------------------------------------------------------*/
(define (avahi-symbol->domain-browser-type btype)
   (case btype
      ((eq? btype 'avahi-domain-browser-browse)
       $avahi-domain-browser-browse)
      ((eq? btype 'avahi-domain-browser-browse-default)
       $avahi-domain-browser-browse-default)
      ((eq? btype 'avahi-domain-browser-register)
       $avahi-domain-browser-register)
      ((eq? btype 'avahi-domain-browser-register-default)
       $avahi-domain-browser-register-default)
      ((eq? btype 'avahi-domain-browser-browse-legacy)
       $avahi-domain-browser-browse-legacy)
      ((eq? btype 'avahi-domain-browser-max)
       $avahi-domain-browser-max)
      (else
       (raise
	  (instantiate::&avahi-error
	     (proc "avahi-symbol->domain-browser-type")
	     (msg "Unknown btype")
	     (obj btype)
	     (errno $avahi-err-invalid-object))))))
