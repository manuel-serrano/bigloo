;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/avahi/examples/browse.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 14 14:28:34 2011                          */
;*    Last change :  Thu Jul 12 14:46:03 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Browse an AVAHI server mimicking the avahi-browse.c example.     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module avahi_browse
   (library avahi pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let* ((poll (instantiate::avahi-threaded-poll))
	  (client (instantiate::avahi-client
		     (poll poll)
		     (proc client-callback)))
	  (sb (if (pair? (cdr argv))
		  (instantiate::avahi-service-browser
		     (client client)
		     (type (cadr argv))
		     (proc service-browser-callback))
		  (instantiate::avahi-service-type-browser
		     (client client)
		     (proc type-browser-callback)))))
      (avahi-threaded-poll-loop poll)
      (read)))

;*---------------------------------------------------------------------*/
;*    client-callback ...                                              */
;*---------------------------------------------------------------------*/
(define (client-callback client::avahi-client state::symbol)
   (when (eq? state 'avahi-client-failure)
      (avahi-threaded-poll-quit (-> client poll))))

;*---------------------------------------------------------------------*/
;*    service-browser-callback ...                                     */
;*---------------------------------------------------------------------*/
(define (service-browser-callback browser::avahi-service-browser
	   interface protocol event name type domain flags)
   (let* ((client::avahi-client (-> browser client))
	  (poll::avahi-threaded-poll (-> client poll)))
      (avahi-threaded-poll-lock! poll)
      (case event
	 ((avahi-browser-failure)
	  'todo)
	 ((avahi-browser-new)
	  (let ((resolver (instantiate::avahi-service-resolver
			     (client client)
			     (interface interface)
			     (protocol protocol)
			     (name name)
			     (type type)
			     (domain domain)
			     (proc resolver-callback))))
	     resolver))
	 ((avahi-browser-remove)
	  'todo)
	 ((avahi-browser-all-for-now avahi-browser-cache-exhausted)
	  'todo)
	 (else
	  (fprintf (current-error-port) "Unknown browser event \"~a\"\n" event)))
      (avahi-threaded-poll-unlock! poll)))
   
;*---------------------------------------------------------------------*/
;*    type-browser-callback ...                                        */
;*---------------------------------------------------------------------*/
(define (type-browser-callback browser::avahi-service-type-browser
	   interface protocol event type domain flags)
   (let* ((client::avahi-client (-> browser client))
	  (poll::avahi-threaded-poll (-> client poll)))
      (avahi-threaded-poll-lock! poll)
      (case event
	 ((avahi-browser-failure)
	  'todo)
	 ((avahi-browser-new)
	  (instantiate::avahi-service-browser
	     (client client)
	     (type type)
	     (proc service-browser-callback)))
	 ((avahi-browser-remove)
	  'todo)
	 ((avahi-browser-all-for-now avahi-browser-cache-exhausted)
	  'todo)
	 (else
	  (fprintf (current-error-port) "Unknown browser event \"~a\"\n" event)))
      (avahi-threaded-poll-unlock! poll)))
   
;*---------------------------------------------------------------------*/
;*    resolver-callback ...                                            */
;*---------------------------------------------------------------------*/
(define (resolver-callback resolver::avahi-service-resolver
	   interface protocol event name type domain hostname address port
	   txtlst flags)
   (let* ((client::avahi-client (-> resolver client))
	  (poll::avahi-threaded-poll (-> client poll)))
      (avahi-threaded-poll-lock! poll)
      (case event
	 ((avahi-resolver-failure)
	  (fprintf (current-error-port) "Failed to resolve service '~a' of type '~a' in domain '~a': ~a\n"
	     name
	     type
	     domain
	     (avahi-client-error-message client)))
	 ((avahi-resolver-found)
	  (fprintf (current-error-port)
	     "Service '~a' of type '~a' in domain '~a':\n" name type domain)
	  (fprintf (current-error-port)
	     "\t~a:~a (~a)\n\tTXT=~l\n"
	     hostname
	     port
	     address
	     txtlst)))
      (avahi-service-resolver-close resolver)
      (avahi-threaded-poll-unlock! poll)))


