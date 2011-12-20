;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/avahi/examples/browse.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 14 14:28:34 2011                          */
;*    Last change :  Mon Dec 19 10:24:48 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
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
   (let* ((poll (instantiate::avahi-simple-poll))
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
      (avahi-simple-poll-loop poll)))

;*---------------------------------------------------------------------*/
;*    client-callback ...                                              */
;*---------------------------------------------------------------------*/
(define (client-callback client::avahi-client state::symbol)
   (when (eq? state 'avahi-client-failure)
      (avahi-simple-poll-quit (-> client poll))))

;*---------------------------------------------------------------------*/
;*    service-browser-callback ...                                     */
;*---------------------------------------------------------------------*/
(define (service-browser-callback browser::avahi-service-browser
	   interface protocol event name type domain flags)
   (let ((client::avahi-client (-> browser client)))
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
	  (fprintf (current-error-port) "Unknown browser event \"~a\"\n" event)))))
   
;*---------------------------------------------------------------------*/
;*    type-browser-callback ...                                        */
;*---------------------------------------------------------------------*/
(define (type-browser-callback browser::avahi-service-type-browser
	   interface protocol event type domain flags)
   (let ((client::avahi-client (-> browser client)))
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
	  (fprintf (current-error-port) "Unknown browser event \"~a\"\n" event)))))
   
;*---------------------------------------------------------------------*/
;*    resolver-callback ...                                            */
;*---------------------------------------------------------------------*/
(define (resolver-callback resolver::avahi-service-resolver
	   interface protocol event name type domain hostname address port
	   txtlst flags)
   (let ((client::avahi-client (-> resolver client)))
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
      (avahi-service-resolver-close resolver)))

