;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/avahi/examples/publish.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 14 08:42:43 2011                          */
;*    Last change :  Wed Jul  4 08:40:37 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Publish an AVAHI service mimicking the avahi-publish.c example.  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module avahi_publish
   (library avahi pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    name ...                                                         */
;*---------------------------------------------------------------------*/
(define name "Megaprint")

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (avahi-publish))

;*---------------------------------------------------------------------*/
;*    avahi-publish ...                                                */
;*---------------------------------------------------------------------*/
(define (avahi-publish)
   (let* ((poll (instantiate::avahi-simple-poll))
	  (client (instantiate::avahi-client
		     (proc client-callback)
		     (poll poll))))
      (unwind-protect
	 (let ((th (instantiate::pthread
		      (body (lambda ()
			       (avahi-simple-poll-loop poll))))))
	    (thread-start-joinable! th)
	    (sleep 10000000)	
	    (modify client)
	    (thread-join! th))
	 (begin
	    (when group (avahi-entry-group-close group))
	    (avahi-client-close client)
	    (avahi-simple-poll-close poll)))))

;*---------------------------------------------------------------------*/
;*    group ...                                                        */
;*---------------------------------------------------------------------*/
(define group #f)

;*---------------------------------------------------------------------*/
;*    client-callback ...                                              */
;*---------------------------------------------------------------------*/
(define (client-callback client::avahi-client state::symbol)
   (case state
      ((avahi-client-running)
       (create-services client))
      ((avahi-client-failure)
       (let ((msg (avahi-client-error-message client)))
	  (avahi-simple-poll-quit (-> client poll))
	  (avahi-error "client-callback" msg client -1)))
      ((avahi-client-collision avahi-client-registering avahi-client-connecting)
       #unspecified)
      (else
       (fprintf (current-error-port) "Unknown client state \"~a\"." state))))

;*---------------------------------------------------------------------*/
;*    create-services ...                                              */
;*---------------------------------------------------------------------*/
(define (create-services client)

   (unless group
      (set! group
	 (instantiate::avahi-entry-group
	    (proc entry-group-callback)
	    (client client))))

   (when (avahi-entry-group-empty? group)
      (with-handler
	 (lambda (e)
	    (if (isa? e &avahi-collision-error)
		(begin
		   (set! name (avahi-alternative-service-name name))
		   (avahi-entry-group-reset! group)
		   (create-services client))
		(raise e)))
	 (begin
	    ;; add the service for ipp
	    (avahi-entry-group-add-service! group
	       :name name
	       :type "_ipp._tcp"
	       :port 651
	       "test=blah"
	       (format "random=~a" (random 100)))
	    ;; add the service for BSD LPR
	    (avahi-entry-group-add-service! group
	       :name name
	       :type "_printer._tcp"
	       :port 515)
	    ;; add and additional (hypothetic) subtype
	    (avahi-entry-group-add-service! group
	       :name name
	       :type "_printer._tcp"
	       :subtype "_magic._sub._printer._tcp")
	    ;; tell the server to register the service
	    (avahi-entry-group-commit group)))))

;*---------------------------------------------------------------------*/
;*    entry-group-callback ...                                         */
;*---------------------------------------------------------------------*/
(define (entry-group-callback egroup::avahi-entry-group state::symbol)
   (case state
      ((avahi-entry-group-established)
       (fprintf (current-error-port) "Service '~a' successfully established.\n"
	  name))
      ((avahi-entry-group-collision)
       (set! name (avahi-alternative-service-name name))
       (fprintf (current-error-port) "Service name collision, renaming service to '~a'\n" name)
       (create-services (-> egroup client)))
      ((avahi-entry-group-failure)
       (fprintf (current-error-port) "Entry group failure: ~a"
	  (avahi-client-error-message (-> egroup client))))
      ((avahi-entry-group-uncommited avahi-entry-group-registering)
       #unspecified)
      (else
       (fprintf (current-error-port) "Unknown entry-group state \"~a\"\n" state))))

;*---------------------------------------------------------------------*/
;*    modify ...                                                       */
;*---------------------------------------------------------------------*/
(define (modify client::avahi-client)
   (set! name "Modified MegaPrinter")
   (tprint "state=" (-> client state))
   (when (eq? (-> client state) 'avahi-client-running)
      (tprint "GROUP=" (typeof group))
      (when group
	 (avahi-entry-group-reset! group)
	 (create-services client))))
