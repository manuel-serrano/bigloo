;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/dbus/recette/recette.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Sun Nov  8 14:42:02 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of Dbus.                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library dbus)
   (main    main))

;*---------------------------------------------------------------------*/
;*    err ...                                                          */
;*---------------------------------------------------------------------*/
(define (err . msg)
   (with-output-to-port (current-error-port)
      (lambda ()
	 (for-each write msg)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    do-something-else ...                                            */
;*---------------------------------------------------------------------*/
(define (do-something-else)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    *tests* ...                                                      */
;*---------------------------------------------------------------------*/
(define *tests* '())

;*---------------------------------------------------------------------*/
;*    *failure* and *success* ...                                      */
;*---------------------------------------------------------------------*/
(define *failure* '())
(define *success* 0)

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
  (display* name "...")
  (flush-output-port (current-output-port))
  (let ((provided (with-handler
		   (lambda (e)
		     (error-notify e)
		     (vector res))
		   (prgm))))
    (if (or (eq? res #unspecified)
	    (and (procedure? res) (res provided))
	    (equal? res provided))
	(begin
	  (set! *success* (+fx 1 *success*))
	  (print "ok."))
	(begin
	  (set! *failure* (cons name *failure*))
	  (print "error.")
	  (print "   ==> provided: [" (with-output-to-string
					(lambda () (write provided)))
		 "]\n       expected: ["
		 (let ((r (if (procedure? res) (res 'result) res)))
		   (with-output-to-string
		     (lambda () (write r))))
		 "]")))))

;*---------------------------------------------------------------------*/
;*    define-test ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (define-test id prgm . rest)
   (let ((t (match-case rest
	       ((:result ?result)
		`(list ',id (lambda () ,prgm) ,result))
	       (()
		`(list ',id (lambda () ,prgm) #unspecified))
	       (else
		(error "define-test" "Illegal rest argument" rest)))))
      `(set! *tests* (cons ,t *tests*))))

;*---------------------------------------------------------------------*/
;*    cond-expand ...                                                  */
;*---------------------------------------------------------------------*/
(define-test cond-expand
   (cond-expand
      (dbus #t)
      (else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    connect-bus ...                                                  */
;*---------------------------------------------------------------------*/
(define-test connect-bus
   (let ((bus (dbus-connect 'session)))
      (let ((conn (dbus-connected? bus)))
	 (dbus-close bus)
	 conn))
   :result #t)

;*---------------------------------------------------------------------*/
;*    connect-bus.2 ...                                                */
;*---------------------------------------------------------------------*/
(define-test connect-bus.2
   (let ((bus (dbus-connect 'session :path "/" :interface "bgl.dbus")))
      (unwind-protect
	 (dbus-connected? bus)
	 (dbus-close bus)))
   :result #t)

;*---------------------------------------------------------------------*/
;*    close-bus ...                                                    */
;*---------------------------------------------------------------------*/
(define-test close-bus
   (let ((bus (dbus-connect 'session)))
      (and (unwind-protect
	      (dbus-connected? bus)
	      (dbus-close bus))
	   (not (dbus-connected? bus))))
   :result #t)

;* {*---------------------------------------------------------------------*} */
;* {*    path ...                                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test path                                                   */
;*    (let ((bus (dbus-connect 'session)))                             */
;*       (unwind-protect                                               */
;* 	 (begin                                                        */
;* 	    (dbus-path-set! bus "/")                                   */
;* 	    (dbus-path bus))                                           */
;* 	 (dbus-close bus)))                                            */
;*    :result "/")                                                     */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    set-interface ...                                                *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test set-interface                                          */
;*   (let ((bus (dbus-connect 'session)))                              */
;*     (dbus-set-interface bus "bgl.dbus")                             */
;*     (dbus-close bus)))                                              */
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    bus-name-has-owner ...                                           */
;*---------------------------------------------------------------------*/
(define-test bus-name-has-owner
   (let ((bus (dbus-connect 'system)))
      (unwind-protect
	 (dbus-bus-name-has-owner? bus "org.freedesktop.DBus")
	 (dbus-close bus)))
   :result #t)

(define-test bus-name-has-owner
   (let ((bus (dbus-connect 'system)))
      (unwind-protect
	 (dbus-bus-name-has-owner? bus "org.freedesktop.Foo")
	 (dbus-close bus)))
   :result #f)

;*---------------------------------------------------------------------*/
;*    set-service-name ...                                             */
;*---------------------------------------------------------------------*/
(define-test set-service-name
   (let ((bus (dbus-connect 'session)))
      (unwind-protect
	 (begin
	    (dbus-bus-name-set! bus "blg.dbus.test.recette")
	    (dbus-bus-name-has-owner? bus "blg.dbus.test.recette"))
	 (dbus-close bus)))
   :result #t)

(define-test set-service-name
   (let ((bus (dbus-connect 'session)))
      (unwind-protect
	 (begin
	    (dbus-bus-name-set! bus "blg.dbus.test.recette")
	    (dbus-bus-release-name! bus "blg.dbus.test.recette")
	    (dbus-bus-name-has-owner? bus "blg.dbus.test.recette"))
	 (dbus-close bus)))
   :result #f)

;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    release-service-name                                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test release-service-name                                   */
;*   (let ((bus (dbus-connect 'session)))                              */
;*     (dbus-set-service-name bus "blg.dbus.test.recette")             */
;*     (dbus-release-service-name bus)                                 */
;*     (set! *res* (not (dbus-service-name-has-owner bus "bgl.dbus.test.recette"))) */
;*     (dbus-close bus)                                                */
;*     *res*)                                                          */
;*   :result (lambda (v)                                               */
;* 	    *res*))                                                    */
;*                                                                     */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    send-method-call ...                                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test send-method-call                                       */
;*   (let* ((bus (dbus-connect 'system))                               */
;*          (service (dbus-handle-service bus "org.freedesktop.Hal"))  */
;*          (object (dbus-handle-object service "/org/freedesktop/Hal/Manager" "org.freedesktop.Hal.Manager"))) */
;*     (set! *res* (dbus-send-method-call object "FindDeviceByCapability" '("volume"))) */
;*     (dbus-close bus))                                               */
;*   :result (lambda (v)                                               */
;* 	    *res*))                                                    */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    get-args-from-message ...                                        *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test get-args-from-message                                  */
;*   (let* ((bus (dbus-connect 'system))                               */
;*          (service (dbus-handle-service bus "org.freedesktop.Hal"))  */
;*          (object (dbus-handle-object service "/org/freedesktop/Hal/Manager" "org.freedesktop.Hal.Manager"))) */
;*     (set! *res* (dbus-get-args-from-message                         */
;* 		 (dbus-send-method-call object "FindDeviceByCapability" '("volume")))) */
;*     (dbus-close bus)                                                */
;*     *res*)                                                          */
;*   :result (lambda (v)                                               */
;* 	    *res*))                                                    */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    send-signal ...                                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test send-signal                                            */
;*   (let ((bus (dbus-connect 'session)))                              */
;*     (dbus-send-signal bus "mysignal")                               */
;*     (dbus-close bus)))                                              */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    introspect-object ...                                            *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test introspect-object                                      */
;*   (let* ((bus (dbus-connect 'system))                               */
;*          (service (dbus-handle-service bus "org.freedesktop.Hal"))  */
;*          (object (dbus-handle-object service "/org/freedesktop/Hal/Manager" "org.freedesktop.Hal.Manager"))) */
;*     (set! *res* (dbus-introspect-object object))                    */
;*     (dbus-close bus)                                                */
;*     *res*)                                                          */
;*   :result (lambda (v)                                               */
;* 	    *res*))                                                    */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    message-type ...                                                 *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test message-type                                           */
;*   (let* ((bus (dbus-connect 'system))                               */
;*          (service (dbus-handle-service bus "org.freedesktop.Hal"))  */
;*          (object (dbus-handle-object service "/org/freedesktop/Hal/Manager" "org.freedesktop.Hal.Manager"))) */
;*     (set! *res* (equal? 'dbus-method-return (dbus-message-type (dbus-send-method-call object "FindDeviceByCapability" '("volume"))))) */
;*     (dbus-close bus)                                                */
;*     *res*)                                                          */
;*   :result (lambda (v)                                               */
;* 	    *res*))                                                    */
;*                                                                     */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    get-sender ...                                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test get-sender                                             */
;*   (let* ((bus (dbus-connect 'system))                               */
;*          (service (dbus-handle-service bus "org.freedesktop.Hal"))  */
;*          (object (dbus-handle-object service "/org/freedesktop/Hal/Manager" "org.freedesktop.Hal.Manager"))) */
;*     (set! *res* (dbus-get-sender (dbus-send-method-call object "FindDeviceByCapability" '("volume")))) */
;*     (dbus-close bus)                                                */
;*     *res*)                                                          */
;*   :result (lambda (v)                                               */
;* 	    *res*))                                                    */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    get-path ...                                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define-test get-path                                               */
;*   (let* ((bus (dbus-connect 'system))                               */
;*          (service (dbus-handle-service bus "org.freedesktop.Hal"))  */
;*          (object (dbus-handle-object service "/org/freedesktop/Hal/Manager" "org.freedesktop.Hal.Manager"))) */
;*     (set! *res* (dbus-get-path (dbus-send-method-call object "FindDeviceByCapability" '("volume")))) */
;*     (dbus-close bus)                                                */
;*     *res*)                                                          */
;*   :result (lambda (v)                                               */
;* 	    *res*))                                                    */

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
	 (else
	  (set! tests (cons (string->symbol else) tests))))
      ;; run all the tests
      (unwind-protect
	 (for-each (lambda (pvn)
		      (apply test pvn))
		   (if (null? tests)
		       (reverse *tests*)
		       (reverse (filter (lambda (t) (memq (car t) tests))
					*tests*))))
	 (when (file-exists? "test.db") (delete-file "test.db")))
      ;; if we reach that point, we are done
      (print "\n"
	     (if (null? tests) "All" (reverse tests))
	     " tests executed...\n"
	     (if (null? *failure*)
		 "all succeeded"
		 (format " ~a succeeded\n ~a failed ~a"
			 *success*
			 (length *failure*)
			 (reverse *failure*))))))


   

