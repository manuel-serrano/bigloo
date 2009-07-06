;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Patient/server.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 17:12:42 1999                          */
;*    Last change :  Sun Mar 20 14:54:45 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Establish a socket server to manage to talk the patient.         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module patient_server
   (import tools_error
	   tools_speek
	   gdb_invoke)
   (export (close-server!)
	   (read-client-reply)
	   (get-server-number)))

;*---------------------------------------------------------------------*/
;*    *server* ...                                                     */
;*---------------------------------------------------------------------*/
(define *server* #unspecified)
(define *client* #f)
(define *connected?* #f)

;*---------------------------------------------------------------------*/
;*    get-server-number ...                                            */
;*---------------------------------------------------------------------*/
(define (get-server-number)
   (if (socket? *server*)
       (socket-port-number *server*)
       (begin
	  (init-server!)
	  (get-server-number))))

;*---------------------------------------------------------------------*/
;*    read-client-reply ...                                            */
;*---------------------------------------------------------------------*/
(define (read-client-reply)
   ;; wait for the client connection
   (if (not *client*)
       (begin
	  (verbose 4 #"waiting for connection...")
	  (flush-output-port (current-output-port))
	  (set! *client* (socket-accept *server*))
	  (verbose 4 #"ok.\n")))
   ;; then read the client output
   (let* ((kind (read (socket-input *client*))))
      (verbose 4 "kind [" kind "]" #\Newline)
      (cond
	 ((boolean? kind)
	  ;; short transfer are directly sent
	  (let ((reply (read (socket-input *client*))))
	     (if kind
		 (begin
		    (verbose 4 "Read (short) from patient [" reply #"]\n")
		    (flush-output-port (socket-output *client*))
		    reply)
		 (let ((reply (string->obj reply)))
		    (verbose 4 "Read (short) from patient [" reply #"]\n")
		    (flush-output-port (socket-output *client*))
		    reply))))
	 ((eq? kind 'file)
	  ;; long data are sent via an file
	  (let* ((fname (read (socket-input *client*)))
		 (res   (with-input-from-file fname
			   (lambda ()
			      (read)))))
	     (verbose 4 "Read (long) from patient [" res #"]\n")
	     (delete-file fname)
	     res))
	 (else
	  ;; else, it is an error
	  (let ((p (open-output-string)))
	     (write kind p)
	     (error "read-client-reply"
		    "Illegal reply"
		    (close-output-port p)))))))

;*---------------------------------------------------------------------*/
;*    init-server! ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function setup the socker server that will communicate      */
;*    with the bigloo running process (the client).                    */
;*    -------------------------------------------------------------    */
;*    This function is called the first time there is a need for       */
;*    the server port number. This is why this particular function     */
;*    is not exported.                                                 */
;*---------------------------------------------------------------------*/
(define (init-server!)
   (if (not (socket? *server*))
       (begin
	  (set! *server* (make-server-socket))
	  (if (not (socket? *server*))
	      (bdb-error "bdb" "Can't setup server" *server*)))))

;*---------------------------------------------------------------------*/
;*    close-server! ...                                                */
;*    -------------------------------------------------------------    */
;*    We are done with bdb, we stop the server.                        */
;*    -------------------------------------------------------------    */
;*    This function is only called once, on termination of BDB.        */
;*---------------------------------------------------------------------*/
(define (close-server!)
   (if (socket? *server*)
       (begin
	  (verbose 4 #"Closing server.\n")
	  (socket-shutdown *server*)
	  (set! *client* #f)
	  (set! *server* #unspecified))))


