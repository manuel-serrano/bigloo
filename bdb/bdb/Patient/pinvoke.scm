;*=====================================================================*/
;*    serrano/prgm/project/bdk/kbdb/src/Patient/pinvoke.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 17:02:51 1999                          */
;*    Last change :  Wed Aug  9 23:08:46 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Invoking the patient.                                            */
;*    -------------------------------------------------------------    */
;*    To invoke the patient, we go through gdb, we construct a         */
;*    call command. When calling a patient function, we send a         */
;*    port to establish a connection by the means of socket on         */
;*    that port.                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module patient_invoke
   (import engine_param
	   patient_server
	   gdb_invoke
	   gdb_annotate)
   (export (patient-call fun::bstring . args)))

;*---------------------------------------------------------------------*/
;*    patient-call ...                                                 */
;*    -------------------------------------------------------------    */
;*    Call a service from the patient a read the result via a socket.  */
;*---------------------------------------------------------------------*/
(define (patient-call fun . args)
   (if (not (gdb-annotate-running?))
       ;; when we are not running the function, we cannot request for
       ;; the patient help.
       #f
       ;; first, we construct the command line
       (let* ((pnum (integer->string (get-server-number)))
	      (cmd  (match-case args
		       (()
			(string-append "output " fun "(" pnum ")"))
		       ((?str)
			(string-append "output " fun "(" pnum "," str ")"))
		       ((?str ?arg)
			(string-append "output " fun "(" pnum "," str "," arg ")"))
		       (else
			(let loop ((cmd (reverse args))
				   (res ")"))
			   (if (null? (cdr cmd))
			       (string-append "output " fun "(" pnum ","
					      (car cmd) res)
			       (loop (cdr cmd)
				     (string-append ", " (car cmd) res))))))))
	  ;; We now call gdb non displaying its result. We check if an error
	  ;; was raised in order to find out if we can read the client answer
	  (gdb-disable-print!)
	  (gdb-server->string cmd)
	  (if (not (gdb-annotate-error?))
	      ;; we now read on the socket for the client answer
	      (read-client-reply)
	      #f))))
	  

