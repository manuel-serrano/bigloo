;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Gdb/proc.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 09:34:22 1999                          */
;*    Last change :  Sun Jul 30 09:01:26 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Starting and stopping the embedded GDB process.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gdb_proc
   (include "Engine/hooks.sch")
   (import  engine_param
	    command_file
	    tools_error
	    tools_speek
	    tools_io
	    gdb_invoke
	    gdb_autoconf
	    patient_mangling)
   (extern  (isatty?::bool    (::int)          "isatty")
	    (ttyname::string  (::int)          "ttyname")
	    (tty-init::string ()               "tty_init")
	    (tty-read::string ()               "tty_read")
	    (tty-write::int   (::string ::int) "tty_write"))
   (export  (gdb-start)
	    (gdb-stop)
	    (get-gdb-process)
	    (read-process-tty-output)
	    (write-process-tty-output str::bstring)))

;*---------------------------------------------------------------------*/
;*    *gdb-process* ...                                                */
;*---------------------------------------------------------------------*/
(define *gdb-process* #unspecified)

;*---------------------------------------------------------------------*/
;*    get-gdb-process ...                                              */
;*---------------------------------------------------------------------*/
(define (get-gdb-process)
   *gdb-process*)

;*---------------------------------------------------------------------*/
;*    gdb-start ...                                                    */
;*    -------------------------------------------------------------    */
;*    Start a new gdb process.                                         */
;*---------------------------------------------------------------------*/
(define (gdb-start)
   ;; we start running gdb
   (if (> *verbose* 3)
       (verbose 4 "spawning : "
		(let ((port (open-output-string)))
		   (display *gdb* port)
		   (display " " port)
		   (for-each (lambda (x)
				(display x port)
				(display " " port))
			     (gdb-options-list))
		   (close-output-port port))
		#\Newline))
   (set! *gdb-process* (apply run-process
			      *gdb*
			      output: pipe:
			      input: pipe:
			      error: pipe:
			      (gdb-options-list)))
   (if (or (not (process? *gdb-process*))
	   (not (process-alive? *gdb-process*)))
       (bdb-error "gdb-start" "Can't start process" *gdb*)
       (begin
	  ;; initial gdb hook
	  (run-hooks *gdb-run-hook* *gdb-process*)
	  ;; flush
	  (flush-gdb-pending-output #t)
	  ;; we silently change the gdb configuration
	  (gdb-server->string "set print repeat -1"))))

;*---------------------------------------------------------------------*/
;*    gdb-stop ...                                                     */
;*---------------------------------------------------------------------*/
(define (gdb-stop)
   (if (and (process? *gdb-process*) (process-alive? *gdb-process*))
       (process-kill *gdb-process*)))

;*---------------------------------------------------------------------*/
;*    *tty-process?* ...                                               */
;*    -------------------------------------------------------------    */
;*    Is the running process using our own tty ?                       */
;*---------------------------------------------------------------------*/
(define *tty-process?* #f)

;*---------------------------------------------------------------------*/
;*    gdb-options-list ...                                             */
;*    -------------------------------------------------------------    */
;*    Build the standard gdb argument list. That list is the standard  */
;*    *GDB-OPTIONS* plus the information about the tty to be used by   */
;*    the process.                                                     */
;*---------------------------------------------------------------------*/
(define (gdb-options-list)
   (define (ttyopt)
      (let ((tname (and *force-tty?*
			(let ((name (tty-init)))
			   (if (run-hooks *tty-init-hook*)
			       name
			       #f)))))
	 (if (and (string? tname) (>fx (string-length tname) 0))
	     ;; we have successfully opened a tty
	     (begin
		(set! *tty-process?* #t)
		(string-append "--tty=" tname))
	     ;; we have been unable to open a tty
	     (begin
		(if (isatty? 0)
		    (let ((tty (ttyname 0)))
		       (string-append "--tty=" tty))
		    (error "gdb-options-list"
			   "Can't find any tty."
			   "Exiting."))))))
   (let ((opts (cons* (ttyopt) *gdb-options*)))
      (if (string? *exec*)
	  (begin
	     (reset-demangling-cache!)
	     (set-previous-file! *exec*)
	     (cons *exec* opts))
	  opts)))

;*---------------------------------------------------------------------*/
;*    read-process-tty-output ...                                      */
;*---------------------------------------------------------------------*/
(define (read-process-tty-output)
   (if (not *tty-process?*)
       #t
       (let ((str (tty-read)))
	  (if (>fx (string-length str) 0)
	      str
	      #f))))

;*---------------------------------------------------------------------*/
;*    write-process-tty-output ...                                     */
;*---------------------------------------------------------------------*/
(define (write-process-tty-output str::bstring)
   (if (not *tty-process?*)
       (error "write-process-tty-output"
	      "Can't write process input value"
	      str)
       (let ((len (string-length str)))
	  (tty-write str len))))
