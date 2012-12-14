;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Gdb/gread.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug  9 08:43:03 2000                          */
;*    Last change :  Fri Dec 14 13:37:58 2012 (serrano)                */
;*    Copyright   :  2000-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Read GDB outputs.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gdb_read
   (import tools_speek
	   engine_param
	   tools_tools
	   gdb_proc
	   tools_io)
   (export (read-unbuffered-line iport::input-port ::bool ::bool)
	   (gdb-read-error)
	   (gdb-read-line::bstring ::int)))

;*---------------------------------------------------------------------*/
;*    *buffer* ...                                                     */
;*---------------------------------------------------------------------*/
(define *buffer* (make-string 255 #\-))

;*---------------------------------------------------------------------*/
;*    enlarge-buffer! ...                                              */
;*---------------------------------------------------------------------*/
(define (enlarge-buffer!)
   (let* ((old-len (string-length *buffer*))
	  (new-len (*fx old-len 2))
	  (new-buf (make-string new-len)))
      (blit-string! *buffer* 0 new-buf 0 old-len)
      (set! *buffer* new-buf)))

;*---------------------------------------------------------------------*/
;*    input-port->file ...                                             */
;*---------------------------------------------------------------------*/
(define (input-port->file::file iport::input-port)
   (pragma::file "PORT_FILE( $1 )" iport))

;*---------------------------------------------------------------------*/
;*    getc ...                                                         */
;*---------------------------------------------------------------------*/
(define (getc::char file::file block?)
   (pragma "extern int (*bdb_getc)(FILE *)")
   (if block?
       (free-pragma::char "getc( $1 )" file)
       (free-pragma::char "bdb_getc( $1 )" file)))

;*---------------------------------------------------------------------*/
;*    read-unbuffered-line ...                                         */
;*    -------------------------------------------------------------    */
;*    Read a line unbuffered.                                          */
;*    -------------------------------------------------------------    */
;*    If PROMPT? is #t, then, the input are stopped when a prompt      */
;*    is found.                                                        */
;*---------------------------------------------------------------------*/
(define (read-unbuffered-line iport::input-port block? prompt?)
   (let ((file::file (input-port->file iport))
	 (plen (-fx (string-length *gdb-prompt*) 1)))
      (bdb-log 7 "read-unbuffered-line: [")
      (let loop ((i   0)
		 (len (string-length *buffer*))
		 (buf *buffer*))
	 (if (=fx i len)
	     (begin 
		(enlarge-buffer!)
		(loop i (string-length *buffer*) *buffer*))
	     (let ((c (getc file block?)))
		(bdb-log 7 c)
		(cond
		   ((char=? c #\Newline)
		    (bdb-log 7 #"]\n")
		    (string-set! buf i c)
		    buf)
		   ((and prompt?
			 (char=? c #\space)
			 (=fx i plen)
			 (substring=? *gdb-prompt* buf (-fx plen 1)))
		    (bdb-log 7 #"]<-- prompt\n")
		    *gdb-prompt*)
		   ((char=? c (pragma::char "EOF"))
		    (error *bdb-name*
			   "Gdb"
			   "Illegal eof of line character"))
		   (else
		    (string-set! buf i c)
		    (loop (+fx i 1)
			  len buf))))))))
	     
;*---------------------------------------------------------------------*/
;*    gdb-read-error ...                                               */
;*    -------------------------------------------------------------    */
;*    This function can be called only when an error markup has been   */
;*    read. This indicates that we should read until the               */
;*    ^Z^Zerror-begin annotations, then, read the next line and then   */
;*    stop.                                                            */
;*---------------------------------------------------------------------*/
(define (gdb-read-error)
   (let* ((proc (get-gdb-process))
	  (perr (process-error-port proc))
	  (line (read-unbuffered-line perr #t #f)))
      (verbose 5 "gdb-read-error ["
	       (string-for-read (string-until line #\Newline 1))
	       #"]\n")
      (let* ((<m> #"error-begin\n")
	     (<m-len> (string-length <m>)))
	 (let loop ((l (read-unbuffered-line perr #t #f)))
	    (verbose 5 "gdb-read-error [l:"
		     (string-for-read (string-until line #\Newline 1))
		     #"]\n")
	    (if (not (substring=? <m> l <m-len>))
		(begin
		   (console-error (string-until l #\Newline 1))
		   (loop (read-unbuffered-line perr #t #f)))
		(let laap ((l (read-unbuffered-line perr #t #f)))
		   (verbose 5 "gdb-read-error [l:"
			    (string-for-read (string-until line #\Newline 1))
			    #"]\n")
		   (if (not (char=? (string-ref l 0) #\Newline))
		       (string-until l #\Newline 1)
		       (laap (read-unbuffered-line perr #t #f)))))))))

;*---------------------------------------------------------------------*/
;*    gdb-read-line ...                                                */
;*    -------------------------------------------------------------    */
;*    Read one additional line in blocking mode, prompt unbound.       */
;*---------------------------------------------------------------------*/
(define (gdb-read-line::bstring delta)
   (let* ((proc (get-gdb-process))
	  (pin  (process-output-port proc))
	  (line (string-until (read-unbuffered-line pin #f #f)
 			      #\Newline
			      delta)))
      (verbose 5 "gdb-read-line [" line #"]\n")
      (bdb-log 5 "gdb-read-line [" line #"]\n")
      line))
   
