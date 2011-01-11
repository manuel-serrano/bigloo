;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/blib/bdb.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 07:10:43 1999                          */
;*    Last change :  Tue Jan 11 07:14:05 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bdb library                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bdb
   
   (extern (macro bdb-printf::int (::string ::int) "printf")
	   (macro tmpnam::string (::long) "tmpnam")
	   (bdb-heap-size::int () "bdb_heap_size")

	   (export bdb-close-client! "bdb_close_client")
	   (export mangle "bdb_mangle")
	   (export mangle-for-funcall "bdb_mangle_for_funcall")
	   (export mangle2 "bdb_mangle2")
	   (export demangle "bdb_demangle")
	   (export demangle2 "bdb_demangle2")
	   (export output-value "bdb_output_value")
	   (export output-classes "bdb_output_classes")
	   (export dump-classes "bdb_dump_classes")
	   (export bdb-print "bdb_print")
	   (export bdb-whatis "bdb_whatis")
	   (export bdb-funcall "bdb_funcall")
	   (export bdb-bint "bdb_bint")
	   (export bdb-bchar "bdb_bchar")
	   (export bdb-true "bdb_true")
	   (export bdb-false "bdb_false")
	   (export bdb-nil "bdb_nil")
	   (export bdb-find-type "bdb_find_type")
	   (export bdb-init! "bdb_s"))
	   
   (import __bdb_env)
   
   (export (bdb-init!)
	   (bdb-initial-breakpoint)
	   (bdb-close-client!)
	   (mangle::int ::int ::string)
	   (mangle-for-funcall::int ::int ::string)
	   (mangle2::int ::int ::string ::string)
	   (demangle::int ::int ::string)
	   (demangle2::int ::int ::string ::string)
	   (output-value::int ::int ::obj ::bool)
	   (output-classes::int)
	   (dump-classes::int ::int)
	   (bdb-print::string ::obj ::bool)
	   (bdb-whatis::int ::int ::obj)
	   (bdb-funcall::obj ::procedure ::pair-nil)
	   (bdb-bint ::bint)
	   (bdb-bchar ::bchar)
	   (bdb-true)
	   (bdb-false)
	   (bdb-nil::obj)
	   (bdb-find-type::string ::obj)))

;*---------------------------------------------------------------------*/
;*    bdb-init! ...                                                    */
;*---------------------------------------------------------------------*/
(define (bdb-init!)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    bdb-initial-breakpoint ...                                       */
;*---------------------------------------------------------------------*/
(define (bdb-initial-breakpoint)
   #unspecified)
 
;*---------------------------------------------------------------------*/
;*    *client* ...                                                     */
;*---------------------------------------------------------------------*/
(define *client* #unspecified)

;*---------------------------------------------------------------------*/
;*    bdb:init-client! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function is called by the server each time the server       */
;*    wants to communicate. There is no way to improve that because    */
;*    there is no way for the server to know that the client is        */
;*    already initialized. In consequence, we have to take care        */
;*    here not to initalize several time the same client.              */
;*---------------------------------------------------------------------*/
(define (bdb:init-client! port-number)
   (let ()
      ($push-trace 'bdb:init-client! #f)
      ;; if we have already opened a client but from a different port-number
      ;; we start closing the previous client
      (if (and (socket? *client*)
	       (not (=fx (socket-port-number *client*) port-number)))
	  (bdb-close-client!))
      (if (not (socket? *client*))
	  (begin
	     ;; we setup the socket client
	     (set! *client* (make-client-socket "localhost" port-number))
	     (if (not (socket? *client*))
		 (error "bdb:init-client" "Can't setup client" *client*))))
      ($pop-trace)
      0))
       
;*---------------------------------------------------------------------*/
;*    bdb-close-client! ...                                            */
;*---------------------------------------------------------------------*/
(define (bdb-close-client!)
   (if (socket? *client*)
       (begin
	  (socket-shutdown *client* #f)
 	  (set! *client* #unspecified))))

;*---------------------------------------------------------------------*/
;*    bdb-invoke ...                                                   */
;*---------------------------------------------------------------------*/
(define (bdb-invoke port-number kind obj)
   (let ()
      ($push-trace 'bdb:invoke #f)
      (bdb:init-client! port-number)
      (let ((port (socket-output *client*)))
	 ;; the kind of connection
	 (write kind port)
	 (newline port)
	 ;; the value to be sent
	 (write obj port)
	 (newline port)
	 ;; we flush the all transfer
	 (flush-output-port port))
      ($pop-trace)
      0))

;* {*---------------------------------------------------------------------*} */
;* {*    *mangling-cache* ...                                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define *demangling-cache* #unspecified)                            */
;* (define *mangling-cache* #unspecified)                              */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    reset-demangling-cache! ...                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define (reset-demangling-cache!)                                   */
;*    (let ()                                                          */
;*       ($push-trace 'bdb:reset-demangling-cache!)                   */
;*       (set! *demangling-cache* (make-hashtable))                    */
;*       (set! *mangling-cache* (make-hashtable))                      */
;*       ($pop-trace)))                                               */

;*---------------------------------------------------------------------*/
;*    *bdb-module-info* ...                                            */
;*---------------------------------------------------------------------*/
(define *bdb-module-info* '())

;*---------------------------------------------------------------------*/
;*    mangle ...                                                       */
;*    -------------------------------------------------------------    */
;*    Mangle the identifier and returns to bdb.                        */
;*---------------------------------------------------------------------*/
(define (mangle port-number::int bgl::string)
   (let ((c (bgl->c bgl)))
      (bdb-invoke port-number #t c))
   0)

;*---------------------------------------------------------------------*/
;*    mangle2 ...                                                      */
;*    -------------------------------------------------------------    */
;*    Mangle the identifier and returns to bdb.                        */
;*---------------------------------------------------------------------*/
(define (mangle2 port-number::int bgl::string loc::string)
   (let ((c (c-bgl->c bgl loc)))
      (bdb-invoke port-number #t c))
   0)

;*---------------------------------------------------------------------*/
;*    mangle-for-funcall ...                                           */
;*    -------------------------------------------------------------    */
;*    Mangle the identifier and returns to bdb.                        */
;*    The returned identifier is the identifier of the closure         */
;*    associated to the Bigloo identifier.                             */
;*---------------------------------------------------------------------*/
(define (mangle-for-funcall port-number::int bgl::string)
   (let ((c (bgl->c-funcall bgl)))
      (bdb-invoke port-number #t c))
   0)

;*---------------------------------------------------------------------*/
;*    demangle ...                                                     */
;*    -------------------------------------------------------------    */
;*    Demangles a C identifier.                                        */
;*---------------------------------------------------------------------*/
(define (demangle port-number::int c::string)
   (let ((c (c->bgl c)))
      (bdb-invoke port-number #t c))
   0)

;*---------------------------------------------------------------------*/
;*    demangle2 ...                                                    */
;*    -------------------------------------------------------------    */
;*    Demangles a C identifier.                                        */
;*---------------------------------------------------------------------*/
(define (demangle2 port-number::int c::string loc::string)
   (let ((c (c-c->bgl c loc)))
      (bdb-invoke port-number #t c))
   0)
   
;*---------------------------------------------------------------------*/
;*    output-value ...                                                 */
;*---------------------------------------------------------------------*/
(define (output-value port-number::int value circle?::bool)
   (let ()
      ($push-trace 'bdb:output-value #f)
      (let ((port (open-output-string)))
	 (if circle?
	     (write-circle value port)
	     (write value port))
	 (let ((c (cons (find-runtime-type value)
			(close-output-port port))))
	    (bdb-invoke port-number #t c)
	    ($pop-trace)
	    0))))

;*---------------------------------------------------------------------*/
;*    output-classes ...                                               */
;*---------------------------------------------------------------------*/
(define (output-classes::int)
   (let ()
      ($push-trace 'bdb:output-value #f)
      (print (bgl-get-classes))
      ($pop-trace)
      0))

;*---------------------------------------------------------------------*/
;*    dump-classes ...                                                 */
;*---------------------------------------------------------------------*/
(define (dump-classes port-number::int)
   (bdb-invoke port-number #t (bgl-get-classes))
   0)

;*---------------------------------------------------------------------*/
;*    bdb-print ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function returns a string that is the result of the         */
;*    printing. This function does not perform any output. This        */
;*    function is called when gdb wants a value. For instance, it is   */
;*    used by the PRINT and the DISPLAY commands.                      */
;*---------------------------------------------------------------------*/
(define (bdb-print::string value::obj circle?::bool)
   (let ()
      ($push-trace 'bdb:print #f)
      (let ((port (open-output-string)))
	 (if circle?
	     (write-circle value port)
	     (write value port))
	 (let ((res (close-output-port port)))
	    ($pop-trace)
	    res))))

;*---------------------------------------------------------------------*/
;*    bdb-whatis ...                                                   */
;*    -------------------------------------------------------------    */
;*    Returns the dynamic type of a Scheme object.                     */
;*---------------------------------------------------------------------*/
(define (bdb-whatis::int port-number::int value::obj)
   (let ()
      ($push-trace 'bdb:whatis #f)
      (let ((port (open-output-string)))
	 (display "type = " port)
	 (write (find-runtime-type value) port)
	 (bdb-invoke port-number #t (close-output-port port)))
      ($pop-trace)
      0))
   
;*---------------------------------------------------------------------*/
;*    bdb-true ...                                                     */
;*---------------------------------------------------------------------*/
(define (bdb-true)
   #t)

;*---------------------------------------------------------------------*/
;*    bdb-false ...                                                    */
;*---------------------------------------------------------------------*/
(define (bdb-false)
   #f)

;*---------------------------------------------------------------------*/
;*    bdb-bchar ...                                                    */
;*---------------------------------------------------------------------*/
(define (bdb-bchar char)
   (pragma::obj "BCHAR( $1 )" (char->integer char)))
   
;*---------------------------------------------------------------------*/
;*    bdb-bint ...                                                     */
;*---------------------------------------------------------------------*/
(define (bdb-bint int)
   (pragma::obj "BINT( $1 )" int))
   
;*---------------------------------------------------------------------*/
;*    bdb-nil ...                                                      */
;*---------------------------------------------------------------------*/
(define (bdb-nil)
   '())

;*---------------------------------------------------------------------*/
;*    bdb-funcall ...                                                  */
;*---------------------------------------------------------------------*/
(define (bdb-funcall fun::procedure pair::pair-nil)
   (apply fun pair))

;*---------------------------------------------------------------------*/
;*    bdb-find-type ...                                                */
;*---------------------------------------------------------------------*/
(define (bdb-find-type obj)
   (find-runtime-type obj))
