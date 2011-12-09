;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Bee/etags.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 29 09:18:33 1999                          */
;*    Last change :  Fri Dec  9 11:11:28 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The debugger tries to load the etags file for the project in     */
;*    order to be able to do some eager demangling. This is important  */
;*    in order to be able to set breakpoints using Bigloo symbolic     */
;*    names before the execution starts.                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bee_etags
   (library bdl)
   (import  engine_param
	    tools_speek
	    tools_error
	    (console-prompt tools_io)
	    (bdb-repl-prompt engine_repl))
   (export  (read-etags-file)
	    (bigloo-symbol-etags-source-location ::bstring)))

;*---------------------------------------------------------------------*/
;*    *prgm* ...                                                       */
;*---------------------------------------------------------------------*/
(define *prgm* #unspecified)

;*---------------------------------------------------------------------*/
;*    read-etags-file ...                                              */
;*---------------------------------------------------------------------*/
(define (read-etags-file)
   (let ((file (find-file/path *etags* (list "." *root-directory*))))
      (if (and (string? file) (file-exists? file))
	  ;; we really have to read the etags file
	  (begin
	     (verbose 4 "reading etags file \"" file "\"...")
	     (let ((prgm (new-program #f file '())))
		(set! *prgm* prgm)
		(read-etags! prgm '()))
	     (verbose 4 #"done.\n"))
	  (error 'read-etags-file
	     (format "Can't find .etags file in path ~a, please generate one with bgltags"
		*etags* (list "." *root-directory*))
	     *etags*))))

;*---------------------------------------------------------------------*/
;*    bigloo-symbol-etags-source-location ...                          */
;*---------------------------------------------------------------------*/
(define (bigloo-symbol-etags-source-location id)
   (let ((e (find-bdl-ident *prgm* id)))
      (if (isa? e bdl-entity)
          (let ((e::bdl-entity e))
	     (with-access::bdl-location (-> e loc) (file line)
		(string-append file ":" 
		   (integer->string line))))
	  #f)))
