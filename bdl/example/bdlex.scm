;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdl/example/bdlex.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  8 10:42:26 2002                          */
;*    Last change :  Thu Aug  8 12:08:23 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    An example of BDL usage                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bdlex
   (library bdl)
   (main main))

;*---------------------------------------------------------------------*/
;*    Global variables                                                 */
;*---------------------------------------------------------------------*/
(define *dir* "../../comptime")
(define *idents* '())

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (args-parse (cdr argv)
      ((("-h" "--help") (help "This help message"))
       (args-parse-usage #f)
       (exit 0))
      ((("--sourcedir" "-d") ?dir (help "The source directory"))
       (set! *dir* dir))
      ((("--ident" "-i") ?ident (help "Search ident"))
       (set! *idents* (cons ident *idents*)))
      (else
       (print "Illegal argument `" else "'. Usage: ")
       (args-parse-usage #f)))
   (let ((prgm (read-program (make-file-name *dir* ".afile")
			     (make-file-name *dir* ".etags"))))
      (if (null? *idents*)
	  (dump-all prgm)
	  (dump-idents prgm *idents*))))

;*---------------------------------------------------------------------*/
;*    dump-all ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-all prgm)
   (print "files: ")
   (for-each (lambda (e)
		(print "   " e))
	     (get-bdl-files prgm))
   (newline)
   (print "modules: ")
   (for-each (lambda (e)
		(print "   " (bdl-entity-ident e)))
	     (get-bdl-modules prgm))
   (newline)
   (print "functions: ")
   (for-each (lambda (e)
		(print "   " (bdl-entity-ident e)))
	     (get-bdl-functions prgm))
   (print "variables: ")
   (for-each (lambda (e)
		(print "   " (bdl-entity-ident e)))
	     (get-bdl-variables prgm))
   (print "classes: ")
   (for-each (lambda (e)
		(print "   " (bdl-entity-ident e)))
	     (get-bdl-classes prgm)))

;*---------------------------------------------------------------------*/
;*    dump-idents ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-idents prgm idents)
   (define (display-ident id)
      (if (bdl-entity? id)
	  (with-access::bdl-entity id (loc ident)
	     (display* "    " ident " [" (find-runtime-type id) "] ")
	     (if (bdl-location? loc)
		 (with-access::bdl-location loc (file line)
		    (print "@ " file ":" line))
		 (newline)))
	  (print "     "  (find-runtime-type id))))
   (for-each (lambda (i)
		(let ((ident (find-bdl-regexp-ident prgm i)))
		   (print "*** " i)
		   (for-each display-ident ident)))
	     idents))
      
