;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pkglib/src/Llib/interface.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 13 11:57:08 2006                          */
;*    Last change :  Sat Oct 13 07:57:36 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Tools for handling interfaces                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pkglib_interface

   (import __pkglib_misc)
   
   (export (interface-read-interface::obj ::input-port)
	   (interface-name-version::obj ::obj)
	   (interface-language::pair-nil ::obj)
	   (interface-export::pair-nil ::obj)
	   (interface-import::pair-nil ::obj)
	   (interface-from::pair-nil ::obj)
	   (interface-source::bstring ::obj)
	   (interface-meta::pair-nil ::obj)))

;*---------------------------------------------------------------------*/
;*    interface-read-interface ...                                     */
;*    -------------------------------------------------------------    */
;*    Read the interface declaration                                   */
;*---------------------------------------------------------------------*/
(define (interface-read-interface ip)
   (with-trace 4 'interface-read-interface
      (let ((e (read ip)))
	 (if (eof-object? e)
	     #f
	     (match-case e
		((interface (? symbol?) . ?rest)
		 (unless (and (list? rest)
			      (every list? rest))
		    (pkglib-error 'interface-read-interface
				  "Illegal interface declaration"
				  e))
		 e)
		(else
		 #f))))))
			   
;*---------------------------------------------------------------------*/
;*    interface-name-version ...                                       */
;*    -------------------------------------------------------------    */
;*    This function assumes a well formed interface.                   */
;*---------------------------------------------------------------------*/
(define (interface-name-version x)
   (match-case x
      ((?- ?name . ?rest)
       (values name (car (assq/default 'version rest '(#f)))))
      (else
       (pkglib-error 'interface-name-version
		     "Illegal interface declaration"
		     x))))

;*---------------------------------------------------------------------*/
;*    interface-language ...                                           */
;*    -------------------------------------------------------------    */
;*    This function assumes a well formed interface.                   */
;*---------------------------------------------------------------------*/
(define (interface-language x)
   (match-case x
      ((?- ?- . ?rest)
       (assq/default 'language rest '(r5rs)))
      (else
       '(r5rs))))

;*---------------------------------------------------------------------*/
;*    interface-export ...                                             */
;*    -------------------------------------------------------------    */
;*    This function assumes a well formed interface.                   */
;*---------------------------------------------------------------------*/
(define (interface-export x)
   (match-case x
      ((?- ?- . ?rest)
       (filter (lambda (x)
		  (match-case x
		     ((from . ?-)
		      #f)
		     (else
		      #t)))
	       (assq* 'export rest '())))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    interface-import ...                                             */
;*    -------------------------------------------------------------    */
;*    This function assumes a well formed interface.                   */
;*---------------------------------------------------------------------*/
(define (interface-import x)
   (match-case x
      ((?- ?- . ?rest)
       (assq* 'import rest '()))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    interface-from ...                                               */
;*    -------------------------------------------------------------    */
;*    This function assumes a well formed interface.                   */
;*---------------------------------------------------------------------*/
(define (interface-from x)
   (match-case x
      ((?- ?- . ?rest)
       (filter-map (lambda (x)
		      (match-case x
			 ((from ?module . ?-)
			  module)
			 (else
			  #f)))
		   (assq* 'export rest '())))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    interface-source ...                                             */
;*---------------------------------------------------------------------*/
(define (interface-source x)
   (match-case x
      ((?- ?name . ?rest)
       (or (assq* 'source rest #f)
	   (let ((suf (assq* 'suffix rest #f)))
	      (if suf
		  (string-append (symbol->string name) "." suf)
		  (string-append (symbol->string name) ".scm")))))
      (else
       "")))
   
;*---------------------------------------------------------------------*/
;*    interface-meta ...                                               */
;*    -------------------------------------------------------------    */
;*    This function assumes a well formed interface.                   */
;*---------------------------------------------------------------------*/
(define (interface-meta x)
   (match-case x
      ((?- ?- . ?meta)
       meta)
      (else
       '())))

