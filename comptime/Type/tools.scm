;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/tools.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 30 10:13:43 1995                          */
;*    Last change :  Sat Dec 23 09:07:00 2000 (serrano)                */
;*    Copyright   :  1995-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Some tools for type handling                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_tools
   
   (import  type_type)
   
   (export  (string-sans-$                 ::bstring)
	    (type-name-sans-$              ::type)
	    ($-in-name?                    ::bstring)
	    (*-name?                       ::bstring)
	    (replace-$                     ::bstring ::bstring)
	    (make-typed-declaration        ::type ::bstring)
	    (make-pointer-to-name::bstring ::type)))

;*---------------------------------------------------------------------*/
;*    string-sans-$ ...                                                */
;*    -------------------------------------------------------------    */
;*    This function allocates a new string where `$' are replaced      */
;*    by ` '.                                                          */
;*---------------------------------------------------------------------*/
(define (string-sans-$ string)
   (let ((new (string-copy string)))
      (let loop ((i (-fx (string-length new) 1)))
	 (cond
	    ((=fx i -1)
	     new)
	    ((char=? (string-ref new i) #\$)
	     (string-set! new i #\space)
	     (loop (-fx i 1)))
	    (else
	     (loop (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    type-name-sans-$ ...                                             */
;*---------------------------------------------------------------------*/
(define (type-name-sans-$ type)
   (let ((tname (type-name type)))
      (if (type-$ type)
	  (string-sans-$ tname)
	  tname)))

;*---------------------------------------------------------------------*/
;*    $-in-name? ...                                                   */
;*    -------------------------------------------------------------    */
;*    Is a name contains a `$' ?                                       */
;*---------------------------------------------------------------------*/
(define ($-in-name? name)
   (let loop ((i (-fx (string-length name) 1)))
      (cond
	 ((=fx i -1)
	  #f)
	 ((char=? (string-ref name i) #\$)
	  #t)
	 (else
	  (loop (-fx i 1))))))

;*---------------------------------------------------------------------*/
;*    *-name? ...                                                      */
;*    -------------------------------------------------------------    */
;*    Is a name stopped on a `*' ?                                     */
;*---------------------------------------------------------------------*/
(define (*-name? name)
   (char=? (string-ref name (-fx (string-length name) 1)) #\*))

;*---------------------------------------------------------------------*/
;*    replace-$ ...                                                    */
;*---------------------------------------------------------------------*/
(define (replace-$ string rplac)
   (let* ((len-string (string-length string))
	  (len-rplac  (string-length rplac))
	  (len        (-fx (+fx len-string len-rplac) 1))
	  (new        (make-string len)))
      (let loop ((r 0)
		 (w 0))
	 (cond
	    ((=fx r len-string)
	     new)
	    ((char=? (string-ref string r) #\$)
	     ;; we insert rplac
	     (let liip ((w  w)
			(rr 0))
		(if (=fx rr len-rplac)
		    (loop (+fx r 1) w)
		    (begin
		       (string-set! new w (string-ref rplac rr))
		       (liip (+fx w 1) (+fx rr 1))))))
	    (else
	     (string-set! new w (string-ref string r))
	     (loop (+fx r 1) (+fx w 1)))))))

;*---------------------------------------------------------------------*/
;*    make-typed-declaration ...                                       */
;*---------------------------------------------------------------------*/
(define (make-typed-declaration type id)
   (let ((tname (type-name type)))
      (cond
	 ((not (type-$ type))
	  (string-append tname " " id))
	 (else
	  (replace-$ tname id)))))
      
;*---------------------------------------------------------------------*/
;*    make-pointer-to-name ...                                         */
;*---------------------------------------------------------------------*/
(define (make-pointer-to-name type)
   (let ((tname (type-name type)))
      (if (not (string? tname))
	  (error "make-pointer-to-name" "Unbound foreign type" (type-id type))
	  (cond
	     ((not (type-$ type))
	      (string-append tname " *"))
	     (else
	      (replace-$ tname "(*)"))))))
      
