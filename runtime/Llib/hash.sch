;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/hash.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 15 07:24:23 2007                          */
;*    Last change :  Wed Nov 20 13:20:14 2013 (serrano)                */
;*    Copyright   :  2007-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hastable structure                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    %hashtable                                                       */
;*---------------------------------------------------------------------*/
(define-struct %hashtable size max-bucket-len buckets eqtest hashn weak
   max-length bucket-expansion)

;*---------------------------------------------------------------------*/
;*    table-get-hashnumber ...                                         */
;*---------------------------------------------------------------------*/
(define (table-get-hashnumber::long table key)
   (let ((hashn (%hashtable-hashn table)))
      (cond
	 ((procedure? hashn)
	  (absfx (hashn key)))
	 ((eq? hashn 'persistent)
	  (get-hashnumber-persistent key))
	 (else
	  (get-hashnumber key)))))

;*---------------------------------------------------------------------*/
;*    hashtable-equal? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (hashtable-equal? table obj1 obj2)
   (let ((eqt (%hashtable-eqtest table)))
      (cond
	 ((procedure? eqt)
	  (eqt obj1 obj2))
	 ((eq? obj1 obj2)
	  #t)
	 ((string? obj1)
	  (if (string? obj2)
	      (string=? obj1 obj2)
	      #f))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    Some constants                                                   */
;*---------------------------------------------------------------------*/
(define-macro (weak-none) 0)
(define-macro (weak-keys) 1)
(define-macro (weak-data) 2)
(define-macro (weak-both) 3)
(define-macro (weak-string) 4)
(define-macro (weak-open-string) 8)
