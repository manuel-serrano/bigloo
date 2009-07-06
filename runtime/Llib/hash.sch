;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/hash.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 15 07:24:23 2007                          */
;*    Last change :  Thu Feb 15 07:42:33 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Hastable structure                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    %hashtable                                                       */
;*---------------------------------------------------------------------*/
(define-struct %hashtable size max-bucket-len buckets eqtest hashn weak)

;*---------------------------------------------------------------------*/
;*    table-get-hashnumber ...                                         */
;*---------------------------------------------------------------------*/
(define (table-get-hashnumber::long table key)
   (let ((hashn (%hashtable-hashn table)))
      (if (procedure? hashn)
	  (absfx (hashn key))
	  (get-hashnumber key))))

;*---------------------------------------------------------------------*/
;*    hashtable-equal? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (hashtable-equal? table obj1 obj2)
   (let ((eqt (%hashtable-eqtest table)))
      (cond
	 ((procedure? eqt)
	  (eqt obj1 obj2))
	 ((string? obj1)
	  (if (string? obj2)
	      (string=? obj1 obj2)
	      #f))
	 (else
	  (equal? obj1 obj2)))))

;*---------------------------------------------------------------------*/
;*    Some constants                                                   */
;*---------------------------------------------------------------------*/
(define-macro (weak-none) 0)
(define-macro (weak-keys) 1)
(define-macro (weak-data) 2)
(define-macro (weak-both) 3)
