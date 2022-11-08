;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawMill/bbset.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 18 08:02:07 2022                          */
;*    Last change :  Mon Jul 18 08:02:31 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Basic block sets                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    bbset ...                                                        */
;*---------------------------------------------------------------------*/
(define-struct bbset mark list)

;*---------------------------------------------------------------------*/
;*    make-empty-bbset ...                                             */
;*---------------------------------------------------------------------*/
(define (make-empty-bbset::struct)
   (bbset (get-bb-mark) '()))

;*---------------------------------------------------------------------*/
;*    bbset-in? ...                                                    */
;*---------------------------------------------------------------------*/
(define (bbset-in?::bool block::blockS set::struct)
   (with-access::blockS block (%mark)
      (eq? %mark (bbset-mark set))))

;*---------------------------------------------------------------------*/
;*    bbset-cons ...                                                   */
;*---------------------------------------------------------------------*/
(define (bbset-cons::struct b::blockS set::struct)
   (let ((m (bbset-mark set)))
      (with-access::blockS b (%mark)
	 (set! %mark m))
      (bbset-list-set! set (cons b (bbset-list set)))
      set))

;*---------------------------------------------------------------------*/
;*    bbset-cons* ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbset-cons*::struct b::blockS . rest)
   (cond
      ((null? (cdr rest))
       (bbset-cons b (car rest)))
      ((null? (cddr rest))
       (bbset-cons b (bbset-cons (car rest) (cadr rest))))
      (else
       (bbset-cons b (apply bbset-cons* rest)))))

;*---------------------------------------------------------------------*/
;*    bbset-append ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbset-append::struct blocks::pair-nil set::struct)
   (let ((m (bbset-mark set)))
      (for-each (lambda (b)
		   (with-access::blockS b (%mark)
		      (set! %mark m)))
	 blocks)
      (bbset-list-set! set (append blocks (bbset-list set)))
      set))

