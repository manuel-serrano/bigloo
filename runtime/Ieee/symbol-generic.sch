;*=====================================================================*/
;*    .../project/bigloo/bigloo/runtime/Ieee/symbol-generic.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 13:46:43 2024                          */
;*    Last change :  Tue Oct  1 10:10:41 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Symbol generic implementation                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export ($$bstring->symbol::symbol ::bstring)
	   ($$bkeyword->symbol::keyword ::bstring)))

;*---------------------------------------------------------------------*/
;*    symbol table                                                     */
;*---------------------------------------------------------------------*/
(define *symbol-mutex* (make-mutex 'symbol-mutex))
(define *symbol-table* '())

;*---------------------------------------------------------------------*/
;*    $$bstring->symbol ...                                            */
;*---------------------------------------------------------------------*/
(define ($$bstring->symbol string)
   (synchronize *symbol-mutex*
      (let ((old (assoc string *symbol-table*)))
	 (if (pair? old)
	     (cdr old)
	     (let ((sym ($make-symbol (string-copy string))))
		(set! *symbol-table* (cons (cons string sym) *symbol-table*))
		sym)))))

;*---------------------------------------------------------------------*/
;*    keyword table                                                    */
;*---------------------------------------------------------------------*/
(define *keyword-mutex* (make-mutex 'keyword-mutex))
(define *keyword-table* '())

;*---------------------------------------------------------------------*/
;*    $$bstring->keyword ...                                           */
;*---------------------------------------------------------------------*/
(define ($$bstring->keyword string)
   (synchronize *keyword-mutex*
      (let ((old (assoc string *keyword-table*)))
	 (if (pair? old)
	     (cdr old)
	     (let ((sym ($make-keyword string)))
		(set! *keyword-table* (cons (cons string sym) *keyword-table*))
		sym)))))



