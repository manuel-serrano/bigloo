;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/runtime/Ieee/symbol-generic.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 13:46:43 2024                          */
;*    Last change :  Sun Sep  8 19:58:31 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Symbol generic implementation                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (use __bexit)
   (import __hash)
   (export ($$bstring->symbol::symbol ::bstring)
	   ($$bstring->keyword::keyword ::bstring)))

;*---------------------------------------------------------------------*/
;*    symbol table                                                     */
;*---------------------------------------------------------------------*/
(define *symbol-mutex* (make-mutex 'symbol-mutex))
(define *symbol-table* *symbol-table*)

;*---------------------------------------------------------------------*/
;*    $$bstring->symbol ...                                            */
;*---------------------------------------------------------------------*/
(define ($$bstring->symbol string)
   (synchronize *symbol-mutex*
      (unless (hashtable? *symbol-table*)
	 (set! *symbol-table* (create-hashtable :weak #f)))
      (let ((old (hashtable-get *symbol-table* string)))
	 (if (symbol? old)
	     old
	     (let ((sym ($make-symbol string)))
		(hashtable-put! *symbol-table* string sym)
		sym)))))

;*---------------------------------------------------------------------*/
;*    keyword table                                                    */
;*---------------------------------------------------------------------*/
(define *keyword-mutex* (make-mutex 'keyword-mutex))
(define *keyword-table* *keyword-table*)

;*---------------------------------------------------------------------*/
;*    $$bstring->keyword ...                                           */
;*---------------------------------------------------------------------*/
(define ($$bstring->keyword string)
   (synchronize *keyword-mutex*
      (unless (hashtable? *keyword-table*)
	 (set! *keyword-table* (create-hashtable :weak #f)))
      (let ((old (hashtable-get *keyword-table* string)))
	 (if (keyword? old)
	     old
	     (let ((key ($make-keyword string)))
		(hashtable-put! *keyword-table* string key)
		key)))))
