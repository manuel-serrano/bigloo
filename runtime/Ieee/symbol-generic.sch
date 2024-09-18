;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/runtime/Ieee/symbol-generic.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 13:46:43 2024                          */
;*    Last change :  Wed Sep 18 16:37:36 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Symbol generic implementation                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (use __bexit
	__structure)
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
      ;; don't use hashtable? predicate because of symbols bootstrap
      (unless (struct? *symbol-table*)
	 (set! *symbol-table* (create-hashtable-open-string)))
      (let ((old (open-string-hashtable-get *symbol-table* string)))
	 (if (symbol? old)
	     old
	     (let ((sym ($make-symbol string)))
		(open-string-hashtable-put! *symbol-table* string sym)
		sym)))))

;*---------------------------------------------------------------------*/
;*    $$symbol-exists? ...                                             */
;*---------------------------------------------------------------------*/
(define ($$symbol-exists? sym)
   (synchronize *symbol-mutex*
      (open-string-hashtable-get *symbol-table* sym)))
   
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
      ;; don't use hashtable? predicate because of symbols bootstrap
      (unless (struct? *keyword-table*)
	 (set! *keyword-table* (create-hashtable-open-string)))
      (let ((old (open-string-hashtable-get *keyword-table* string)))
	 (if (keyword? old)
	     old
	     (let ((key ($make-keyword string)))
		(open-string-hashtable-put! *keyword-table* string key)
		key)))))
