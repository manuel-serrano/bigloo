;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cnst/cache.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 19 10:35:59 1995                          */
;*    Last change :  Mon Mar 10 15:20:13 2014 (serrano)                */
;*    Copyright   :  1995-2014 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    A cache to be able to recognize function call very fast.         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cnst_cache
   (import  type_type
	    ast_var
	    ast_env
	    engine_param)
   (export  (start-cnst-cache!)
	    (stop-cnst-cache!)
	    *cnst-table-ref*
	    *cnst-table-set!*
	    *cons*
	    *btrue*
	    *bfalse*
	    *string->bstring*
	    *string->ucs2string*
	    *bstring->symbol*
	    *bstring->keyword*
	    *bool->bbool*
	    *long->int*
	    *make-fx-procedure*
	    *make-va-procedure*
	    *make-l-procedure*
	    *double->real*
	    *elong->belong*
	    *llong->bllong*
	    *int32->bint32*
	    *uint32->buint32*
	    *int64->bint64*
	    *uint64->buint64*
	    *list->vector*
	    *vector-tag-set!*
	    *string->bignum*
	    *list->struct*))

;*---------------------------------------------------------------------*/
;*    The cache registers definition                                   */
;*---------------------------------------------------------------------*/
(define *cache-started?* #f)

(define *cnst-table-ref* #f)
(define *cnst-table-set!* #f)
(define *cons* #f)
(define *btrue* #f)
(define *bfalse* #f)
(define *string->bstring* #f)
(define *string->ucs2string* #f)
(define *bstring->symbol* #f)
(define *bstring->keyword* #f)
(define *bool->bbool* #f)
(define *long->int* #f)
(define *make-fx-procedure* #f)
(define *make-va-procedure* #f)
(define *make-l-procedure* #f)
(define *double->real* #f)
(define *elong->belong* #f)
(define *llong->bllong* #f)
(define *int32->bint32* #f)
(define *uint32->buint32* #f)
(define *int64->bint64* #f)
(define *uint64->buint64* #f)
(define *list->vector* #f)
(define *vector-tag-set!* #f)
(define *list->struct* #f)
(define *string->bignum* #f)

;*---------------------------------------------------------------------*/
;*    start-cnst-cache! ...                                            */
;*---------------------------------------------------------------------*/
(define (start-cnst-cache!)
   (if (not *cache-started?*)
       (begin
	  (set! *cache-started?* #t)
	  (set! *cnst-table-ref*
		(get-global/module 'cnst-table-ref 'foreign))
	  (set! *cnst-table-set!*
		(get-global/module 'cnst-table-set! 'foreign))
	  (set! *cons*
		(get-global/module '$cons 'foreign))
	  (set! *btrue*
		(get-global/module 'btrue 'foreign))
	  (set! *bfalse*
		(get-global/module 'bfalse 'foreign))
	  (set! *string->bstring*
		(get-global/module '$string->bstring 'foreign))
	  (set! *string->ucs2string*
		(get-global/module 'c-utf8-string->ucs2-string 'foreign))
	  (set! *bstring->symbol*
 		(get-global/module 'c-bstring->symbol 'foreign))
	  (set! *bstring->keyword*
		(get-global/module 'c-bstring->keyword 'foreign))
	  (set! *bool->bbool*
		(get-global/module '$bool->bbool 'foreign))
	  (set! *long->int*
		(get-global/module '$long->int 'foreign))
	  (set! *make-fx-procedure*
		(get-global/module 'make-fx-procedure 'foreign))
	  (set! *make-va-procedure*
		(get-global/module 'make-va-procedure 'foreign))
	  (set! *make-l-procedure*
		(get-global/module 'make-l-procedure 'foreign))
	  (set! *double->real*
		(get-global/module '$double->real 'foreign))
	  (set! *elong->belong*
		(get-global/module '$elong->belong 'foreign))
	  (set! *llong->bllong*
		(get-global/module '$llong->bllong 'foreign))
	  (set! *int32->bint32*
		(get-global/module '$int32->bint32 'foreign))
	  (set! *uint32->buint32*
		(get-global/module '$uint32->buint32 'foreign))
	  (set! *int64->bint64*
		(get-global/module '$int64->bint64 'foreign))
	  (set! *uint64->buint64*
		(get-global/module '$uint64->buint64 'foreign))
	  (set! *list->vector*
		(find-global 'list->vector))
	  (set! *vector-tag-set!*
		(find-global 'vector-tag-set!))
	  (set! *list->struct*
		(find-global 'list->struct))
	  (set! *string->bignum*
		(or (find-global '$string->bignum 'foreign)
		    (get-global/module '$string->bignum '__bignum)))
	  #t)
       #t))

;*---------------------------------------------------------------------*/
;*    stop-cnst-cache! ...                                             */
;*---------------------------------------------------------------------*/
(define (stop-cnst-cache!)
   (set! *string->bignum* #f)
   (set! *string->bstring* #f)
   (set! *string->ucs2string* #f)
   (set! *bstring->symbol* #f)
   (set! *bstring->keyword* #f)
   (set! *bool->bbool* #f)
   (set! *long->int* #f)
   (set! *make-fx-procedure* #f)
   (set! *make-va-procedure* #f)
   (set! *double->real* #f)
   (set! *cons* #f)
   (set! *btrue* #f)
   (set! *bfalse* #f)
   #t)






