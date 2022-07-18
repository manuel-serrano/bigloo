;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-cache.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 12:49:30 2017                          */
;*    Last change :  Mon Jul 18 11:44:17 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    bbv-cache                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-cache
   
   (import  type_type
	    type_cache
	    ast_var
	    ast_env)
   
   (export  (start-bbv-cache!)
	    (stop-bbv-cache!)
	    *<fx* *<=fx* *>=fx* *>fx* *=fx* *-fx* *+fx*
	    *-fx-safe* *+fx-safe*
	    *<fl* *<=fl* *>=fl* *>fl* *=fl* *-fl* *+fl*
	    *2-*
	    *2+*
	    *int->long*
	    *bint->long*
	    *long->bint*
	    *vector-bound-check*))

;*---------------------------------------------------------------------*/
;*    The cache registers definition                                   */
;*---------------------------------------------------------------------*/
(define *cache-started?* #f)

(define *<fx* #f)
(define *<=fx* #f)
(define *>=fx* #f)
(define *>fx* #f)
(define *=fx* #f)
(define *-fx* #f)
(define *+fx* #f)
(define *-fx-safe* #f)
(define *+fx-safe* #f)
(define *<fl* #f)
(define *<=fl* #f)
(define *>=fl* #f)
(define *>fl* #f)
(define *=fl* #f)
(define *-fl* #f)
(define *+fl* #f)
(define *2-* #f)
(define *2+* #f)
(define *int->long* #f)
(define *bint->long* #f)
(define *long->bint* #f)
(define *vector-bound-check* #f)
(define *type-norms* #f)

;*---------------------------------------------------------------------*/
;*    start-bbv-cache! ...                                             */
;*---------------------------------------------------------------------*/
(define (start-bbv-cache!)
   (unless *cache-started?*
      (set! *cache-started?* #t)
      (set! *<fx* (get-global/module 'c-<fx 'foreign))
      (set! *<=fx* (get-global/module 'c-<=fx 'foreign))
      (set! *>fx* (get-global/module 'c->fx 'foreign))
      (set! *>=fx* (get-global/module 'c->=fx 'foreign))
      (set! *=fx* (get-global/module 'c-=fx 'foreign))
      (set! *-fx* (get-global/module 'c--fx 'foreign))
      (set! *+fx* (get-global/module 'c-+fx 'foreign))
      (set! *-fx-safe* (get-global/module '-fx-safe 'foreign))
      (set! *+fx-safe* (get-global/module '+fx-safe 'foreign))
      (set! *<fl* (get-global/module 'c-<fl 'foreign))
      (set! *<=fl* (get-global/module 'c-<=fl 'foreign))
      (set! *>fl* (get-global/module 'c->fl 'foreign))
      (set! *>=fl* (get-global/module 'c->=fl 'foreign))
      (set! *=fl* (get-global/module 'c-=fl 'foreign))
      (set! *-fl* (get-global/module 'c--fl 'foreign))
      (set! *+fl* (get-global/module 'c-+fl 'foreign))
      (set! *2-* (get-global/module '2- '__r4_numbers_6_5))
      (set! *2+* (get-global/module '2+ '__r4_numbers_6_5))
      (set! *int->long* (get-global/module '$int->long 'foreign))
      (set! *bint->long* (get-global/module '$bint->long 'foreign))
      (set! *long->bint* (get-global/module '$long->bint 'foreign))
      (set! *vector-bound-check* (get-global/module '$vector-bound-check? 'foreign))))

;*---------------------------------------------------------------------*/
;*    stop-bbv-cache! ...                                              */
;*---------------------------------------------------------------------*/
(define (stop-bbv-cache!)
   (set! *cache-started?* #f)
   (set! *<fx* #f)
   (set! *<=fx* #f)
   (set! *>fx* #f)
   (set! *>=fx* #f)
   (set! *=fx* #f)
   (set! *-fx* #f)
   (set! *+fx* #f)
   (set! *-fx-safe* #f)
   (set! *+fx-safe* #f)
   (set! *<fl* #f)
   (set! *<=fl* #f)
   (set! *>fl* #f)
   (set! *>=fl* #f)
   (set! *=fl* #f)
   (set! *-fl* #f)
   (set! *+fl* #f)
   (set! *2-* #f)
   (set! *2+* #f)
   (set! *int->long* #f)
   (set! *bint->long* #f)
   (set! *long->bint* #f)
   (set! *vector-bound-check* #f)
   (set! *type-norms* #f))
