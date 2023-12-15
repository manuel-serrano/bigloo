;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-cache.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 12:49:30 2017                          */
;*    Last change :  Fri Dec 15 10:44:14 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
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
	    *<fx* *<=fx* *>=fx* *>fx* *=fx* *-fx* *+fx* **fx*
            *subfx* *addfx*							
            *-fx-safe* *+fx-safe* **fx-safe*
            *$-fx/ov* *$+fx/ov* *$*fx/ov* 
            *$-fx/w-ov* *$+fx/w-ov* *$*fx/w-ov* 
	    *<fl* *<=fl* *>=fl* *>fl* *=fl* *-fl* *+fl*
	    *2-*
	    *2+*
	    *int->long*
	    *bint->long*
	    *$int->long*
	    *long->bint*
	    *vector-bound-check*
	    *string-bound-check*
	    *string-length*
	    *error*
	    *number?*))

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
(define **fx* #f)

(define *subfx* #f)
(define *addfx* #f)

(define *-fx-safe* #f)
(define *+fx-safe* #f)
(define **fx-safe* #f)

(define *$-fx/ov* #f)
(define *$+fx/ov* #f)
(define *$*fx/ov* #f)

(define *$-fx/w-ov* #f)
(define *$+fx/w-ov* #f)
(define *$*fx/w-ov* #f)

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
(define *$int->long* #f)
(define *long->bint* #f)
(define *vector-bound-check* #f)
(define *string-bound-check* #f)
(define *string-length* #f)
(define *error* #f)
(define *number?* #f)
(define *type-norms* #f)

;*---------------------------------------------------------------------*/
;*    start-bbv-cache! ...                                             */
;*---------------------------------------------------------------------*/
(define (start-bbv-cache!)
   (unless *cache-started?*
      (set! *cache-started?* #t)
      (set! *<fx* (get-global/module '$<fx 'foreign))
      (set! *<=fx* (get-global/module '$<=fx 'foreign))
      (set! *>fx* (get-global/module '$>fx 'foreign))
      (set! *>=fx* (get-global/module '$>=fx 'foreign))
      (set! *=fx* (get-global/module '$=fx 'foreign))
      (set! *-fx* (get-global/module '$-fx 'foreign))
      (set! *+fx* (get-global/module '$+fx 'foreign))
      (set! **fx* (get-global/module '$*fx 'foreign))
      (set! *subfx* (get-global/module '$subfx 'foreign))
      (set! *addfx* (get-global/module '$addfx 'foreign))
      (set! *-fx-safe* (get-global/module '-fx-safe 'foreign))
      (set! *+fx-safe* (get-global/module '+fx-safe 'foreign))
      (set! **fx-safe* (get-global/module '*fx-safe 'foreign))
      (set! *$-fx/ov* (get-global/module '$-fx/ov 'foreign))
      (set! *$+fx/ov* (get-global/module '$+fx/ov 'foreign))
      (set! *$*fx/ov* (get-global/module '$*fx/ov 'foreign))
      (set! *$-fx/w-ov* (get-global/module '$-fx/w-ov 'foreign))
      (set! *$+fx/w-ov* (get-global/module '$+fx/w-ov 'foreign))
      (set! *$*fx/w-ov* (get-global/module '$*fx/w-ov 'foreign))
      (set! *<fl* (get-global/module '$<fl 'foreign))
      (set! *<=fl* (get-global/module '$<=fl 'foreign))
      (set! *>fl* (get-global/module '$>fl 'foreign))
      (set! *>=fl* (get-global/module '$>=fl 'foreign))
      (set! *=fl* (get-global/module '$=fl 'foreign))
      (set! *-fl* (get-global/module '$-fl 'foreign))
      (set! *+fl* (get-global/module '$+fl 'foreign))
      (set! *2-* (get-global/module '2- '__r4_numbers_6_5))
      (set! *2+* (get-global/module '2+ '__r4_numbers_6_5))
      (set! *int->long* (get-global/module '$int->long 'foreign))
      (set! *bint->long* (get-global/module '$bint->long 'foreign))
      (set! *$int->long* (get-global/module '$int->long 'foreign))
      (set! *long->bint* (get-global/module '$long->bint 'foreign))
      (set! *vector-bound-check* (get-global/module '$vector-bound-check? 'foreign))
      (set! *string-bound-check* (get-global/module '$string-bound-check? 'foreign))
      (set! *string-length* (get-global/module '$string-length 'foreign))
      (set! *error* (get-global/module 'error '__error))
      (set! *number?* (get-global/module 'number? '__r4_numbers_6_5))))

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
   (set! **fx* #f)

   (set! *subfx* #f)
   (set! *addfx* #f)
   
   (set! *-fx-safe* #f)
   (set! *+fx-safe* #f)
   (set! **fx-safe* #f)
   
   (set! *$-fx/ov* #f)
   (set! *$+fx/ov* #f)
   (set! *$*fx/ov* #f)

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
   (set! *$int->long* #f)
   (set! *long->bint* #f)
   (set! *vector-bound-check* #f)
   (set! *string-bound-check* #f)
   (set! *string-length* #f)
   (set! *error* #f)
   (set! *number?* #f)
   (set! *type-norms* #f))
