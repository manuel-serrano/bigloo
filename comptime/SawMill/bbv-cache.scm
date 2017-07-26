;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/bbv-cache.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 12:49:30 2017                          */
;*    Last change :  Wed Jul 26 10:12:18 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
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
	    *<fx*
	    *<=fx*
	    *>=fx*
	    *>fx*
	    *=fx*
	    *int->long*
	    *bint->long*
	    *vector-bound-check*
	    *type-norms*))

;*---------------------------------------------------------------------*/
;*    The cache registers definition                                   */
;*---------------------------------------------------------------------*/
(define *cache-started?* #f)

(define *<fx* #f)
(define *<=fx* #f)
(define *>=fx* #f)
(define *>fx* #f)
(define *=fx* #f)
(define *int->long* #f)
(define *bint->long* #f)
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
      (set! *int->long* (get-global/module '$int->long 'foreign))
      (set! *bint->long* (get-global/module '$bint->long 'foreign))
      (set! *vector-bound-check* (get-global/module '$vector-bound-check? 'foreign))
      (set! *type-norms*
	 (list (cons *int* *bint*)))))

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
   (set! *int->long* #f)
   (set! *bint->long* #f)
   (set! *vector-bound-check* #f)
   (set! *type-norms* #f))
