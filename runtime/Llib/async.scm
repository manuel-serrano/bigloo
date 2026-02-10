;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/runtime/Llib/async.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 10 08:40:18 2026                          */
;*    Last change :  Tue Feb 10 09:38:29 2026 (serrano)                */
;*    Copyright   :  2026 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Asynchronous functions                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __async
   
   (import  __error
	    __object)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    __structure
	    __date
	    __os
	    __bit
	    __thread
	    __promise
	    __generator

	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
	    __r4_characters_6_6
	    __r4_equivalence_6_2 
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_output_6_10_3

	    __r5_control_features_6_4
	    
	    __foreign
	    __evenv)

   (export  (expand-async x::pair e::procedure)
	    (expand-define-async x::pair e::procedure)
	    (spawn-async::promise ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-async ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-async x e)
   (match-case x
      ((async ?args . ?body)
       (let ((ae (lambda (x ne)
		    (match-case x
		       ((await ?val)
			`(yield ,(e val e)))
		       (else
			(e x ne))))))
	  (ae `(lambda ,args (spawn-async (lambda* () ,@body))) ae)))
      (else
       (error "async" "wrong syntax" x))))

;*---------------------------------------------------------------------*/
;*    expand-define-async ...                                          */
;*---------------------------------------------------------------------*/
(define (expand-define-async x e)
   (match-case x
      ((define-async (?fun . ?args) . ?body)
       (e `(define ,fun (async ,args ,@body)) e))
      (else
       (error "define-async" "wrong syntax" x))))

;*---------------------------------------------------------------------*/
;*    spawn-async ...                                                  */
;*---------------------------------------------------------------------*/
(define (spawn-async gen::procedure)

   (make-promise
      (lambda (resolve reject)

	 (define (step g::generator action::procedure arg::obj)
	    (let ((p (with-handler
			(lambda (e)
			   (reject e))
			(action g arg))))
	       (cond
		  ((not p)
		   #unspecified)
		  ((-> g done)
		   (resolve p)
		   #unspecified)
		  (else
		   (let ((prom (resolved p)))
		      (then prom (lambda (v) (step g next v)))
		      (catch prom (lambda (e) (step g throw e))))))))

	 (step (gen) next #unspecified))))

		   
	    

   
   

