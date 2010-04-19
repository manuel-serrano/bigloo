;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evenv.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 28 18:54:38 1994                          */
;*    Last change :  Mon Apr 19 14:59:02 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    La manipulation de l'environnement global de l'interprete        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __evenv
   
   (import  __r4_symbols_6_4)

   (use     __type
 	    __object
	    __error
	    __bigloo
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __param
	    __thread
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3)

   (extern  (macro __evmeaning_address::obj (::obj)
		   "__EVMEANING_ADDRESS")
	    (macro __evmeaning_address-ref::obj (::obj)
		   "__EVMEANING_ADDRESS_REF")
	    (macro __evmeaning_address-set!::obj (::obj ::obj)
		   "__EVMEANING_ADDRESS_SET"))
   
   (java    (class foreign
	       (method static __evmeaning_address::obj (::obj)
		       "__EVMEANING_ADDRESS")
	       (method static __evmeaning_address-ref::obj (::procedure)
		       "__EVMEANING_ADDRESS_REF")
	       (method static __evmeaning_address-set!::obj (::procedure ::obj)
		       "__EVMEANING_ADDRESS_SET")))
   
   (export  (init-the-global-environment!)
	    (inline eval-global?           exp)
	    (inline eval-global-value      exp)
	    (inline set-eval-global-value! exp exp)
	    (inline eval-global-tag        exp)
	    (inline eval-global-name       exp)
	    (bind-eval-global!             name var)
	    (eval-lookup                   exp)
	    (unbind-primop!                var)
	    (define-primop!                var val)
	    (define-primop-ref!            var addr))

   (pragma  (__evmeaning_address args-safe)))

;*---------------------------------------------------------------------*/
;*    init-the-global-environment! ...                                 */
;*    -------------------------------------------------------------    */
;*    Il faut que cette fonction utilise le symbol `0000'              */
;*    pour etre sur qu'il est definit au moment ou on fait les         */
;*    `define-primop'.                                                 */
;*---------------------------------------------------------------------*/
(define (init-the-global-environment!)
   'nothing)
   
;*---------------------------------------------------------------------*/
;*    eval-global? ...                                                 */
;*    -------------------------------------------------------------    */
;*    A global is a vector of 3 elements. The first one is the tag     */
;*    which discriminates compiled, read-only compiled, and evaluated  */
;*    global variable. The second element is the name. The third       */
;*    is the value.                                                    */
;*---------------------------------------------------------------------*/
(define-inline (eval-global? variable)
   (if (vector? variable)
       (=fx (vector-length variable) 3)
       #f))

;*---------------------------------------------------------------------*/
;*    bind-eval-global! ...                                            */
;*---------------------------------------------------------------------*/
(define (bind-eval-global! name var)
   (putprop! name '_0000 var))

;*---------------------------------------------------------------------*/
;*    bind-assert-eval-global! ...                                     */
;*---------------------------------------------------------------------*/
(define (bind-assert-eval-global! name var)
   (putprop! name '_0000_assert var))

;*---------------------------------------------------------------------*/
;*    unbind-primop! ...                                               */
;*---------------------------------------------------------------------*/
(define (unbind-primop! name)
   (remprop! name '_0000))

;*---------------------------------------------------------------------*/
;*    unbind-assert-primop! ...                                        */
;*---------------------------------------------------------------------*/
(define (unbind-assert-primop! name)
   (remprop! name '_0000_assert))

;*---------------------------------------------------------------------*/
;*    define-primop! ...                                               */
;*---------------------------------------------------------------------*/
(define (define-primop! var val)
   (let ((cell (eval-lookup var)))
      (if (not (eval-global? cell))
	  (bind-eval-global! var (vector 0 var val))
	  (set-eval-global-value! cell val))))

;*---------------------------------------------------------------------*/
;*    define-primop-ref! ...                                           */
;*---------------------------------------------------------------------*/
(define (define-primop-ref! var addr)
   (let ((cell (eval-lookup var)))
      (if (not (eval-global? cell))
	  (bind-eval-global! var (vector 1 var addr))
	  ;; MS 14 dec 2005
	  (begin
	     (set-eval-global-value! cell addr)
	     (warning-notify
	      (make-&eval-warning
	       #f #f #f
	       (list "Rebinding compiled constant" var)))))))

;*---------------------------------------------------------------------*/
;*    define-assert-primop-ref! ...                                    */
;*---------------------------------------------------------------------*/
(define (define-assert-primop-ref! var addr)
   (bind-assert-eval-global! var (vector 1 var addr)))

;*---------------------------------------------------------------------*/
;*    eval-lookup ...                                                  */
;*---------------------------------------------------------------------*/
(define (eval-lookup var)
   (let ((prop (getprop var '_0000_assert)))
      (if prop
	  prop
	  (let ((prop (getprop var '_0000)))
	     (if prop
		 prop
		 #f)))))

;*---------------------------------------------------------------------*/
;*    eval-global-tag ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (eval-global-tag eval-global)
   (vector-ref-ur eval-global 0))

;*---------------------------------------------------------------------*/
;*    eval-global-name ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (eval-global-name eval-global)
   (vector-ref-ur eval-global 1))

;*---------------------------------------------------------------------*/
;*    eval-global-value ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (eval-global-value eval-global)
   (vector-ref-ur eval-global 2))

;*---------------------------------------------------------------------*/
;*    set-eval-global-value! ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (set-eval-global-value! eval-global value)
   (vector-set-ur! eval-global 2 value))
