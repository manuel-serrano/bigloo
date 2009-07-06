;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/local.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 17:39:21 1994                          */
;*    Last change :  Wed Apr 23 18:21:48 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The declaration of local variables                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_local
   (import  type_type
	    type_cache
	    engine_param
	    ast_var
	    ast_ident)
   (export  (clone-local::local          ::local  ::value)
	    (make-local-svar::local      ::symbol ::type)
	    (make-user-local-svar::local ::symbol ::type)
	    (make-local-sexit::local     ::symbol ::type ::sexit)
	    (make-local-sfun::local      ::symbol ::type ::sfun)
	    (make-user-local-sfun::local ::symbol ::type ::sfun)))

;*---------------------------------------------------------------------*/
;*    *local-key* ...                                                  */
;*---------------------------------------------------------------------*/
(define *local-key* 0)

;*---------------------------------------------------------------------*/
;*    get-new-key ...                                                  */
;*---------------------------------------------------------------------*/
(define (get-new-key)
   (set! *local-key* (+fx *local-key* 1))
   *local-key*)

;*---------------------------------------------------------------------*/
;*    make-new-local ...                                               */
;*    -------------------------------------------------------------    */
;*    All local variables are allocated using this function.           */
;*---------------------------------------------------------------------*/
(define (make-new-local id type value user?)
   (let ((key (get-new-key)))
      (instantiate::local
	 (key key)
	 (id id)
	 (name (string-append (local-id->name id) "_" (integer->string key)))
	 (type type)
	 (user? user?)
	 (value value))))

;*---------------------------------------------------------------------*/
;*    clone-local ...                                                  */
;*---------------------------------------------------------------------*/
(define (clone-local local value)
   (let ((key (get-new-key)))
      (with-access::local local (id)
	 (duplicate::local local
	    (key key)
	    (name (string-append (id->name id) "_" (integer->string key)))
	    (value value)))))

;*---------------------------------------------------------------------*/
;*    make-local-svar ...                                              */
;*---------------------------------------------------------------------*/
(define (make-local-svar id type)
   (make-new-local id type (instantiate::svar) #f))
  
;*---------------------------------------------------------------------*/
;*    make-user-local-svar ...                                         */
;*---------------------------------------------------------------------*/
(define (make-user-local-svar id type)
   (make-new-local id type (instantiate::svar) #t))
  
;*---------------------------------------------------------------------*/
;*    make-local-sexit ...                                             */
;*---------------------------------------------------------------------*/
(define (make-local-sexit id type sexit)
   (make-new-local id type sexit #f))
  
;*---------------------------------------------------------------------*/
;*    make-local-sfun ...                                              */
;*    -------------------------------------------------------------    */
;*    All local variables are allocated using this function.           */
;*---------------------------------------------------------------------*/
(define (make-local-sfun id type sfun)
   (make-new-local id type sfun #f))
  
;*---------------------------------------------------------------------*/
;*    make-user-local-sfun ...                                         */
;*    -------------------------------------------------------------    */
;*    All local variables are allocated using this function.           */
;*---------------------------------------------------------------------*/
(define (make-user-local-sfun id type sfun)
   (make-new-local id type sfun #t))
  
