;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/cache.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 11:28:43 1995                          */
;*    Last change :  Thu Dec  3 09:38:43 2009 (serrano)                */
;*    Copyright   :  1995-2009 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    A small type cache to avoid to many lookup in Tenv.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_cache
   (import type_type
	   type_env)
   (export (install-type-cache!)
           *obj*
           *cobj*
	   *cell*
	   *magic*
	   *void*
	   *bint*
	   *int*
	   *long*
	   *elong*
	   *belong*
	   *llong*
	   *bllong*
	   *bool*
	   *bbool*
	   *real*
	   *breal*
	   *bignum*
	   *char*
	   *bchar*
	   *bnil*
	   *pair*
	   *epair*
	   *pair-nil*
	   *string*
	   *bstring*
	   *ucs2string*
	   *symbol*
	   *keyword*
	   *vector*
	   *struct*
	   *unspec*
	   *procedure*
	   *procedure-el*
	   *procedure-el1*
	   *exit*
	   *_*
	   (get-default-type::type)
	   (set-default-type! ::type)
	   (get-object-type)
	   (get-default-c-type::type)))

;*---------------------------------------------------------------------*/
;*    install-type-cache! ...                                          */
;*---------------------------------------------------------------------*/
(define (install-type-cache!)
   (set! *obj*           (use-type! 'obj #f))
   (set! *cobj*          (use-type! 'cobj #f))
   (set! *cell*          (use-type! 'cell #f))
   (set! *magic*         (use-type! 'magic #f))
   (set! *void*          (use-type! 'void #f))
   (set! *bint*          (use-type! 'bint #f))
   (set! *int*           (use-type! 'int #f))
   (set! *long*          (use-type! 'long #f))
   (set! *elong*         (use-type! 'elong #f))
   (set! *belong*        (use-type! 'belong #f))
   (set! *llong*         (use-type! 'llong #f))
   (set! *bllong*        (use-type! 'bllong #f))
   (set! *bool*          (use-type! 'bool #f))
   (set! *bbool*         (use-type! 'bbool #f))
   (set! *real*          (use-type! 'double #f))
   (set! *breal*         (use-type! 'real #f))
   (set! *bignum*        (use-type! 'bignum #f))
   (set! *char*          (use-type! 'uchar #f))
   (set! *bchar*         (use-type! 'bchar #f))
   (set! *pair*          (use-type! 'pair #f))
   (set! *epair*         (use-type! 'epair #f))
   (set! *pair-nil*      (use-type! 'pair-nil #f))
   (set! *bnil*          (use-type! 'nil #f))
   (set! *string*        (use-type! 'string #f))
   (set! *bstring*       (use-type! 'bstring #f))
   (set! *ucs2string*    (use-type! 'ucs2string #f))
   (set! *symbol*        (use-type! 'symbol #f))
   (set! *keyword*       (use-type! 'keyword #f))
   (set! *vector*        (use-type! 'vector #f))
   (set! *struct*        (use-type! 'struct #f))
   (set! *procedure*     (use-type! 'procedure #f))
   (set! *procedure-el*  (use-type! 'procedure-el #f))
   (set! *procedure-el1* (use-type! 'procedure-el1 #f))
   (set! *unspec*        (use-type! 'unspecified #f))
   (set! *exit*          (use-type! 'exit #f))
   (set! *object*        (if (type-exists? 'object)
			     (find-type 'object)
			     #f))
   (set! *_*             (use-type! '_ #f))
   (set! *default-type* *_*))

;*---------------------------------------------------------------------*/
;*    The register cache                                               */
;*---------------------------------------------------------------------*/
(define *obj*           'no-type-yet)
(define *cobj*          'no-type-yet)
(define *cell*          'no-type-yet)
(define *magic*         'no-type-yet)
(define *void*          'no-type-yet)
(define *bint*          'no-type-yet)
(define *int*           'no-type-yet)
(define *long*          'no-type-yet)
(define *llong*         'no-type-yet)
(define *bllong*        'no-type-yet)
(define *elong*         'no-type-yet)
(define *belong*        'no-type-yet)
(define *bool*          'no-type-yet)
(define *bbool*         'no-type-yet)
(define *real*          'no-type-yet)
(define *breal*         'no-type-yet)
(define *bignum*        'no-type-yet)
(define *char*          'no-type-yet)
(define *bchar*         'no-type-yet)
(define *string*        'no-type-yet)
(define *bstring*       'no-type-yet)
(define *ucs2string*    'no-type-yet)
(define *symbol*        'no-type-yet)
(define *keyword*       'no-type-yet)
(define *pair*          'no-type-yet)
(define *epair*         'no-type-yet)
(define *pair-nil*      'no-type-yet)
(define *bnil*          'no-type-yet)
(define *vector*        'no-type-yet)
(define *struct*        'no-type-yet)
(define *procedure*     'no-type-yet)
(define *procedure-el*  'no-type-yet)
(define *procedure-el1* 'no-type-yet)
(define *unspec*        'no-type-yet)
(define *exit*          'no-type-yet)
(define *object*        'no-type-yet)
(define *_*             'no-type-yet)
(define *default-type*  'no-type-yet)

;*---------------------------------------------------------------------*/
;*    get-default-type ...                                             */
;*    -------------------------------------------------------------    */
;*    The default type is not a legal type that can be used in a       */
;*    foreign clause. Otherwise, the use-foreign-type! function        */
;*    implementation is incorrect.                                     */
;*---------------------------------------------------------------------*/
(define (get-default-type)
   *default-type*)

;*---------------------------------------------------------------------*/
;*    set-default-type! ...                                            */
;*---------------------------------------------------------------------*/
(define (set-default-type! type)
   (set! *default-type* type))

;*---------------------------------------------------------------------*/
;*    get-object-type ...                                              */
;*---------------------------------------------------------------------*/
(define (get-object-type)
   (cond
      ((type? *object*)
       *object*)
      (else
       (set! *object* (find-type 'object))
       *object*)))

;*---------------------------------------------------------------------*/
;*    get-default-c-type ...                                           */
;*    -------------------------------------------------------------    */
;*    The default type for extern function.                            */
;*---------------------------------------------------------------------*/
(define (get-default-c-type)
   *int*)
