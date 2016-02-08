;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/cache.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 18 11:28:43 1995                          */
;*    Last change :  Sat Feb  6 09:44:17 2016 (serrano)                */
;*    Copyright   :  1995-2016 Manuel Serrano, see LICENSE file        */
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
	   *int8*
	   *bint8*
	   *uint8*
	   *buint8*
	   *int16*
	   *bint16*
	   *uint16*
	   *buint16*
	   *int32*
	   *bint32*
	   *uint32*
	   *buint32*
	   *int64*
	   *bint64*
	   *uint64*
	   *buint64*
	   *bool*
	   *bbool*
	   *real*
	   *breal*
	   *bignum*
	   *char*
	   *schar*
	   *bchar*
	   *bnil*
	   *pair*
	   *epair*
	   *pair-nil*
	   *list*
	   *string*
	   *bstring*
	   *ucs2string*
	   *symbol*
	   *keyword*
	   *vector*
	   *hvectors*
	   *struct*
	   *unspec*
	   *procedure*
	   *procedure-el*
	   *exit*
	   *foreign*
	   *mutex*
	   *_*
	   (get-default-type::type)
	   (set-default-type! ::type)
	   (get-object-type)
	   (get-class-type)
	   (get-default-c-type::type)
	   (get-bigloo-type::type ::type)
	   (get-bigloo-defined-type::type ::type)))

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
   (set! *int8*          (use-type! 'int8 #f))
   (set! *bint8*         (use-type! 'bint8 #f))
   (set! *uint8*         (use-type! 'uint8 #f))
   (set! *buint8*        (use-type! 'buint8 #f))
   (set! *int16*         (use-type! 'int16 #f))
   (set! *bint16*        (use-type! 'bint16 #f))
   (set! *uint16*        (use-type! 'uint16 #f))
   (set! *buint16*       (use-type! 'buint16 #f))
   (set! *int32*         (use-type! 'int32 #f))
   (set! *bint32*        (use-type! 'bint32 #f))
   (set! *uint32*        (use-type! 'uint32 #f))
   (set! *buint32*       (use-type! 'buint32 #f))
   (set! *int64*         (use-type! 'int64 #f))
   (set! *bint64*        (use-type! 'bint64 #f))
   (set! *uint64*        (use-type! 'uint64 #f))
   (set! *buint64*       (use-type! 'buint64 #f))
   (set! *bool*          (use-type! 'bool #f))
   (set! *bbool*         (use-type! 'bbool #f))
   (set! *real*          (use-type! 'double #f))
   (set! *breal*         (use-type! 'real #f))
   (set! *bignum*        (use-type! 'bignum #f))
   (set! *char*          (use-type! 'uchar #f))
   (set! *schar*         (use-type! 'char #f))
   (set! *bchar*         (use-type! 'bchar #f))
   (set! *pair*          (use-type! 'pair #f))
   (set! *epair*         (use-type! 'epair #f))
   (set! *pair-nil*      (use-type! 'pair-nil #f))
   (set! *list*          (use-type! 'list #f))
   (set! *bnil*          (use-type! 'nil #f))
   (set! *string*        (use-type! 'string #f))
   (set! *bstring*       (use-type! 'bstring #f))
   (set! *ucs2string*    (use-type! 'ucs2string #f))
   (set! *symbol*        (use-type! 'symbol #f))
   (set! *keyword*       (use-type! 'keyword #f))
   (set! *vector*        (use-type! 'vector #f))
   (set! *hvectors*       (list
			     (use-type! 's8vector #f)
			     (use-type! 'u8vector #f)
			     (use-type! 's16vector #f)
			     (use-type! 'u16vector #f)
			     (use-type! 's32vector #f)
			     (use-type! 'u32vector #f)
			     (use-type! 's64vector #f)
			     (use-type! 'u64vector #f)
			     (use-type! 'f32vector #f)
			     (use-type! 'f64vector #f)))
   (set! *struct*        (use-type! 'struct #f))
   (set! *procedure*     (use-type! 'procedure #f))
   (set! *procedure-el*  (use-type! 'procedure-el #f))
   (set! *unspec*        (use-type! 'unspecified #f))
   (set! *exit*          (use-type! 'exit #f))
   (set! *object*        (if (type-exists? 'object)
			     (find-type 'object)
			     #f))
   (set! *class*         (if (type-exists? 'class)
			     (find-type 'class)
			     #f))
   (set! *foreign*       (use-type! 'foreign #f))
   (set! *mutex*         (use-type! 'mutex #f))
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
(define *int8*          'no-type-yet)
(define *bint8*         'no-type-yet)
(define *uint8*         'no-type-yet)
(define *buint8*        'no-type-yet)
(define *int16*         'no-type-yet)
(define *bint16*        'no-type-yet)
(define *uint16*        'no-type-yet)
(define *buint16*       'no-type-yet)
(define *int32*         'no-type-yet)
(define *bint32*        'no-type-yet)
(define *uint32*        'no-type-yet)
(define *buint32*       'no-type-yet)
(define *int64*         'no-type-yet)
(define *bint64*        'no-type-yet)
(define *uint64*        'no-type-yet)
(define *buint64*       'no-type-yet)
(define *elong*         'no-type-yet)
(define *belong*        'no-type-yet)
(define *bool*          'no-type-yet)
(define *bbool*         'no-type-yet)
(define *real*          'no-type-yet)
(define *breal*         'no-type-yet)
(define *bignum*        'no-type-yet)
(define *char*          'no-type-yet)
(define *schar*         'no-type-yet)
(define *bchar*         'no-type-yet)
(define *string*        'no-type-yet)
(define *bstring*       'no-type-yet)
(define *ucs2string*    'no-type-yet)
(define *symbol*        'no-type-yet)
(define *keyword*       'no-type-yet)
(define *pair*          'no-type-yet)
(define *epair*         'no-type-yet)
(define *pair-nil*      'no-type-yet)
(define *list*          'no-type-yet)
(define *bnil*          'no-type-yet)
(define *vector*        'no-type-yet)
(define *hvectors*      'no-type-yet)
(define *struct*        'no-type-yet)
(define *procedure*     'no-type-yet)
(define *procedure-el*  'no-type-yet)
(define *unspec*        'no-type-yet)
(define *exit*          'no-type-yet)
(define *object*        'no-type-yet)
(define *class*         'no-type-yet)
(define *foreign*       'no-type-yet)
(define *mutex*         'no-type-yet)
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
;*    get-class-type ...                                               */
;*---------------------------------------------------------------------*/
(define (get-class-type)
   (cond
      ((type? *class*)
       *class*)
      (else
       (set! *class* (find-type 'class))
       *class*)))

;*---------------------------------------------------------------------*/
;*    get-default-c-type ...                                           */
;*    -------------------------------------------------------------    */
;*    The default type for extern function.                            */
;*---------------------------------------------------------------------*/
(define (get-default-c-type)
   *int*)

;*---------------------------------------------------------------------*/
;*    get-bigloo-type ...                                              */
;*    -------------------------------------------------------------    */
;*    Find the corresponding Bigloo (boxed) type.                      */
;*---------------------------------------------------------------------*/
(define (get-bigloo-type type)
   (cond
      ((bigloo-type? type) type)
      ((or (eq? type *int*) (eq? type *long*)) *bint*)
      ((eq? type *elong*) *belong*)
      ((eq? type *llong*) *bllong*)
      ((eq? type *int8*) *bint8*)
      ((eq? type *uint8*) *buint8*)
      ((eq? type *int16*) *bint16*)
      ((eq? type *uint16*) *buint16*)
      ((eq? type *int32*) *bint32*)
      ((eq? type *uint32*) *buint32*)
      ((eq? type *int64*) *bint64*)
      ((eq? type *uint64*) *buint64*)
      ((eq? type *bool*) *bbool*)
      ((eq? type *real*) *breal*)
      ((eq? type *char*) *bchar*)
      ((eq? type *string*) *bstring*)
      (else *obj*)))
      
;*---------------------------------------------------------------------*/
;*    get-bigloo-defined-type ...                                      */
;*---------------------------------------------------------------------*/
(define (get-bigloo-defined-type t::type)
   (if (eq? t *_*)
       *obj*
       (get-bigloo-type t)))
      
