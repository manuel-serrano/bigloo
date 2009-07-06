;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/srfi4.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov  6 16:28:39 2006                          */
;*    Last change :  Thu Jun  5 11:00:11 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo srfi-4 implementation                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi4
   
   (import  __error
	    __hash
	    __reader
	    __param
	    __r4_symbols_6_4)
   
   (use     __type
	    __bigloo
	    __structure
	    __bexit
	    __bignum
	    __object
	    __thread
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_vectors_6_8
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r4_output_6_10_3
	    __r4_ports_6_10_1
	    __r4_control_features_6_9
	    __r5_control_features_6_4
	    
	    __evenv)

   (extern (macro $hvector?::bool (::obj) "BGL_HVECTORP")
	   (macro $s8vector?::bool (::obj) "BGL_S8VECTORP")
	   (macro $u8vector?::bool (::obj) "BGL_U8VECTORP")
	   (macro $s16vector?::bool (::obj) "BGL_S16VECTORP")
	   (macro $u16vector?::bool (::obj) "BGL_U16VECTORP")
	   (macro $s32vector?::bool (::obj) "BGL_S32VECTORP")
	   (macro $u32vector?::bool (::obj) "BGL_U32VECTORP")
	   (macro $s32vector?::bool (::obj) "BGL_S32VECTORP")
	   (macro $u32vector?::bool (::obj) "BGL_U32VECTORP")
	   (macro $s64vector?::bool (::obj) "BGL_S64VECTORP")
	   (macro $u64vector?::bool (::obj) "BGL_U64VECTORP")
	   (macro $f32vector?::bool (::obj) "BGL_F32VECTORP")
	   (macro $f64vector?::bool (::obj) "BGL_F64VECTORP")

	   (macro $hvector-ident::int (::obj) "BGL_HVECTOR_IDENT")

	   (macro $hvector-length::long (::obj) "BGL_HVECTOR_LENGTH")
	   (macro $hvector-bound-check?::bool (::int ::int) "BOUND_CHECK")

	   (macro $alloc-s8vector::s8vector (::long) "BGL_ALLOC_S8VECTOR")
	   (macro $alloc-u8vector::u8vector (::long) "BGL_ALLOC_U8VECTOR")
	   (macro $alloc-s16vector::s16vector (::long) "BGL_ALLOC_S16VECTOR")
	   (macro $alloc-u16vector::u16vector (::long) "BGL_ALLOC_U16VECTOR")
	   (macro $alloc-s32vector::s32vector (::long) "BGL_ALLOC_S32VECTOR")
	   (macro $alloc-u32vector::u32vector (::long) "BGL_ALLOC_U32VECTOR")
	   (macro $alloc-s64vector::s64vector (::long) "BGL_ALLOC_S64VECTOR")
	   (macro $alloc-u64vector::u64vector (::long) "BGL_ALLOC_U64VECTOR")
	   (macro $alloc-f32vector::f32vector (::long) "BGL_ALLOC_F32VECTOR")
	   (macro $alloc-f64vector::f64vector (::long) "BGL_ALLOC_F64VECTOR")

	   (macro $s8vector-ref::byte (::s8vector ::long)
		  "BGL_S8VREF")
	   (macro $s8vector-set!::void (::s8vector ::long ::byte)
		  "BGL_S8VSET")
	   (macro $u8vector-ref::ubyte (::u8vector ::long)
		  "BGL_U8VREF")
	   (macro $u8vector-set!::void (::u8vector ::long ::ubyte)
		  "BGL_U8VSET")
	   (macro $s16vector-ref::short (::s16vector ::long)
		  "BGL_S16VREF")
	   (macro $s16vector-set!::void (::s16vector ::long ::short)
		  "BGL_S16VSET")
	   (macro $u16vector-ref::ushort (::u16vector ::long)
		  "BGL_U16VREF")
	   (macro $u16vector-set!::void (::u16vector ::long ::ushort)
		  "BGL_U16VSET")
	   (macro $s32vector-ref::long (::s32vector ::long)
		  "BGL_S32VREF")
	   (macro $s32vector-set!::void (::s32vector ::long ::long)
		  "BGL_S32VSET")
	   (macro $u32vector-ref::ulong (::u32vector ::long)
		  "BGL_U32VREF")
	   (macro $u32vector-set!::void (::u32vector ::long ::ulong)
		  "BGL_U32VSET")
	   (macro $s64vector-ref::llong (::s64vector ::long)
		  "BGL_S64VREF")
	   (macro $s64vector-set!::void (::s64vector ::long ::llong)
		  "BGL_S64VSET")
	   (macro $u64vector-ref::ullong (::u64vector ::long)
		  "BGL_U64VREF")
	   (macro $u64vector-set!::void (::u64vector ::long ::ullong)
		  "BGL_U64VSET")
	   (macro $f32vector-ref::float (::f32vector ::long)
		  "BGL_F32VREF")
	   (macro $f32vector-set!::void (::f32vector ::long ::float)
		  "BGL_F32VSET")
	   (macro $f64vector-ref::double (::f64vector ::long)
		  "BGL_F64VREF")
	   (macro $f64vector-set!::void (::f64vector ::long ::double)
		  "BGL_F64VSET"))

   (java   (class foreign
	      (method static $hvector?::bool (::obj)
		      "BGL_HVECTORP")
	      (method static $s8vector?::bool (::obj)
		      "BGL_S8VECTORP")
	      (method static $u8vector?::bool (::obj)
		      "BGL_U8VECTORP")
	      (method static $s16vector?::bool (::obj)
		      "BGL_S16VECTORP")
	      (method static $u16vector?::bool (::obj)
		      "BGL_U16VECTORP")
	      (method static $s32vector?::bool (::obj)
		      "BGL_S32VECTORP")
	      (method static $u32vector?::bool (::obj)
		      "BGL_U32VECTORP")
	      (method static $s32vector?::bool (::obj)
		      "BGL_S32VECTORP")
	      (method static $u32vector?::bool (::obj)
		      "BGL_U32VECTORP")
	      (method static $s64vector?::bool (::obj)
		      "BGL_S64VECTORP")
	      (method static $u64vector?::bool (::obj)
		      "BGL_U64VECTORP")
	      (method static $f32vector?::bool (::obj)
		      "BGL_F32VECTORP")
	      (method static $f64vector?::bool (::obj)
		      "BGL_F64VECTORP")
	      
	      (method static $hvector-ident::int (::obj)
		      "BGL_HVECTOR_IDENT")
	      
	      (method static $hvector-length::long (::obj)
		      "BGL_HVECTOR_LENGTH")
	      (method static $hvector-bound-check?::bool (::int ::int)
		      "BOUND_CHECK")
	      
	      (method static $alloc-s8vector::s8vector (::long)
		      "BGL_ALLOC_S8VECTOR")
	      (method static $alloc-u8vector::u8vector (::long)
		      "BGL_ALLOC_U8VECTOR")
	      (method static $alloc-s16vector::s16vector (::long)
		      "BGL_ALLOC_S16VECTOR")
	      (method static $alloc-u16vector::u16vector (::long)
		      "BGL_ALLOC_U16VECTOR")
	      (method static $alloc-s32vector::s32vector (::long)
		      "BGL_ALLOC_S32VECTOR")
	      (method static $alloc-u32vector::u32vector (::long)
		      "BGL_ALLOC_U32VECTOR")
	      (method static $alloc-s64vector::s64vector (::long)
		      "BGL_ALLOC_S64VECTOR")
	      (method static $alloc-u64vector::u64vector (::long)
		      "BGL_ALLOC_U64VECTOR")
	      (method static $alloc-f32vector::f32vector (::long)
		      "BGL_ALLOC_F32VECTOR")
	      (method static $alloc-f64vector::f64vector (::long)
		      "BGL_ALLOC_F64VECTOR")
	      
	      (method static $s8vector-ref::byte (::s8vector ::long)
		      "BGL_S8VREF")
	      (method static $s8vector-set!::void (::s8vector ::long ::byte)
		      "BGL_S8VSET")
	      (method static $u8vector-ref::ubyte (::u8vector ::long)
		      "BGL_U8VREF")
	      (method static $u8vector-set!::void (::u8vector ::long ::ubyte)
		      "BGL_U8VSET")
	      (method static $s16vector-ref::short (::s16vector ::long)
		      "BGL_S16VREF")
	      (method static $s16vector-set!::void (::s16vector ::long ::short)
		      "BGL_S16VSET")
	      (method static $u16vector-ref::ushort (::u16vector ::long)
		      "BGL_U16VREF")
	      (method static $u16vector-set!::void (::u16vector ::long ::ushort)
		      "BGL_U16VSET")
	      (method static $s32vector-ref::long (::s32vector ::long)
		      "BGL_S32VREF")
	      (method static $s32vector-set!::void (::s32vector ::long ::long)
		      "BGL_S32VSET")
	      (method static $u32vector-ref::ulong (::u32vector ::long)
		      "BGL_U32VREF")
	      (method static $u32vector-set!::void (::u32vector ::long ::ulong)
		      "BGL_U32VSET")
    	      (method static $s64vector-ref::llong (::s64vector ::long)
		     "BGL_S64VREF")
	      (method static $s64vector-set!::void (::s64vector ::long ::llong)
		     "BGL_S64VSET")
	      (method static $u64vector-ref::ullong (::u64vector ::long)
		     "BGL_U64VREF")
	      (method static $u64vector-set!::void (::u64vector ::long ::ullong)
		     "BGL_U64VSET")
	      (method static $f32vector-ref::float (::f32vector ::long)
		      "BGL_F32VREF")
	      (method static $f32vector-set!::void (::f32vector ::long ::float)
		      "BGL_F32VSET")
	      (method static $f64vector-ref::double (::f64vector ::long)
		      "BGL_F64VREF")
	      (method static $f64vector-set!::void (::f64vector ::long ::double)
		      "BGL_F64VSET")))

   (export (inline homogeneous-vector? ::obj)
	   (inline s8vector?::bool ::obj)
	   (inline u8vector?::bool ::obj)
	   (inline s16vector?::bool ::obj)
	   (inline u16vector?::bool ::obj)
	   (inline s32vector?::bool ::obj)
	   (inline u32vector?::bool ::obj)
	   (inline s64vector?::bool ::obj)
	   (inline u64vector?::bool ::obj)
	   (inline f32vector?::bool ::obj)
	   (inline f64vector?::bool ::obj)

	   (homogeneous-vector-info ::obj)

	   (make-s8vector::s8vector ::long #!optional (init 0))
	   (make-u8vector::u8vector ::long #!optional (init 0))
	   (make-s16vector::s16vector ::long #!optional (init 0))
	   (make-u16vector::u16vector ::long #!optional (init 0))
	   (make-s32vector::s32vector ::long #!optional (init 0))
	   (make-u32vector::u32vector ::long #!optional (init 0))
	   (make-s64vector::s64vector ::long #!optional (init #l0))
	   (make-u64vector::u64vector ::long #!optional (init #l0))
	   (make-f32vector::f32vector ::long #!optional (init 0.0))
	   (make-f64vector::f64vector ::long #!optional (init 0.0))

	   (s8vector::s8vector . obj)
	   (u8vector::u8vector . obj)
	   (s16vector::s16vector . obj)
	   (u16vector::u16vector . obj)
	   (s32vector::s32vector . obj)
	   (u32vector::u32vector . obj)
	   (s64vector::s64vector . obj)
	   (u64vector::u64vector . obj)
	   (f32vector::f32vector . obj)
	   (f64vector::f64vector . obj)

	   (inline s8vector-length::long ::s8vector)
	   (inline u8vector-length::long ::u8vector)
	   (inline s16vector-length::long ::s16vector)
	   (inline u16vector-length::long ::u16vector)
	   (inline s32vector-length::long ::s32vector)
	   (inline u32vector-length::long ::u32vector)
 	   (inline s64vector-length::long ::s64vector)
	   (inline u64vector-length::long ::u64vector)
	   (inline f32vector-length::long ::f32vector)
	   (inline f64vector-length::long ::f64vector)

	   (list->s8vector::s8vector ::pair-nil)
	   (list->u8vector::u8vector ::pair-nil)
	   (list->s16vector::s16vector ::pair-nil)
	   (list->u16vector::u16vector ::pair-nil)
	   (list->s32vector::s32vector ::pair-nil)
	   (list->u32vector::u32vector ::pair-nil)
	   (list->s64vector::s64vector ::pair-nil)
	   (list->u64vector::u64vector ::pair-nil)
	   (list->f32vector::f32vector ::pair-nil)
	   (list->f64vector::f64vector ::pair-nil)

	   (s8vector->list::pair-nil ::s8vector)
	   (u8vector->list::pair-nil ::u8vector)
	   (s16vector->list::pair-nil ::s16vector)
	   (u16vector->list::pair-nil ::u16vector)
	   (s32vector->list::pair-nil ::s32vector)
	   (u32vector->list::pair-nil ::u32vector)
	   (s64vector->list::pair-nil ::s64vector)
	   (u64vector->list::pair-nil ::u64vector)
	   (f32vector->list::pair-nil ::f32vector)
	   (f64vector->list::pair-nil ::f64vector)

	   (inline s8vector-ref::byte ::s8vector ::long)
	   (inline s8vector-set! ::s8vector ::long ::byte)
	   (inline u8vector-ref::ubyte ::u8vector ::long)
	   (inline u8vector-set! ::u8vector ::long ::ubyte)
	   (inline s16vector-ref::short ::s16vector ::long)
	   (inline s16vector-set! ::s16vector ::long ::short)
	   (inline u16vector-ref::ushort ::u16vector ::long)
	   (inline u16vector-set! ::u16vector ::long ::ushort)
	   (inline s32vector-ref::long ::s32vector ::long)
	   (inline s32vector-set! ::s32vector ::long ::long)
	   (inline u32vector-ref::ulong ::u32vector ::long)
	   (inline u32vector-set! ::u32vector ::long ::ulong)
	   (inline s64vector-ref::llong ::s64vector ::long)
	   (inline s64vector-set! ::s64vector ::long ::llong)
	   (inline u64vector-ref::ullong ::u64vector ::long)
	   (inline u64vector-set! ::u64vector ::long ::ullong)
	   (inline f32vector-ref::float ::f32vector ::long)
	   (inline f32vector-set! ::f32vector ::long ::float)
	   (inline f64vector-ref::double ::f64vector ::long)
	   (inline f64vector-set! ::f64vector ::long ::double)))

;*---------------------------------------------------------------------*/
;*    define-hvector ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (define-hvector constructor)
   `(begin
       (,constructor s 8)
       (,constructor u 8)
       (,constructor s 16)
       (,constructor u 16)
       (,constructor s 32)
       (,constructor u 32)
       (,constructor s 64)
       (,constructor u 64)
       (,constructor f 32)
       (,constructor f 64)))

;*---------------------------------------------------------------------*/
;*    hvector?                                                         */
;*---------------------------------------------------------------------*/
(define-inline (homogeneous-vector? x) ($hvector? x))
(define-inline (s8vector? x) ($s8vector? x))
(define-inline (u8vector? x) ($u8vector? x))
(define-inline (s16vector? x) ($s16vector? x))
(define-inline (u16vector? x) ($u16vector? x))
(define-inline (s32vector? x) ($s32vector? x))
(define-inline (u32vector? x) ($u32vector? x))
(define-inline (s64vector? x) ($s64vector? x))
(define-inline (u64vector? x) ($u64vector? x))
(define-inline (f32vector? x) ($f32vector? x))
(define-inline (f64vector? x) ($f64vector? x))

;*---------------------------------------------------------------------*/
;*    homogeneous-vector-info ...                                      */
;*---------------------------------------------------------------------*/
(define (homogeneous-vector-info o)
   (if ($hvector? o)
       (case ($hvector-ident o)
	  ;; the function $hvector-ident assumes a strict ordering
	  ;; for hvector type definitions (see bigloo.h)
	  ((0)
	   (values 's8 1 s8vector-ref s8vector-set!))
	  ((1)
	   (values 'u8 1 u8vector-ref u8vector-set!))
	  ((2)
	   (values 's16 2 s16vector-ref s16vector-set!))
	  ((3)
	   (values 'u16 2 u16vector-ref u16vector-set!))
	  ((4)
	   (values 's32 4 s32vector-ref s32vector-set!))
	  ((5)
	   (values 'u32 4 u32vector-ref u32vector-set!))
	  ((6)
	   (values 's64 8 s64vector-ref s64vector-set!))
	  ((7)
	   (values 'u64 8 u64vector-ref u64vector-set!))
	  ((8)
	   (values 'f32 4 f32vector-ref f32vector-set!))
	  ((9)
	   (values 'f64 8 f64vector-ref f64vector-set!))
	  (else
	   (error 'homogeneous-vector-info
		  "Illegal hvector ident"
		  ($hvector-ident o))))
       (bigloo-type-error 'homogeneous-vector-info "hvector" o)))

;*---------------------------------------------------------------------*/
;*    hvector-length ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (s8vector-length x) ($hvector-length x))
(define-inline (u8vector-length x) ($hvector-length x))
(define-inline (s16vector-length x) ($hvector-length x))
(define-inline (u16vector-length x) ($hvector-length x))
(define-inline (s32vector-length x) ($hvector-length x))
(define-inline (u32vector-length x) ($hvector-length x))
(define-inline (s64vector-length x) ($hvector-length x))
(define-inline (u64vector-length x) ($hvector-length x))
(define-inline (f32vector-length x) ($hvector-length x))
(define-inline (f64vector-length x) ($hvector-length x))

;*---------------------------------------------------------------------*/
;*    define-hvector-alloc ...                                         */
;*---------------------------------------------------------------------*/
(define-macro (define-hvector-alloc sign sz)
   (let* ((vector (symbol-append sign
			      (string->symbol (integer->string sz)) 'vector))
	  (lid (symbol-append 'list-> vector)))
      `(define (,vector . x) (,lid x))))

(define-hvector define-hvector-alloc)

;*---------------------------------------------------------------------*/
;*    define-make-hvector ...                                          */
;*---------------------------------------------------------------------*/
(define-macro (define-make-hvector sign sz init)
   (let* ((tid (symbol-append sign
			      (string->symbol (integer->string sz)) 'vector))
	  (make (symbol-append 'make- tid))
	  (aid (symbol-append '$alloc- tid))
	  (vsetid (symbol-append '$ tid '-set!)))
      `(define (,make len #!optional (init ,init))
	  (let ((v (,aid len)))
	     (unless (= init 0)
		(let loop ((i 0))
		   (when (<fx i len)
		      (,vsetid v i init)
		      (loop (+fx i 1)))))
	     v))))

(define-make-hvector s 8 0)
(define-make-hvector u 8 0)
(define-make-hvector s 16 0)
(define-make-hvector u 16 0)
(define-make-hvector s 32 0)
(define-make-hvector u 32 0)
(define-make-hvector s 64 #l0)
(define-make-hvector u 64 #l0)
(define-make-hvector f 32 0.0)
(define-make-hvector f 64 0.0)

;*---------------------------------------------------------------------*/
;*    hvector-ref                                                      */
;*---------------------------------------------------------------------*/
(define-inline (s8vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($s8vector-ref v k)
       (error '$s8vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

(define-inline (u8vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($u8vector-ref v k)
       (error '$u8vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

(define-inline (s16vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($s16vector-ref v k)
       (error '$s16vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

(define-inline (u16vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($u16vector-ref v k)
       (error '$u16vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

(define-inline (s32vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($s32vector-ref v k)
       (error '$s32vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

(define-inline (u32vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($u32vector-ref v k)
       (error '$u32vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

(define-inline (s64vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($s64vector-ref v k)
       (error '$s64vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

(define-inline (u64vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))                
       ($u64vector-ref v k)                                         
       (error '$u64vector-ref                                       
 	      (string-append "index out of range [0.."                 
 			     (integer->string (-fx ($hvector-length v) 1))
 			     "]")                                      
 	      k)))                                                     

(define-inline (f32vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($f32vector-ref v k)
       (error '$f32vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

(define-inline (f64vector-ref v k)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($f64vector-ref v k)
       (error '$f64vector-ref
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

;*---------------------------------------------------------------------*/
;*    hvector-set! ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (s8vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($s8vector-set! v k val)
       (error '$s8vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (u8vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($u8vector-set! v k val)
       (error '$u8vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (s16vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($s16vector-set! v k val)
       (error '$s16vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (u16vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($u16vector-set! v k val)
       (error '$u16vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (s32vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($s32vector-set! v k val)
       (error '$s32vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (u32vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($u32vector-set! v k val)
       (error '$u32vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (s64vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($s64vector-set! v k val)
       (error '$s64vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (u64vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($u64vector-set! v k val)
       (error '$u64vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (f32vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($f32vector-set! v k val)
       (error '$f32vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))
(define-inline (f64vector-set! v k val)
   (if ($hvector-bound-check? k ($hvector-length v))
       ($f64vector-set! v k val)
       (error '$f64vector-set!
	      (string-append "index out of range [0.."
			     (integer->string (-fx ($hvector-length v) 1))
			     "]")
	      k)))

;*---------------------------------------------------------------------*/
;*    define-hvector->list ...                                         */
;*---------------------------------------------------------------------*/
(define-macro (define-hvector->list sign sz)
   (let* ((tid (symbol-append sign
			      (string->symbol (integer->string sz)) 'vector))
	  (->list (symbol-append tid '->list))
	  (lenid (symbol-append tid '-length))
	  (vrefid (symbol-append '$ tid '-ref)))
      `(define (,->list x)
	  (let loop ((i ($hvector-length x))
		     (res '()))
	     (if (=fx i 0)
		 res
		 (let ((ni (-fx i 1)))
		    (loop ni (cons (,vrefid x ni) res))))))))

(define-hvector define-hvector->list)

;*---------------------------------------------------------------------*/
;*    define-list->hvector ...                                         */
;*---------------------------------------------------------------------*/
(define-macro (define-list->hvector sign sz)
   (let* ((tid (symbol-append sign
			      (string->symbol (integer->string sz)) 'vector))
	  (list-> (symbol-append 'list-> tid))
	  (cid (symbol-append '$alloc- tid))
	  (vsetid (symbol-append '$ tid '-set!)))
      `(define (,list-> x)
	  (let* ((len (length x))
		 (vec (,cid len)))
	     (let loop ((i 0)
			(l x))
		(if (=fx i len)
		    vec
		    (begin
		       (,vsetid vec i (car l))
		       (loop (+fx i 1) (cdr l)))))))))

(define-hvector define-list->hvector)
