;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/srfi4.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov  6 16:28:39 2006                          */
;*    Last change :  Fri Nov 14 08:05:50 2014 (serrano)                */
;*    Copyright   :  2006-14 Manuel Serrano                            */
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
	    __bit
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
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
	   
	   (macro $alloc-s8vector::s8vector (::int32) "BGL_ALLOC_S8VECTOR")
	   (macro $alloc-u8vector::u8vector (::int32) "BGL_ALLOC_U8VECTOR")
	   (macro $alloc-s16vector::s16vector (::int32) "BGL_ALLOC_S16VECTOR")
	   (macro $alloc-u16vector::u16vector (::int32) "BGL_ALLOC_U16VECTOR")
	   (macro $alloc-s32vector::s32vector (::int32) "BGL_ALLOC_S32VECTOR")
	   (macro $alloc-u32vector::u32vector (::int32) "BGL_ALLOC_U32VECTOR")
	   (macro $alloc-s64vector::s64vector (::int32) "BGL_ALLOC_S64VECTOR")
	   (macro $alloc-u64vector::u64vector (::int32) "BGL_ALLOC_U64VECTOR")
	   (macro $alloc-f32vector::f32vector (::int32) "BGL_ALLOC_F32VECTOR")
	   (macro $alloc-f64vector::f64vector (::int32) "BGL_ALLOC_F64VECTOR")
	   
	   (macro $s8vector-ref::int8 (::s8vector ::long)
		  "BGL_S8VREF")
	   (macro $s8vector-set!::void (::s8vector ::long ::int8)
		  "BGL_S8VSET")
	   (macro $s8vector-ref-ur::int8 (::s8vector ::long)
		  "BGL_S8VREF")
	   (macro $s8vector-set-ur!::void (::s8vector ::long ::int8)
		  "BGL_S8VSET")
	   
	   (macro $u8vector-ref::uint8 (::u8vector ::long)
		  "BGL_U8VREF")
	   (macro $u8vector-set!::void (::u8vector ::long ::uint8)
		  "BGL_U8VSET")
	   (macro $u8vector-ref-ur::uint8 (::u8vector ::long)
		  "BGL_U8VREF")
	   (macro $u8vector-set-ur!::void (::u8vector ::long ::uint8)
		  "BGL_U8VSET")
	   
	   (macro $s16vector-ref::int16 (::s16vector ::long)
		  "BGL_S16VREF")
	   (macro $s16vector-set!::void (::s16vector ::long ::int16)
		  "BGL_S16VSET")
	   (macro $s16vector-ref-ur::int16 (::s16vector ::long)
		  "BGL_S16VREF")
	   (macro $s16vector-set-ur!::void (::s16vector ::long ::int16)
		  "BGL_S16VSET")
	   
	   (macro $u16vector-ref::uint16 (::u16vector ::long)
		  "BGL_U16VREF")
	   (macro $u16vector-set!::void (::u16vector ::long ::uint16)
		  "BGL_U16VSET")
	   (macro $u16vector-ref-ur::uint16 (::u16vector ::long)
		  "BGL_U16VREF")
	   (macro $u16vector-set-ur!::void (::u16vector ::long ::uint16)
		  "BGL_U16VSET")
	   
	   (macro $s32vector-ref::int32 (::s32vector ::long)
		  "BGL_S32VREF")
	   (macro $s32vector-set!::void (::s32vector ::long ::int32)
		  "BGL_S32VSET")
	   (macro $s32vector-ref-ur::int32 (::s32vector ::long)
		  "BGL_S32VREF")
	   (macro $s32vector-set-ur!::void (::s32vector ::long ::int32)
		  "BGL_S32VSET")
	   
	   (macro $u32vector-ref::uint32 (::u32vector ::long)
		  "BGL_U32VREF")
	   (macro $u32vector-set!::void (::u32vector ::long ::uint32)
		  "BGL_U32VSET")
	   (macro $u32vector-ref-ur::uint32 (::u32vector ::long)
		  "BGL_U32VREF")
	   (macro $u32vector-set-ur!::void (::u32vector ::long ::uint32)
		  "BGL_U32VSET")
	   
	   (macro $s64vector-ref::int64 (::s64vector ::long)
		  "BGL_S64VREF")
	   (macro $s64vector-set!::void (::s64vector ::long ::int64)
		  "BGL_S64VSET")
	   (macro $s64vector-ref-ur::int64 (::s64vector ::long)
		  "BGL_S64VREF")
	   (macro $s64vector-set-ur!::void (::s64vector ::long ::int64)
		  "BGL_S64VSET")
	   
	   (macro $u64vector-ref::uint64 (::u64vector ::long)
		  "BGL_U64VREF")
	   (macro $u64vector-set!::void (::u64vector ::long ::uint64)
		  "BGL_U64VSET")
	   (macro $u64vector-ref-ur::uint64 (::u64vector ::long)
		  "BGL_U64VREF")
	   (macro $u64vector-set-ur!::void (::u64vector ::long ::uint64)
		  "BGL_U64VSET")
	   
	   (macro $f32vector-ref::float (::f32vector ::long)
		  "BGL_F32VREF")
	   (macro $f32vector-set!::void (::f32vector ::long ::float)
		  "BGL_F32VSET")
	   (macro $f32vector-ref-ur::float (::f32vector ::long)
		  "BGL_F32VREF")
	   (macro $f32vector-set-ur!::void (::f32vector ::long ::float)
		  "BGL_F32VSET")
	   
	   (macro $f64vector-ref::double (::f64vector ::long)
		  "BGL_F64VREF")
	   (macro $f64vector-set!::void (::f64vector ::long ::double)
		  "BGL_F64VSET")
	   (macro $f64vector-ref-ur::double (::f64vector ::long)
		  "BGL_F64VREF")
	   (macro $f64vector-set-ur!::void (::f64vector ::long ::double)
		  "BGL_F64VSET")
	   
	   (macro $s16/u8vector-ref::int16 (::u8vector ::long)
		  "BGL_S16_U8VREF")
	   (macro $s16/u8vector-set!::void (::u8vector ::long ::int16)
		  "BGL_S16_U8VSET")
	   (macro $u16/u8vector-ref::uint16 (::u8vector ::long)
		  "BGL_U16_U8VREF")
	   (macro $u16/u8vector-set!::void (::u8vector ::long ::uint16)
		  "BGL_U16_U8VSET")
	   
	   (macro $s32/u8vector-ref::int32 (::u8vector ::long)
		  "BGL_S32_U8VREF")
	   (macro $s32/u8vector-set!::void (::u8vector ::long ::int32)
		  "BGL_S32_U8VSET")
	   (macro $u32/u8vector-ref::uint32 (::u8vector ::long)
		  "BGL_U32_U8VREF")
	   (macro $u32/u8vector-set!::void (::u8vector ::long ::uint32)
		  "BGL_U32_U8VSET")

	   (macro $s64/u8vector-ref::int64 (::u8vector ::long)
		  "BGL_S64_U8VREF")
	   (macro $s64/u8vector-set!::void (::u8vector ::long ::int64)
		  "BGL_S64_U8VSET")
	   (macro $u64/u8vector-ref::uint64 (::u8vector ::long)
		  "BGL_U64_U8VREF")
	   (macro $u64/u8vector-set!::void (::u8vector ::long ::uint64)
		  "BGL_U64_U8VSET")

	   (macro $f32/u8vector-ref::float (::u8vector ::long)
		  "BGL_F32_U8VREF")
	   (macro $f32/u8vector-set!::void (::u8vector ::long ::float)
		  "BGL_F32_U8VSET")
	   
	   (macro $f64/u8vector-ref::double (::u8vector ::long)
		  "BGL_F64_U8VREF")
	   (macro $f64/u8vector-set!::void (::u8vector ::long ::double)
		  "BGL_F64_U8VSET")

	   (macro $s8vector-copy!::void (::s8vector ::long ::s8vector ::long ::long)
		  "BGL_SU8VECTOR_COPY")
	   (macro $u8vector-copy!::void (::u8vector ::long ::u8vector ::long ::long)
		  "BGL_SU8VECTOR_COPY")
	   (macro $s16vector-copy!::void (::s16vector ::long ::s16vector ::long ::long)
		  "BGL_SU16VECTOR_COPY")
	   (macro $u16vector-copy!::void (::u16vector ::long ::u16vector ::long ::long)
		  "BGL_SU16VECTOR_COPY")
	   (macro $s32vector-copy!::void (::s32vector ::long ::s32vector ::long ::long)
		  "BGL_SU32VECTOR_COPY")
	   (macro $u32vector-copy!::void (::u32vector ::long ::u32vector ::long ::long)
		  "BGL_SU32VECTOR_COPY")
	   (macro $s64vector-copy!::void (::s64vector ::long ::s64vector ::long ::long)
		  "BGL_SU64VECTOR_COPY")
	   (macro $u64vector-copy!::void (::u64vector ::long ::u64vector ::long ::long)
		  "BGL_SU64VECTOR_COPY")
	   (macro $f32vector-copy!::void (::f32vector ::long ::f32vector ::long ::long)
		  "BGL_F32VECTOR_COPY")
	   (macro $f64vector-copy!::void (::f64vector ::long ::f64vector ::long ::long)
		  "BGL_F64VECTOR_COPY")
	   )
   
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
	      
	      (method static $s8vector-ref::int8 (::s8vector ::long)
		 "BGL_S8VREF")
	      (method static $s8vector-set!::void (::s8vector ::long ::int8)
		 "BGL_S8VSET")
	      (method static $s8vector-ref-ur::int8 (::s8vector ::long)
		 "BGL_S8VREF")
	      (method static $s8vector-set-ur!::void (::s8vector ::long ::int8)
		 "BGL_S8VSET")
	      
	      (method static $u8vector-ref::uint8 (::u8vector ::long)
		 "BGL_U8VREF")
	      (method static $u8vector-set!::void (::u8vector ::long ::uint8)
		 "BGL_U8VSET")
	      (method static $u8vector-ref-ur::uint8 (::u8vector ::long)
		 "BGL_U8VREF")
	      (method static $u8vector-set-ur!::void (::u8vector ::long ::uint8)
		 "BGL_U8VSET")
	      
	      (method static $s16vector-ref::int16 (::s16vector ::long)
		 "BGL_S16VREF")
	      (method static $s16vector-set!::void (::s16vector ::long ::int16)
		 "BGL_S16VSET")
	      (method static $s16vector-ref-ur::int16 (::s16vector ::long)
		 "BGL_S16VREF")
	      (method static $s16vector-set-ur!::void (::s16vector ::long ::int16)
		 "BGL_S16VSET")
	      
	      (method static $u16vector-ref::uint16 (::u16vector ::long)
		 "BGL_U16VREF")
	      (method static $u16vector-set!::void (::u16vector ::long ::uint16)
		 "BGL_U16VSET")
	      (method static $u16vector-ref-ur::uint16 (::u16vector ::long)
		 "BGL_U16VREF")
	      (method static $u16vector-set-ur!::void (::u16vector ::long ::uint16)
		 "BGL_U16VSET")
	      
	      (method static $s32vector-ref::int32 (::s32vector ::long)
		 "BGL_S32VREF")
	      (method static $s32vector-set!::void (::s32vector ::long ::int32)
		 "BGL_S32VSET")
	      (method static $s32vector-ref-ur::int32 (::s32vector ::long)
		 "BGL_S32VREF")
	      (method static $s32vector-set-ur!::void (::s32vector ::long ::int32)
		 "BGL_S32VSET")
	      
	      (method static $u32vector-ref::uint32 (::u32vector ::long)
		 "BGL_U32VREF")
	      (method static $u32vector-set!::void (::u32vector ::long ::uint32)
		 "BGL_U32VSET")
	      (method static $u32vector-ref-ur::uint32 (::u32vector ::long)
		 "BGL_U32VREF")
	      (method static $u32vector-set-ur!::void (::u32vector ::long ::uint32)
		 "BGL_U32VSET")
	      
    	      (method static $s64vector-ref::int64 (::s64vector ::long)
		 "BGL_S64VREF")
	      (method static $s64vector-set!::void (::s64vector ::long ::int64)
		 "BGL_S64VSET")
    	      (method static $s64vector-ref-ur::int64 (::s64vector ::long)
		 "BGL_S64VREF")
	      (method static $s64vector-set-ur!::void (::s64vector ::long ::int64)
		 "BGL_S64VSET")
	      
	      (method static $u64vector-ref::uint64 (::u64vector ::long)
		 "BGL_U64VREF")
	      (method static $u64vector-set!::void (::u64vector ::long ::uint64)
		 "BGL_U64VSET")
	      (method static $u64vector-ref-ur::uint64 (::u64vector ::long)
		 "BGL_U64VREF")
	      (method static $u64vector-set-ur!::void (::u64vector ::long ::uint64)
		 "BGL_U64VSET")
	      
	      (method static $f32vector-ref::float (::f32vector ::long)
		 "BGL_F32VREF")
	      (method static $f32vector-set!::void (::f32vector ::long ::float)
		 "BGL_F32VSET")
	      (method static $f32vector-ref-ur::float (::f32vector ::long)
		 "BGL_F32VREF")
	      (method static $f32vector-set-ur!::void (::f32vector ::long ::float)
		 "BGL_F32VSET")
	      
	      (method static $f64vector-ref::double (::f64vector ::long)
		 "BGL_F64VREF")
	      (method static $f64vector-set!::void (::f64vector ::long ::double)
		 "BGL_F64VSET")
	      (method static $f64vector-ref-ur::double (::f64vector ::long)
		 "BGL_F64VREF")
	      (method static $f64vector-set-ur!::void (::f64vector ::long ::double)
		 "BGL_F64VSET")
	      
	      
	      (method static $s16/u8vector-ref::int16 (::u8vector ::long)
		 "BGL_S16_U8VREF")
	      (method static $s16/u8vector-set!::void (::u8vector ::long ::int16)
		 "BGL_S16_U8VSET")
	      (method static $u16/u8vector-ref::int16 (::u8vector ::long)
		 "BGL_S16_U8VREF")
	      (method static $u16/u8vector-set!::void (::u8vector ::long ::int16)
		 "BGL_S16_U8VSET")
	      
	      (method static $s32/u8vector-ref::int32 (::u8vector ::long)
		 "BGL_S32_U8VREF")
	      (method static $s32/u8vector-set!::void (::u8vector ::long ::int32)
		 "BGL_S32_U8VSET")
	      (method static $u32/u8vector-ref::int32 (::u8vector ::long)
		 "BGL_S32_U8VREF")
	      (method static $u32/u8vector-set!::void (::u8vector ::long ::int32)
		 "BGL_S32_U8VSET")

	      (method static $s64/u8vector-ref::int64 (::u8vector ::long)
		 "BGL_S64_U8VREF")
	      (method static $s64/u8vector-set!::void (::u8vector ::long ::int64)
		 "BGL_S64_U8VSET")
	      (method static $u64/u8vector-ref::int64 (::u8vector ::long)
		 "BGL_S64_U8VREF")
	      (method static $u64/u8vector-set!::void (::u8vector ::long ::int64)
		 "BGL_S64_U8VSET")

	      (method static $f32/8vector-ref::float (::u8vector ::long)
		 "BGL_F32_U8VREF")
	      (method static $f32/8vector-set!::void (::u8vector ::long ::float)
		 "BGL_F32_U8VREF")
	      
	      (method static $f64/u8vector-ref::double (::u8vector ::long)
		 "BGL_F64_U8VREF")
	      (method static $f64/u8vector-set!::void (::u8vector ::long ::double)
		 "BGL_F64_U8VREF")
	      
	      (method static $s8vector-copy!::void (::s8vector ::long ::s8vector ::long ::long)
		 "BGL_SU8VECTOR_COPY")
	      (method static $u8vector-copy!::void (::u8vector ::long ::u8vector ::long ::long)
		 "BGL_SU8VECTOR_COPY")
	      (method static $s16vector-copy!::void (::s16vector ::long ::s16vector ::long ::long)
		 "BGL_SU16VECTOR_COPY")
	      (method static $u16vector-copy!::void (::u16vector ::long ::u16vector ::long ::long)
		 "BGL_SU16VECTOR_COPY")
	      (method static $s32vector-copy!::void (::s32vector ::long ::s32vector ::long ::long)
		 "BGL_SU32VECTOR_COPY")
	      (method static $u32vector-copy!::void (::u32vector ::long ::u32vector ::long ::long)
		 "BGL_SU32VECTOR_COPY")
	      (method static $s64vector-copy!::void (::s64vector ::long ::s64vector ::long ::long)
		 "BGL_SU64VECTOR_COPY")
	      (method static $u64vector-copy!::void (::u64vector ::long ::u64vector ::long ::long)
		 "BGL_SU64VECTOR_COPY")
	      (method static $f32vector-copy!::void (::f32vector ::long ::f32vector ::long ::long)
		 "BGL_F32VECTOR_COPY")
	      (method static $f64vector-copy!::void (::f64vector ::long ::f64vector ::long ::long)
		 "BGL_F64VECTOR_COPY")
	      ))
   
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
	   
	   (make-s8vector::s8vector ::long #!optional (init::int8 #s8:0))
	   (make-u8vector::u8vector ::long #!optional (init::uint8 #u8:0))
	   (make-s16vector::s16vector ::long #!optional (init::int16 #s16:0))
	   (make-u16vector::u16vector ::long #!optional (init::uint16 #u16:0))
	   (make-s32vector::s32vector ::long #!optional (init::int32 #s32:0))
	   (make-u32vector::u32vector ::long #!optional (init::uint32 #u32:0))
	   (make-s64vector::s64vector ::long #!optional (init::int64 #s64:0))
	   (make-u64vector::u64vector ::long #!optional (init::uint64 #u64:0))
	   (make-f32vector::f32vector ::long #!optional (init::float 0.0))
	   (make-f64vector::f64vector ::long #!optional (init::double 0.0))
	   
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
	   
	   (hvector-range-error ::bstring ::obj ::long)
	   
	   (inline s8vector-ref::int8 ::s8vector ::long)
	   (inline s8vector-set! ::s8vector ::long ::int8)
	   (inline u8vector-ref::uint8 ::u8vector ::long)
	   (inline u8vector-set! ::u8vector ::long ::uint8)
	   (inline s16vector-ref::int16 ::s16vector ::long)
	   (inline s16vector-set! ::s16vector ::long ::int16)
	   (inline u16vector-ref::uint16 ::u16vector ::long)
	   (inline u16vector-set! ::u16vector ::long ::uint16)
	   (inline s32vector-ref::int32 ::s32vector ::long)
	   (inline s32vector-set! ::s32vector ::long ::int32)
	   (inline u32vector-ref::uint32 ::u32vector ::long)
	   (inline u32vector-set! ::u32vector ::long ::uint32)
	   (inline s64vector-ref::int64 ::s64vector ::long)
	   (inline s64vector-set! ::s64vector ::long ::int64)
	   (inline u64vector-ref::uint64 ::u64vector ::long)
	   (inline u64vector-set! ::u64vector ::long ::uint64)
	   (inline f32vector-ref::float ::f32vector ::long)
	   (inline f32vector-set! ::f32vector ::long ::float)
	   (inline f64vector-ref::double ::f64vector ::long)
	   (inline f64vector-set! ::f64vector ::long ::double)
	   
	   (s8vector-copy! target::s8vector tstart::long source::s8vector
	      #!optional (sstart 0) (send (s8vector-length source)))
	   (u8vector-copy! target::u8vector tstart::long source::u8vector
	      #!optional (sstart 0) (send (u8vector-length source)))
	   (s16vector-copy! target::s16vector tstart::long source::s16vector
	      #!optional (sstart 0) (send (s16vector-length source)))
	   (u16vector-copy! target::u16vector tstart::long source::u16vector
	      #!optional (sstart 0) (send (u16vector-length source)))
	   (s32vector-copy! target::s32vector tstart::long source::s32vector
	      #!optional (sstart 0) (send (s32vector-length source)))
	   (u32vector-copy! target::u32vector tstart::long source::u32vector
	      #!optional (sstart 0) (send (u32vector-length source)))
	   (s64vector-copy! target::s64vector tstart::long source::s64vector
	      #!optional (sstart 0) (send (s64vector-length source)))
	   (u64vector-copy! target::u64vector tstart::long source::u64vector
	      #!optional (sstart 0) (send (u64vector-length source)))
	   (f32vector-copy! target::f32vector tstart::long source::f32vector
	      #!optional (sstart 0) (send (f32vector-length source)))
	   (f64vector-copy! target::f64vector tstart::long source::f64vector
	      #!optional (sstart 0) (send (f64vector-length source)))))

;* 	   (u8vector-copy-from-string! target::u8vector tstart::long source::bstring */
;* 	      #!optional (sstart 0) (send (string-length source)))     */
;* 	   (u8vector-copy-to-string! target::u8vector tstart::long source::bstring */
;* 	      #!optional (sstart 0) (send (string-length source)))))   */

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
	   (values 's8 1 s8vector-ref s8vector-set! =s8))
	  ((1)
	   (values 'u8 1 u8vector-ref u8vector-set! =u8))
	  ((2)
	   (values 's16 2 s16vector-ref s16vector-set! =s16))
	  ((3)
	   (values 'u16 2 u16vector-ref u16vector-set! =u16))
	  ((4)
	   (values 's32 4 s32vector-ref s32vector-set! =s32))
	  ((5)
	   (values 'u32 4 u32vector-ref u32vector-set! =u32))
	  ((6)
	   (values 's64 8 s64vector-ref s64vector-set! =s64))
	  ((7)
	   (values 'u64 8 u64vector-ref u64vector-set! =u64))
	  ((8)
	   (values 'f32 4 f32vector-ref f32vector-set! =fl))
	  ((9)
	   (values 'f64 8 f64vector-ref f64vector-set! =fl))
	  (else
	   (error "homogeneous-vector-info" "Illegal hvector ident"
	      ($hvector-ident o))))
       (bigloo-type-error "homogeneous-vector-info" "hvector" o)))

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
	  (tinit (string->symbol
		    (if (eq? sign 'f)
			(if (eq? sz 32) "init::float" "init::double")
			(format "init::~a~a"
			   (if (eq? sign 's) "int" "uint") sz))))
	  (aid (symbol-append '$alloc- tid))
	  (vsetid (symbol-append '$ tid '-set!)))
      `(define (,make len #!optional (,tinit ,init))
	  (let ((v (,aid len)))
	     (let loop ((i 0))
		(when (<fx i len)
		   (,vsetid v i init)
		   (loop (+fx i 1))))
	     v))))

(define-make-hvector s 8 #s8:0)
(define-make-hvector u 8 #u8:0)
(define-make-hvector s 16 #s16:0)
(define-make-hvector u 16 #u16:0)
(define-make-hvector s 32 #s32:0)
(define-make-hvector u 32 #u32:0)
(define-make-hvector s 64 #s64:0)
(define-make-hvector u 64 #u64:0)
(define-make-hvector f 32 0.0)
(define-make-hvector f 64 0.0)

;*---------------------------------------------------------------------*/
;*    hvector-range-error ...                                          */
;*---------------------------------------------------------------------*/
(define (hvector-range-error fun v k)
   (error fun
      (string-append "index out of range [0.."
	 (integer->string (-fx ($hvector-length v) 1))
	 "]")
      k))
   
;*---------------------------------------------------------------------*/
;*    hvector-ref                                                      */
;*---------------------------------------------------------------------*/
(define-inline (s8vector-ref v k)
   ($s8vector-ref v k))

(define-inline (u8vector-ref v k)
   ($u8vector-ref v k))

(define-inline (s16vector-ref v k)
   ($s16vector-ref v k))

(define-inline (u16vector-ref v k)
   ($u16vector-ref v k))

(define-inline (s32vector-ref v k)
   ($s32vector-ref v k))

(define-inline (u32vector-ref v k)
   ($u32vector-ref v k))

(define-inline (s64vector-ref v k)
   ($s64vector-ref v k))

(define-inline (u64vector-ref v k)
   ($u64vector-ref v k))

(define-inline (f32vector-ref v k)
   ($f32vector-ref v k))

(define-inline (f64vector-ref v k)
   ($f64vector-ref v k))

(define-inline (s32/u8vector-ref v k)
   ($s32/u8vector-ref v k))
   
;*---------------------------------------------------------------------*/
;*    hvector-set! ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (s8vector-set! v k val)
   ($s8vector-set! v k val))

(define-inline (u8vector-set! v k val)
   ($u8vector-set! v k val))

(define-inline (s16vector-set! v k val)
   ($s16vector-set! v k val))

(define-inline (u16vector-set! v k val)
   ($u16vector-set! v k val))

(define-inline (s32vector-set! v k val)
   ($s32vector-set! v k val))

(define-inline (u32vector-set! v k val)
   ($u32vector-set! v k val))

(define-inline (s64vector-set! v k val)
   ($s64vector-set! v k val))

(define-inline (u64vector-set! v k val)
   ($u64vector-set! v k val))

(define-inline (f32vector-set! v k val)
   ($f32vector-set! v k val))

(define-inline (f64vector-set! v k val)
   ($f64vector-set! v k val))

(define-inline (s32/u8vector-set! v k val)
   ($s32/u8vector-set! v k val))

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
	  (vsetid (symbol-append '$ tid '-set!))
	  (name (if (eq? sign 's) "fixnum->int" "fixnum->uint"))
	  (conv (string->symbol (string-append name (integer->string sz)))))
      `(define (,list-> x)
	  (let* ((len (length x))
		 (vec (,cid len)))
	     (let loop ((i 0)
			(l x))
		(if (=fx i len)
		    vec
		    (begin
		       (,vsetid vec i
			  (if (fixnum? (car l)) (,conv (car l)) (car l)))
		       (loop (+fx i 1) (cdr l)))))))))

(define-hvector define-list->hvector)

;*---------------------------------------------------------------------*/
;*    unsafe-range ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (unsafe-range body)
   (if *unsafe-range*
       #unspecified
       body))

;*---------------------------------------------------------------------*/
;*    vector-copy! ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (define-vector-copy! sign sz)
   (let* ((tid (symbol-append sign
		  (string->symbol (integer->string sz)) 'vector))
	  (list-> (symbol-append 'list-> tid))
	  (cid (symbol-append '$alloc- tid))
	  (vsetid (symbol-append '$ tid '-set!))
	  (name (if (eq? sign 's) "fixnum->int" "fixnum->uint"))
	  (conv (string->symbol (string-append name (integer->string sz))))
	  (getlen (string->symbol (format "~a~avector-length" sign sz)))
	  (copy! (string->symbol (format "~a~avector-copy!" sign sz)))
	  ($copy! (symbol-append '$ copy!)))
      `(define (,copy! target tstart source
		  #!optional (sstart 0) (send (,getlen source)))
	  (unsafe-range
	     (cond
		((<fx tstart 0)
		 (error ,(symbol->string copy!) "Illegal target start index" tstart))
		((<fx sstart 0)
		 (error ,(symbol->string copy!) "Illegal source start index" sstart))
		((>fx send (,getlen source))
		 (error ,(symbol->string copy!) "Illegal source end index" send))
		((<fx send sstart)
		 (error ,(symbol->string copy!) "Illegal source end index" send))
		
		((> (-fx send sstart) (,getlen target))
		 (error ,(symbol->string copy!) "Illegal source length"  (-fx send sstart)))))
	  (,$copy! target tstart source sstart send))))


(define-hvector define-vector-copy!)

;* {*---------------------------------------------------------------------*} */
;* {*    u8vector-copy-from-string! ...                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define (u8vector-copy-from-string! target::u8vector tstart::long source::bstring */
;* 	   #!optional (sstart 0) (send (string-length source)))        */
;*    (unsafe-range                                                    */
;*       (cond                                                         */
;* 	 ((<fx tstart 0)                                               */
;* 	  (error "u8vector-copy-from-string!"                          */
;* 	     "Illegal target start index" tstart))                     */
;* 	 ((<fx sstart 0)                                               */
;* 	  (error "u8vector-copy-from-string!"                          */
;* 	     "Illegal source start index" sstart))                     */
;* 	 ((>fx send (string-length source))                            */
;* 	  (error "u8vector-copy-from-string!"                          */
;* 	     "Illegal source end index" send))                         */
;* 	 ((<fx send sstart)                                            */
;* 	  (error "u8vector-copy-from-string!"                          */
;* 	     "Illegal source end index" send))                         */
;* 	 ((> (-fx send sstart) (u8vector-length target))               */
;* 	  (error "u8vector-copy-from-string!"                          */
;* 	     "Illegal source length"  (-fx send sstart)))))            */
;*    ($u8vector-copy-from-string! target tstart source sstart send))  */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    u8vector-copy-to-string! ...                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define (u8vector-copy-to-string! target::u8vector tstart::long source::bstring */
;* 	   #!optional (sstart 0) (send (string-length source)))        */
;*    (unsafe-range                                                    */
;*       (cond                                                         */
;* 	 ((<fx tstart 0)                                               */
;* 	  (error "u8vector-copy-to-string!"                            */
;* 	     "Illegal target start index" tstart))                     */
;* 	 ((<fx sstart 0)                                               */
;* 	  (error "u8vector-copy-to-string!"                            */
;* 	     "Illegal source start index" sstart))                     */
;* 	 ((>fx send (string-length source))                            */
;* 	  (error "u8vector-copy-to-string!"                            */
;* 	     "Illegal source end index" send))                         */
;* 	 ((<fx send sstart)                                            */
;* 	  (error "u8vector-copy-to-string!"                            */
;* 	     "Illegal source end index" send))                         */
;* 	 ((> (-fx send sstart) (u8vector-length target))               */
;* 	  (error "u8vector-copy-to-string!"                            */
;* 	     "Illegal source length"  (-fx send sstart)))))            */
;*    ($u8vector-copy-to-string! target tstart source sstart send))    */
