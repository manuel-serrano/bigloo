/*=====================================================================*/
/*    .../project/bigloo/flt/runtime/Include/bigloo_real_flt.h         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Mar  6 07:07:32 2016                          */
/*    Last change :  Tue Nov 26 11:08:05 2024 (serrano)                */
/*    Copyright   :  2016-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo FLOATING POINT TAGGING reals                              */
/*=====================================================================*/
#ifndef BIGLOO_REAL_FLT_H 
#define BIGLOO_REAL_FLT_H

#include <stdbool.h>

/*---------------------------------------------------------------------*/
/*    Does someone really wants C++ here?                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus_just_for_emacs_indent
}
#endif

/*---------------------------------------------------------------------*/
/*    extern                                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL obj_t bigloo_nan, bigloo_infinity, bigloo_minfinity;
BGL_RUNTIME_DECL obj_t make_real(double);

#if (BGL_SAW == 1) 
BGL_RUNTIME_DECL obj_t bgl_saw_make_real(double);
#endif

/*---------------------------------------------------------------------*/
/*    bgl_real ...                                                     */
/*---------------------------------------------------------------------*/
struct bgl_real {
#if (!defined(TAG_REAL))   
   header_t header;
#endif   
   double val;
};

#if (!defined(TAG_REAL))   
#  define BREAL(p) BREF(p)
#  define CREAL(p) CREF(p)
#else
#  define BREAL(p) ((obj_t)((long)p + TAG_REAL))
#  define CREAL(p) ((obj_t)((long)p - TAG_REAL))
#endif

#define REAL(o) (CREAL(o)->real)
#define REAL_SIZE  (sizeof(struct bgl_real))

/*---------------------------------------------------------------------*/
/*    tagging                                                          */
/*---------------------------------------------------------------------*/
union bgl_fltobj {
   double _double;
   obj_t _obj;
};

/*---------------------------------------------------------------------*/
/*    BGL_REAL_TAG_MASK_TABLE                                          */
/*    -------------------------------------------------------------    */
/*    This value is used to test efficiently if the three least        */
/*    bits of a word correspond to any of the REAL_TAG. The table      */
/*    is such that:                                                    */
/*                                                                     */
/*      (char)((BGL_REAL_TAG_MASK_TABLE) << (obj & 7)) < 0             */
/*                                                                     */
/*    That is for any of TAG_REALL (3), TAG_REALZ (0), TAG_REALU (4)   */
/*    the value of BGL_REAL_TAG_MASK_TABLE must be such that           */
/*    shifted by the TAG value is set a 1 in the most significant      */
/*    bit and 0 otherwise. The value value that satisfies this is:     */
/*        10011000 = 152                                               */
/*    as                                                               */
/*        (10011000 << TAG_REALZ) == 10011000                          */
/*        (10011000 << TAG_REALU) == 10000000                          */
/*        (10011000 << TAG_REALL) == 11000000                          */
/*                                                                     */
/*    This gorgeous trick is due to Marc Feeley.                       */
/*---------------------------------------------------------------------*/
#if defined(TAG_REALZ)
#  define BGL_REAL_TAG_MASK_TABLE \
     ((1 << (7 - TAG_REALZ)) | (1 << (7 - TAG_REALU)) | (1 << (7 - TAG_REALL)))
#  define BGL_REAL_TAG_SET_BIT_TEST \
     ((1 << TAG_REALU) | (1 << TAG_REALL) | (1 << TAG_REALZ))
#else
#  define BGL_REAL_TAG_MASK_TABLE \
     ((1 << (7 - TAG_REALU)) | (1 << (7 - TAG_REALL)))
#  define BGL_REAL_TAG_SET_BIT_TEST \
     ((1 << TAG_REALU) | (1 << TAG_REALL))
#endif

// cast operations
#define BGL_ASOBJ(_d) (((union bgl_fltobj)(_d))._obj)
#define BGL_ASDOUBLE(_o) (((union bgl_fltobj)(_o))._double)

// BGL_TAGGED_REALP
/* #define BGL_TAGGED_REALP(_o) \                                      */
/*    ((char)((BGL_REAL_TAG_MASK_TABLE << ((char)((long)_o) & 7))) < 0) */
#define BGL_TAGGED_REALP(_o) \
   (((int32_t)((uint32_t)BGL_REAL_TAG_MASK_TABLE * 0x1010101) << (long)(_o)) < 0)

// BGL_BOXED_REALP
#if (defined(TAG_REAL))
#  define BGL_BOXED_REALP(o) BGL_TAGGED_PTRP(o, TAG_REAL, TAG_MASK)
#else
#  define BGL_BOXED_REALP(o) (POINTERP(o) && (TYPE(o) == REAL_TYPE))
#endif

// predicates
#if defined(TAG_REALZ) || !defined(TAG_REAL)
#  define FLONUMP(_o) (BGL_TAGGED_REALP(_o) || BGL_BOXED_REALP(_o))
#else
#  define BGL_FLONUMP_TAG_MASK_TABLE \
     ((1 << (7 - TAG_REAL)) | (1 << (7 - TAG_REALU)) | (1 << (7 - TAG_REALL)))
#  define FLONUMP(_o) (((int32_t)((uint32_t)BGL_FLONUMP_TAG_MASK_TABLE * 0x1010101) << (long)(_o)) < 0)
#endif

#define REALP(_o) FLONUMP(_o)

#define BGL_REAL_CNST(name) name

// constants
#if (!defined(TAG_REAL))   
#define DEFINE_REAL(name, aux, flonum) \
  static struct { __CNST_ALIGN header_t header; double real; } \
     aux = { __CNST_FILLER BGL_MAKE_HEADER(REAL_TYPE, 0), flonum }; \
  static const obj_t name = BREAL(&(aux.header))
#else
#define DEFINE_REAL(name, aux, flonum) \
  static struct { double real; } \
     aux = { flonum }; \
  static const obj_t name = BREAL(&aux)
#endif

#define BGL_REAL_SET(o, v) \
   ((BGL_TAGGED_REALP(o) ? DOUBLE_TO_REAL(v) : ((REAL(o).val = v), o)))

/*---------------------------------------------------------------------*/
/*    tagging and boxing                                               */
/*---------------------------------------------------------------------*/
#if (defined(TAG_FL_ROT_BITS))
// FL tags encoded in 3 bits of the exponent
#  define BGL_ROT_NBITS 64

#  define BGL_BIT_ROTL(_o) \
     ((obj_t)(((unsigned long)_o << TAG_FL_ROT_BITS) \
	      | ((unsigned long)_o >> (BGL_ROT_NBITS - TAG_FL_ROT_BITS))))
#  define BGL_BIT_ROTR(_o) \
     ((obj_t)(((unsigned long)_o >> TAG_FL_ROT_BITS) \
	      | ((unsigned long)_o << (BGL_ROT_NBITS - TAG_FL_ROT_BITS))))
#else
// FL tags encoded in the 3 least significant bits of the word
#  define BGL_BIT_ROTL(_o) (_o)
#  define BGL_BIT_ROTR(_o) (_o)
#endif

/*---------------------------------------------------------------------*/
/*    REAL_TO_DOUBLE                                                   */
/*---------------------------------------------------------------------*/
/* #define REAL_TO_DOUBLE(_o) \                                        */
/*    (BGL_TAGGED_REALP(_o) \                                          */
/*     ? (BGL_ASDOUBLE(BGL_BIT_ROTL(_o))) \                            */
/*     : REAL(_o).val)                                                 */
#define REAL_TO_DOUBLE(_o) \
   (!BGL_POINTERP(_o) \
    ? (BGL_ASDOUBLE(BGL_BIT_ROTL(_o))) \
    : REAL(_o).val)

/*---------------------------------------------------------------------*/
/*    x86_64 assembly inline                                           */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_ASM_X86_64 && (defined(TAG_FL_ROT_BITS))
#  define DOUBLE_TO_REAL _double_to_real

inline __attribute__((always_inline))
obj_t _double_to_real(double _d) {
   obj_t result;
   bool carry;
   __asm__("ror $60, %%rax;"
	   "bt  %%ax, %3;"
	   : "=@ccc"(carry), "=a"(result)
	   : "a"(((union bgl_fltobj)(_d))._obj),
	     "r"((uint16_t)(BGL_REAL_TAG_SET_BIT_TEST * 0x0101)));
   if (carry) {
      return result;
   } else {
      return make_real(_d);
   }
}
#else /* generic implementation */
#  define DOUBLE_TO_REAL(_d) \
   (BGL_TAGGED_REALP(BGL_BIT_ROTR(BGL_ASOBJ(_d))) \
      ? BGL_BIT_ROTR(BGL_ASOBJ(_d)) \
      : make_real(_d))
#endif

#define FLOAT_TO_REAL(_d) DOUBLE_TO_REAL((double)(_d))
#define REAL_TO_FLOAT(_o) ((float)(REAL_TO_DOUBLE(_o)))

#if (!defined(TAG_REAL))
#  define BGL_INIT_REAL(an_object, _d) \
     (an_object)->real.header = BGL_MAKE_HEADER(REAL_TYPE, REAL_SIZE); \
     (an_object)->real.val = _d;
#else
#  define BGL_INIT_REAL(an_object, _d) \
     (an_object)->real.val = _d;
#endif

#if (BGL_GC_CUSTOM || !defined(__GNUC__))
#  define MAKE_REAL(_d) make_real(_d)
#  define BGL_MAKE_INLINE_REAL(_d) MAKE_REAL(_d)
#else
#  define MAKE_REAL(_d) \
   ({ obj_t an_object = GC_MALLOC_ATOMIC(REAL_SIZE); \
      BGL_INIT_REAL(an_object, _d);		\
      BREAL(an_object); })

#  define BGL_MAKE_INLINE_REAL(_d) \
     an_object = GC_MALLOC_ATOMIC(REAL_SIZE); \
     BGL_INIT_REAL(an_object, _d); \
     BREAL(an_object)
#endif

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

