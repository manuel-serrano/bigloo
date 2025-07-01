/*=====================================================================*/
/*    .../project/bigloo/bigloo/runtime/Include/bigloo_real_flt.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Mar  6 07:07:32 2016                          */
/*    Last change :  Tue Jul  1 11:02:42 2025 (serrano)                */
/*    Copyright   :  2016-25 Manuel Serrano                            */
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
/*    inline annotation                                                */
/*---------------------------------------------------------------------*/
#if HAVE_INLINE
#  define INLINE inline __attribute__((always_inline))
#else
#  define INLINE static
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
/*    cast                                                             */
/*---------------------------------------------------------------------*/
union bgl_fltobj {
   double _double;
   obj_t _obj;
   unsigned long _long;
};

#define BGL_ASOBJ(d) (((union bgl_fltobj)(d))._obj)
#define BGL_ASDOUBLE(o) (((union bgl_fltobj)(o))._double)
#define BGL_ASLONG(o) (((union bgl_fltobj)(o))._long)

/*---------------------------------------------------------------------*/
/*    basic tagging and boxing                                         */
/*---------------------------------------------------------------------*/
#if (defined(TAG_FL_ROT_BITS))
// ---------------------------
// FL tags encoded in 3 bits of the exponent
#  define BGL_ROT_NBITS 64

#  define BGL_BIT_ROTL(o, n) \
     ((BGL_ASLONG(o) << n) | (BGL_ASLONG(o) >> (BGL_ROT_NBITS - n)))
#  define BGL_BIT_ROTR(o, n) \
     ((BGL_ASLONG(o) >> n) | (BGL_ASLONG(o) << (BGL_ROT_NBITS - n)))
#else
// --
// FL tags encoded in the 3 least significant bits of the word
#  define BGL_BIT_ROTL(o, n) BGL_ASLONG(o)
#  define BGL_BIT_ROTR(o, n) BGL_ASLONG(o)
#endif
// ---

/*---------------------------------------------------------------------*/
/*    BGL_REAL_TAG_MASK_TABLE ...                                      */
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
// -------------------
#  define BGL_REAL_TAG_MASK_TABLE \
     ((1 << (7 - TAG_REALZ)) | (1 << (7 - TAG_REALU)) | (1 << (7 - TAG_REALL)))
#  define BGL_REAL_TAG_SET \
     ((1 << TAG_REALU) | (1 << TAG_REALL) | (1 << TAG_REALZ))
#else
// --
#  define BGL_REAL_TAG_MASK_TABLE \
     ((1 << (7 - TAG_REALU)) | (1 << (7 - TAG_REALL)))
#  define BGL_REAL_TAG_SET \
     ((1 << TAG_REALU) | (1 << TAG_REALL))
#endif
// ---

/*---------------------------------------------------------------------*/
/*    BGL_TAGGED_REALP ...                                             */
/*---------------------------------------------------------------------*/
#if BGL_TAGGING == BGL_TAGGING_FL || BGL_TAGGING == BGL_TAGGING_FLNZ
// -----------------------------------------------------------------
#  if !BGL_HAVE_ASM_X86_64
#    define BGL_TAGGED_REALP(o) BGL_TAG_IN_SETP(o, BGL_REAL_TAG_SET)
#    define BGL_TAG_IN_SETP(t, set) \
       ((((uint32_t)1 << ((long)(t))) & (~(uint32_t)0/0xff * (set))) != 0)
/* #define BGL_TAGGED_REALP(o) \                                      */
/*    ((char)((BGL_REAL_TAG_MASK_TABLE << ((char)((long)o) & 7))) < 0) */
/* #  define BGL_TAGGED_REALP(o) \                                     */
/*      (((int32_t)((uint32_t)BGL_REAL_TAG_MASK_TABLE * 0x1010101) << (long)(o)) < 0) */
#  else
INLINE bool BGL_TAGGED_REALP(obj_t _o) {
   bool carry;
   __asm__("bt %%ax, %2;" /* get one bit of mask into carry (ax is index) */
	   : "=@ccc"(carry)
	   : "a"((uint16_t)((int64_t)_o)),
	     "r"((uint16_t)((uint16_t)(~(uint16_t)0)/0xff * BGL_REAL_TAG_SET)));
   return carry;
}
#  endif
#elif BGL_TAGGING == BGL_TAGGING_FLLB
// ----------------------------------
#  define BGL_TAGGED_REALP(o) \
     ((char)((BGL_REAL_TAG_MASK_TABLE << ((char)((long)o) & TAG_MASK))) < 0)
#  define BGL_TAGGED_REALSP(o, p) \
   (BGL_TAGGED_REALP(o) && BGL_TAGGED_REALP(p))
#elif BGL_TAGGING == BGL_TAGGING_FL1
// ---------------------------------
// ((o>>59)+13)>>1
#  define BGL_TAGGED_REALP(o) \
     (((long)o & TAG_MASK) == TAG_REALZ)
#  define BGL_TAGGED_REALSP(o, p) \
     (((((long)o - TAG_REALZ) | ((long)p - TAG_REALZ)) & TAG_MASK) == 0)
#else
// --
#  define BGL_TAGGED_REALP(o) \
     ((char)((BGL_REAL_TAG_MASK_TABLE << ((char)((long)o) & 7))) < 0)
#  define BGL_TAGGED_REALSP(o, p) \
   (BGL_TAGGED_REALP(o) && BGL_TAGGED_REALP(p))
#endif
// ---

/*---------------------------------------------------------------------*/
/*    BGL_BOXED_REALP ...                                              */
/*---------------------------------------------------------------------*/
#if (defined(TAG_REAL))
#  define BGL_BOXED_REALP(o) BGL_TAGGED_PTRP(o, TAG_REAL, TAG_MASK)
#else
#  define BGL_BOXED_REALP(o) BGL_HEADER_PTRP(o, REAL_TYPE)
#endif

// predicates
#if defined(TAG_REALZ) || !defined(TAG_REAL) || !defined(TAG_FL_ROT_BITS)
// ----------------------------------------------------------------------
#  define FLONUMP(o) (BGL_TAGGED_REALP(o) || BGL_BOXED_REALP(o))
#  define BGL_FAST_REALP(o) BGL_TAGGED_REALP(o)
#  define BGL_FAST_REALSP(o, p) BGL_TAGGED_REALSP(o, p)
#else
// --
#  define BGL_FLONUMP_TAG_MASK_TABLE \
     ((1 << (7 - TAG_REAL)) | (1 << (7 - TAG_REALU)) | (1 << (7 - TAG_REALL)))
#  define BGL_FAST_FLONUMP_TAG_MASK_TABLE \
     ((1 << (7 - TAG_REALU)) | (1 << (7 - TAG_REALL)))
#  define FLONUMP(o) (((int32_t)((uint32_t)BGL_FLONUMP_TAG_MASK_TABLE * 0x1010101) << (long)(o)) < 0)
#  define BGL_FAST_REALVAL(o) ((int32_t)((uint32_t)BGL_FAST_FLONUMP_TAG_MASK_TABLE * 0x1010101) << (long)(o))
#  define BGL_FAST_REALP(o) (BGL_FAST_REALVAL(o) < 0)
#  define BGL_FAST_REALSP(o) ((BGL_FAST_REALVAL(o) | BGL_FAST_REALVAL(p)) < 0)
#endif
// ---

#define REALP(o) FLONUMP(o)
#define REALSP(o, p) (FLONUMP(o) && FLONUMP(p))

#define BGL_REAL_CNST(name) name

/*---------------------------------------------------------------------*/
/*    BGL_FAST_REAL_TO_DOUBLE ...                                      */
/*---------------------------------------------------------------------*/
#if (BGL_TAGGING != BGL_TAGGING_FL1)
// ---------------------------------
#  define BGL_FAST_REAL_TO_DOUBLE(o) \
     BGL_ASDOUBLE(BGL_BIT_ROTL(o, TAG_FL_ROT_BITS))
#else
// --
INLINE double BGL_FAST_REAL_TO_DOUBLE(obj_t o) {
   // ((o<<1)-13)<<58
   unsigned long t = BGL_BIT_ROTL(o, 1) - TAG_FL_EXPONENT_SHIFT;
   return BGL_ASDOUBLE(BGL_BIT_ROTL(t, TAG_FL_ROT_BITS));
}
#endif
// ---

/*---------------------------------------------------------------------*/
/*    REAL_TO_DOUBLE ...                                               */
/*---------------------------------------------------------------------*/
#if defined(TAG_REAL)
#  define BGL_POINTER_REALP(o) BGL_BOXED_REALP(o)
#else
#  define BGL_POINTER_REALP(o) BGL_POINTERP(o)
#endif

INLINE double REAL_TO_DOUBLE(obj_t o) {
   // fprintf(stderr, "REAL_TO_DOUBLE...\n");
   double d = BGL_FAST_REAL_TO_DOUBLE(o);
   if (!BGL_POINTER_REALP(o)) {
      // fprintf(stderr, "d.1=%g\n", d);
      return d;
   } else {
      // fprintf(stderr, "d.2=%g\n", ((struct bgl_real *)CREAL(o))->val);
      return ((struct bgl_real *)CREAL(o))->val;
   }
}

#define REAL_TO_FLOAT(o) ((float)(REAL_TO_DOUBLE(o)))

/*---------------------------------------------------------------------*/
/*    BGL_DOUBLE_TO_OBJ ...                                            */
/*---------------------------------------------------------------------*/
#if BGL_TAGGING == BGL_TAGGING_FL || BGL_TAGGING == BGL_TAGGING_FLNZ
#  define BGL_DOUBLE_TO_OBJ(d) \
     BGL_ASOBJ(BGL_BIT_ROTR(d, TAG_FL_ROT_BITS))
#elif BGL_TAGGING == BGL_TAGGING_FLLB
// ----------------------------------
#  define BGL_DOUBLE_TO_OBJ(d) \
     BGL_ASOBJ(d)
#elif BGL_TAGGING == BGL_TAGGING_FL1
// ---------------------------------
// ((o>>59)+13)>>1
INLINE obj_t BGL_DOUBLE_TO_OBJ(double d) {
   unsigned long t = BGL_BIT_ROTR(d, TAG_FL_ROT_BITS) + TAG_FL_EXPONENT_SHIFT;
   return BGL_ASOBJ(BGL_BIT_ROTR(t, 1));
}
#else
// --
#  define BGL_DOUBLE_TO_OBJ(d) \
     BGL_ASOBJ(BGL_BIT_ROTR(d, TAG_FL_ROT_BITS))
#endif
// ---

/*---------------------------------------------------------------------*/
/*    DOUBLE_TO_REAL ...                                               */
/*---------------------------------------------------------------------*/
INLINE obj_t DOUBLE_TO_REAL(double d) {
   obj_t o = BGL_DOUBLE_TO_OBJ(d);
   if (BGL_TAGGED_REALP(o)) {
      return o;
   } else {
      return make_real(d);
   }
}

#define FLOAT_TO_REAL(d) DOUBLE_TO_REAL((double)(d))

/*---------------------------------------------------------------------*/
/*    DEFINE_REAL ...                                                  */
/*---------------------------------------------------------------------*/
#define BGL_CREATE_REAL(aux, flonum) \
  static const double aux = flonum

#define BGL_DECLARE_REAL(n, aux) static obj_t n = 0L
#define BGL_BIND_REAL(n, aux) n = DOUBLE_TO_REAL(aux)

#define BGL_DEFINE_REAL(name, aux, flonum) \
   BGL_CREATE_REAL(aux, flonum); \
   BGL_DECLARE_REAL(name, aux)

/*---------------------------------------------------------------------*/
/*    BGL_REAL_SET ...                                                 */
/*---------------------------------------------------------------------*/
#define BGL_REAL_SET(o, v) \
   ((BGL_TAGGED_REALP(o) ? DOUBLE_TO_REAL(v) : ((REAL(o).val = v), o)))

#if (!defined(TAG_REAL))
// ---------------------
#  define BGL_INIT_REAL(an_object, d) \
     (an_object)->real.header = BGL_MAKE_HEADER(REAL_TYPE, REAL_SIZE); \
     (an_object)->real.val = d;
#else
// --
#  define BGL_INIT_REAL(an_object, d) \
     (an_object)->real.val = d;
#endif
// ---

#if (BGL_GC_CUSTOM || !defined(__GNUC__))
// --------------------------------------
#  define MAKE_REAL(d) make_real(d)
#  define BGL_MAKE_INLINE_REAL(d) MAKE_REAL(d)
#else
// --
#  define MAKE_REAL(d) \
   ({ obj_t an_object = GC_MALLOC_ATOMIC(REAL_SIZE); \
      BGL_INIT_REAL(an_object, d);		\
      BREAL(an_object); })

#  define BGL_MAKE_INLINE_REAL(d) \
     an_object = GC_MALLOC_ATOMIC(REAL_SIZE); \
     BGL_INIT_REAL(an_object, d); \
     BREAL(an_object)
#endif
// ---

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

