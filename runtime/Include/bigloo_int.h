/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/runtime/Include/bigloo_int.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar  2 05:40:03 2017                          */
/*    Last change :  Mon Jun 30 08:57:11 2025 (serrano)                */
/*    Copyright   :  2017-25 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo INTEGERs                                                  */
/*=====================================================================*/
#ifndef BIGLOO_INT_H 
#define BIGLOO_INT_H

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
/*    Integers                                                         */
/*---------------------------------------------------------------------*/
#define BINT_NULL BINT(0)

#if ((BGL_TAGGING != BGL_TAGGING_NAN) && (BGL_TAGGING != BGL_TAGGING_NUN) && !BGL_SMI)
/* normal tagging */
#  define INTEGERP(o) ((((long)o) & TAG_MASK) == TAG_INT)
#  define INTEGERSP(o, p) ((((((long)o) - TAG_INT) | (((long)p) - TAG_INT)) & TAG_MASK) == 0)

#  define BGL_LONG_MIN (LONG_MIN >> TAG_SHIFT)
#  define BGL_LONG_MAX (LONG_MAX >> TAG_SHIFT)

#  define BINT(i) (obj_t)TAG(i, TAG_SHIFT, TAG_INT)
#  define CINT(o) (long)UNTAG(o, TAG_SHIFT, TAG_INT)
#  define ADDFX(x, y) (obj_t)((long)(x) + ((long)(y)) - TAG_INT)
#  define SUBFX(x, y) (obj_t)((long)(x) - ((long)(y)) + TAG_INT)

#  define LTFX(x, y) ((long)(x) < (long)(y))
#  define LEFX(x, y) ((long)(x) <= (long)(y))
#  define GTFX(x, y) ((long)(x) > (long)(y))
#  define GEFX(x, y) ((long)(x) >= (long)(y))
#  define EGFX(x, y) ((long)(x) == (long)(y))
#elif (BGL_TAGGING == BGL_TAGGING_NUN)
/* nun tagging */
#  define INTEGERP(o) ((((unsigned long)o) >> 48) == (TAG_INT >> 48))
#  define INTEGERSP(o, p) (INTEGERP(o) && INTEGERP(p))

#  define BGL_LONG_MIN (INT32_MIN)
#  define BGL_LONG_MAX (INT32_MAX)

#  define BINT(i) ((obj_t)(((unsigned long)((uint32_t)(i))) + TAG_INT))
#  define CINT(i) ((long)(((int32_t)(((unsigned long)(i)) - TAG_INT))))
#  define ADDFX(x, y) BINT(CINT(x) + CINT(y))
#  define SUBFX(x, y) BINT(CINT(x) - CINT(y))

#  define LTFX(x, y) ((long)(x) < (long)(y))
#  define LEFX(x, y) ((long)(x) <= (long)(y))
#  define GTFX(x, y) ((long)(x) > (long)(y))
#  define GEFX(x, y) ((long)(x) >= (long)(y))
#  define EGFX(x, y) ((long)(x) == (long)(y))
#elif ((BGL_TAGGING != BGL_TAGGING_NAN) && BGL_SMI)
/* smi (int32) tagging */
#  define INTEGERP(o) ((((long)o) & TAG_MASK) == TAG_INT)
#  define INTEGERSP(o, p) (INTEGERP(o) && INTEGERP(p))

#  define BGL_LONG_MIN (INT32_MIN)
#  define BGL_LONG_MAX (INT32_MAX)

#  define BINT(i) ((obj_t)((((long)((int32_t)i)) << 32) + TAG_INT))
#  define CINT(i) ((long)((int32_t)((long)((long)i) >> 32)))
#  define ADDFX(x, y) (obj_t)((long)(x) + ((long)(y)) - TAG_INT)
#  define SUBFX(x, y) (obj_t)((long)(x) - ((long)(y)) + TAG_INT)

#  define LTFX(x, y) ((long)(x) < (long)(y))
#  define LEFX(x, y) ((long)(x) <= (long)(y))
#  define GTFX(x, y) ((long)(x) > (long)(y))
#  define GEFX(x, y) ((long)(x) >= (long)(y))
#  define EGFX(x, y) ((long)(x) == (long)(y))
#else
/* nan tagging */      
#  define INTEGERP(o) ((((long)o) & TAG_MASK) == TAG_INT)
#  define INTEGERSP(o, p) (INTEGERP(o) && INTEGERP(p))

#  define BGL_LONG_MIN (INT32_MIN)
#  define BGL_LONG_MAX (INT32_MAX)

#  define BINT(i) ((obj_t)(((long)((uint32_t)((int32_t)i)) | TAG_INT)))
#  define CINT(i) ((long)((int32_t)((long)i)))
#  define ADDFX(x, y) BINT(CINT(x) + CINT(y))
#  define SUBFX(x, y) BINT(CINT(x) - CINT(y))

#  define LTFX(x, y) (CINT(x) < CINT(y))
#  define LEFX(x, y) (CINT(x) <= CINT(y))
#  define GTFX(x, y) (CINT(x) > CINT(y))
#  define GEFX(x, y) (CINT(x) >= CINT(y))
#  define EGFX(x, y) (((long)(x)) == ((long)(y)))
#endif

#define ODDP_FX(i)  ((i) & 1)
#define EVENP_FX(i) (!ODDP_FX(i))

/*---------------------------------------------------------------------*/
/*    Long long                                                        */
/*---------------------------------------------------------------------*/
#define BGL_CREATE_LLONG(aux, num) \
   static struct { __CNST_ALIGN header_t header; BGL_LONGLONG_T llong; } \
      const aux = { __CNST_FILLER BGL_MAKE_HEADER(LLONG_TYPE, 0), (BGL_LONGLONG_T)(num) }

#if BGL_CNST_TWO_STEPS_INIT
#   define BGL_DECLARE_LLONG(name, cobj) \
       static obj_t name = 0L
#   define BGL_BIND_LLONG(name, cobj) \
       name = BREF(&(cobj.header))
#else   
#   define BGL_DECLARE_LLONG(name, cobj) \
       static obj_t name = BREF(&(cobj.header))
#   define BGL_BIND_LLONG(name, cobj)
#endif
   
#define BGL_DEFINE_LLONG(name, cobj, num) \
   BGL_CREATE_LLONG(cobj, num); \
   BGL_DECLARE_LLONG(name, cobj)
   
#define LLONG_SIZE (sizeof(struct llong))

#define LLONGP(o) (POINTERP(o) && (TYPE(o) == LLONG_TYPE))

#define LLONG(o) CREF(o)->llong
   
#define LONG_TO_LLONG(l) ((BGL_LONGLONG_T)l)   
#define LLONG_TO_LONG(o) ((long) o)
	    
#define BLLONG_TO_LLONG(l) (LLONG(l).val)
#define BLLONG_TO_LONG(l) ((long)BLLONG_TO_LLONG(l))
#define LONG_TO_BLLONG(l) (make_bllong((BGL_LONGLONG_T)l))
#define LLONG_TO_BLLONG(l) (make_bllong((BGL_LONGLONG_T)l))
   
/*---------------------------------------------------------------------*/
/*    Exact long                                                       */
/*---------------------------------------------------------------------*/
#define BGL_CREATE_ELONG(aux, num) \
   static struct { __CNST_ALIGN header_t header; long elong; } \
      const aux = { __CNST_FILLER BGL_MAKE_HEADER(ELONG_TYPE, 0), num }; \

#if BGL_CNST_TWO_STEPS_INIT
#   define BGL_DECLARE_ELONG(name, cobj) \
       static obj_t name = 0L
#   define BGL_BIND_ELONG(name, cobj) \
       name = BREF(&(cobj.header))
#else   
#   define BGL_DECLARE_ELONG(name, cobj) \
       static obj_t name = BREF(&(cobj.header))
#   define BGL_BIND_ELONG(name, cobj)
#endif
   
#define BGL_DEFINE_ELONG(name, cobj, num) \
   BGL_CREATE_ELONG(cobj, num); \
   BGL_DECLARE_ELONG(name, cobj)
		 
#define ELONG_SIZE (sizeof(struct elong))

#define ELONGP(o) (POINTERP(o) && (TYPE(o) == ELONG_TYPE))

#define ELONG(o) CREF(o)->elong

#define ELONG_TO_BELONG(_1) make_belong(_1)
#define BELONG_TO_LONG(l) (ELONG(l).val)

/*---------------------------------------------------------------------*/
/*    Stdint                                                           */
/*---------------------------------------------------------------------*/
#define BGL_INT8P(o) \
   BGL_CNSTP(o, BINT8H, BGL_CNST_SHIFT_INT16)
#define BGL_UINT8P(o) \
   BGL_CNSTP(o, BUINT8H, BGL_CNST_SHIFT_INT16)

#define BGL_INT16P(o) \
   BGL_CNSTP(o, BINT16H, BGL_CNST_SHIFT_INT16)
#define BGL_UINT16P(o) \
   BGL_CNSTP(o, BUINT16H, BGL_CNST_SHIFT_INT16)

#if (defined(BGL_CNST_SHIFT_INT32))   
#  define BGL_INT32P(o) \
     BGL_CNSTP(o, BINT32H, BGL_CNST_SHIFT_INT32)
#  define BGL_UINT32P(o) \
     BGL_CNSTP(o, BUINT32H, BGL_CNST_SHIFT_INT32)
#else
#  define BGL_INT32P(o) (POINTERP(o) && (TYPE(o) == INT32_TYPE))
#  define BGL_UINT32P(o) (POINTERP(o) && (TYPE(o) == UINT32_TYPE))
#endif
   
#define BGL_INT64P(o) (POINTERP(o) && (TYPE(o) == INT64_TYPE))
#define BGL_UINT64P(o) (POINTERP(o) && (TYPE(o) == UINT64_TYPE))

// MS 11mar2025, int8 conversions must be applied as uint8 not to
// propagate the number sign
#define BGL_INT8_TO_BINT8(i) \
   BGL_CNST_TO_BCNST(i, 0xffL, BINT8H, BGL_CNST_SHIFT_INT16, uint8_t)
#define BGL_UINT8_TO_BUINT8(i) \
   BGL_CNST_TO_BCNST(i, 0xffL, BUINT8H, BGL_CNST_SHIFT_INT16, uint8_t)

#define BGL_BINT8_TO_INT8(o) \
   BGL_BCNST_TO_CNST(o, 0xffL, BGL_CNST_SHIFT_INT16, int8_t)
#define BGL_BUINT8_TO_UINT8(o) \
   BGL_BCNST_TO_CNST(o, 0xffL, BGL_CNST_SHIFT_INT16, uint8_t)

// MS 11mar2025, int16 conversions must be applied as uint16 not to
// propagate the number sign
#define BGL_INT16_TO_BINT16(i) \
   BGL_CNST_TO_BCNST(i, 0xffffL, BINT16H, BGL_CNST_SHIFT_INT16, uint16_t)
#define BGL_UINT16_TO_BUINT16(i) \
   BGL_CNST_TO_BCNST(i, 0xffffL, BUINT16H, BGL_CNST_SHIFT_INT16, uint16_t)
	    
#define BGL_BINT16_TO_INT16(o) \
   BGL_BCNST_TO_CNST(o, 0xffffL, BGL_CNST_SHIFT_INT16, int16_t)
#define BGL_BUINT16_TO_UINT16(o) \
   BGL_BCNST_TO_CNST(o, 0xffffL, BGL_CNST_SHIFT_INT16, uint16_t)

#if (defined(BGL_CNST_SHIFT_INT32)) /* BGL_CNST_SHIFT_INT32 */
#  define BGL_DEFINE_INT32(name, aux, num) \
   const obj_t name = BGL_INT32_TO_BINT32(num)
#  define BGL_DEFINE_UINT32(name, aux, num) \
   const obj_t name = BGL_UINT32_TO_BUINT32(num)
   
#  define BGL_BIND_INT32(name, cobj)
#  define BGL_BIND_UINT32(name, cobj)
   
#  define BGL_INT32_TO_BINT32(i) \
   BGL_CNST_TO_BCNST(i, 0xffffffffL, BINT32H, BGL_CNST_SHIFT_INT32, int32_t)
#  define BGL_UINT32_TO_BUINT32(i) \
   BGL_CNST_TO_BCNST(i, 0xffffffffL, BUINT32H, BGL_CNST_SHIFT_INT32, uint32_t)
#  define BGL_BINT32_TO_INT32(o) \
   BGL_BCNST_TO_CNST(o, 0xffffffffL, BGL_CNST_SHIFT_INT32, int32_t)
#  define BGL_BUINT32_TO_UINT32(o) \
   BGL_BCNST_TO_CNST(o, 0xffffffffL, BGL_CNST_SHIFT_INT32, uint32_t)
#else /* !BGL_CNST_SHIFT_INT32 */
#  define GL_CREATE_INT32(aux, num) \
   static struct { __CNST_ALIGN header_t header; int32_t val; } \
      const aux = { __CNST_FILLER BGL_MAKE_HEADER(INT32_TYPE, 0), (int32_t)(num) }
   
#  define BGL_CREATE_UINT32(aux, num) \
   static struct { __CNST_ALIGN header_t header; uint32_t val; } \
      const aux = { __CNST_FILLER BGL_MAKE_HEADER(UINT32_TYPE, 0), (uint32_t)(num) }
   
#  if BGL_CNST_TWO_STEPS_INIT
#     define BGL_DECLARE_INT32(name, cobj) \
         static obj_t name = 0L
#     define BGL_DECLARE_UINT32(name, cobj) \
         static obj_t name = 0L
#     define BGL_BIND_INT32(name, cobj) \
         name = BREF(&(cobj.header)
#     define BGL_BIND_UINT32(name, cobj) \
         name = BREF(&(cobj.header)
#else   
#     define BGL_DECLARE_INT32(name, cobj) \
         static obj_t name = BREF(&(cobj.header))
#     define BGL_DECLARE_UINT32(name, cobj) \
         static obj_t name = BREF(&(cobj.header))
#     define BGL_BIND_INT32(name, cobj)
#     define BGL_BIND_UINT32(name, cobj)
#endif
   
#  define BGL_DEFINE_INT32(name, cobj, num) \
     BGL_CREATE_INT32(cobj, num); \
     BGL_DECLARE_INT32(name, cobj)
		 
#  define BGL_DEFINE_UINT32(name, cobj, num) \
     BGL_CREATE_UINT32(cobj, num); \
     BGL_DECLARE_UINT32(name, cobj)

#  define BGL_INT32_SIZE (sizeof(struct bgl_sint32))
#  define BGL_UINT32_SIZE (sizeof(struct bgl_uint32))
#  define BGL_INT32(o) CREF(o)->sint32
#  define BGL_UINT32(o) CREF(o)->uint32
#  define BGL_INT32_TO_BINT32(_1) bgl_make_bint32(_1)
#  define BGL_UINT32_TO_BUINT32(_1) bgl_make_buint32(_1)
#  define BGL_BINT32_TO_INT32(o) BGL_INT32(o).val
#  define BGL_BUINT32_TO_UINT32(o) BGL_UINT32(o).val
#endif /* BGL_CNST_SHIFT_INT32 */

#define BGL_CREATE_INT64(aux, num) \
   static struct { __CNST_ALIGN header_t header; int64_t val; } \
      const aux = { __CNST_FILLER BGL_MAKE_HEADER(INT64_TYPE, 0), (int64_t)(num) }
#define BGL_CREATE_UINT64(aux, num) \
   static struct { __CNST_ALIGN header_t header; int64_t val; } \
      const aux = { __CNST_FILLER BGL_MAKE_HEADER(INT64_TYPE, 0), (int64_t)(num) }

#if BGL_CNST_TWO_STEPS_INIT
#   define BGL_DECLARE_INT64(name, cobj) \
       static obj_t name = 0L
#   define BGL_DECLARE_UINT64(name, cobj) \
       static obj_t name = 0L
#   define BGL_BIND_INT64(name, cobj) \
       name = BREF(&(cobj.header))
#   define BGL_BIND_UINT64(name, cobj) \
       name = BREF(&(cobj.header))
#else   
#   define BGL_DECLARE_INT64(name, cobj) \
       static obj_t name = BREF(&(cobj.header))
#   define BGL_DECLARE_UINT64(name, cobj) \
       static obj_t name = BREF(&(cobj.header))
#   define BGL_BIND_INT64(name, cobj)
#   define BGL_BIND_UINT64(name, cobj)
#endif
		 
#define BGL_DEFINE_INT64(name, cobj, num) \
   BGL_CREATE_INT64(cobj, num); \
   BGL_DECLARE_INT64(name, cobj)
		 
#define BGL_DEFINE_UINT64(name, cobj, num) \
   BGL_CREATE_UINT64(cobj, num); \
   BGL_DECLARE_UINT64(name, cobj)
		 
#define BGL_INT64_SIZE (sizeof(struct bgl_sint64))
#define BGL_UINT64_SIZE (sizeof(struct bgl_uint64))
#define BGL_INT64(o) CREF(o)->sint64
#define BGL_UINT64(o) CREF(o)->uint64
#define BGL_INT64_TO_BINT64(_1) bgl_make_bint64(_1)
#define BGL_UINT64_TO_BUINT64(_1) bgl_make_buint64(_1)
#define BGL_BINT64_TO_INT64(o) BGL_INT64(o).val
#define BGL_BUINT64_TO_UINT64(o) BGL_UINT64(o).val

#define DEFINE_INT32(a,b,c) BGL_DEFINE_INT32(a,b,c)
#define DEFINE_UINT32(a,b,c) BGL_DEFINE_UINT32(a,b,c)
#define DEFINE_INT64(a,b,c) BGL_DEFINE_INT64(a,b,c)
#define DEFINE_UINT64(a,b,c) BGL_DEFINE_UINT64(a,b,c)
   

/*---------------------------------------------------------------------*/
/*    overflow                                                         */
/*    -------------------------------------------------------------    */
/*    These macros are only used when:                                 */
/*      1- Bigloo has been configured with GCC                         */
/*      2- Used to compile with another C compiler,                    */
/*      3- and the C overflow functions are explicitly used by the app */
/*    -------------------------------------------------------------    */
/*    See Hacker's Delight, second edition, page 29.                   */
/*---------------------------------------------------------------------*/
#if (!BGL_HAVE_OVERFLOW)
static int __builtin_saddl_overflow(long x, long y, long *res) {
#if (BGL_ELONG_BIT_SIZE == 32)
   long z = (~((long)x ^ (long)y)) & 0x80000000;
#else   
   long z = (~((long)x ^ (long)y)) & 0x8000000000000000;;
#endif
   if (z & (~((((long)x ^ (long)z) + ((long)y)) ^ ((long) y)))) {
      return 1;
   } else {
      *res = (x + y);
      return 0;
   }
}

static int __builtin_ssubl_overflow(long x, long y, long *res) {
#if (BGL_ELONG_BIT_SIZE == 32)
   long z = ((long)x ^ (long)y) & 0x80000000;
#else
   long z = ((long)x ^ (long)y) & 0x8000000000000000;
#endif   
   if (z & ((((long)x ^ (long)z) - ((long)y)) ^ ((long) y))) {
      return 1;
   } else {
      *res = (x - y);
      return 0;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    Overflows                                                        */
/*    -------------------------------------------------------------    */
/*    This macros have to be used with extreme care and should         */
/*    probably never be used directly in user code. They should        */
/*    be used in patterns such as:                                     */
/*                                                                     */
/*       ($let ((res::bint 0))                                         */
/*          (if ($+fx/ov x y res)                                      */
/*              WHATEVER-WITH-THE-OVERFLOW                             */
/*              res)))                                                 */
/*                                                                     */
/*    This uses the special "$let" construct that introduces           */
/*    unremovable variables. This enables $+fx/ov to be implemented    */
/*    as one of the following C macros that takes the address of the   */
/*    local variable.                                                  */
/*---------------------------------------------------------------------*/
#if !BGL_HAVE_OVERFLOW
extern bool_t __builtinsadd_overflow(int x, int y, int *res);
extern bool_t __builtinsaddl_overflow(long x, long y, long *res);
extern bool_t __builtinsub_overflow(int x, int y, int *res);
extern bool_t __builtinsubl_overflow(long x, long y, long *res);
extern bool_t __builtinmul_overflow(int x, int y, int *res);
extern bool_t __builtinmull_overflow(long x, long y, long *res);
#endif

#if (BGL_TAGGING != BGL_TAGGING_NAN) && (BGL_TAGGING != BGL_TAGGING_NUN) && TAG_INT == 0
#  define BGL_ADDFX_OV(x, y, res) __builtin_saddl_overflow((long)x, (long)y, (long*)&res)
#  define BGL_SUBFX_OV(x, y, res) __builtin_ssubl_overflow((long)x, (long)y, (long*)&res)
#  define BGL_MULFX_OV(x, y, res) __builtin_smull_overflow((long)x, CINT(y), (long*)&res)
#endif

#if (BGL_TAGGING != BGL_TAGGING_NAN) && (BGL_TAGGING != BGL_TAGGING_NUN) && TAG_INT != 0
#  define BGL_BINT_SANS_TAG(x) ((long)(x) & ~TAG_MASK)
#  define BGL_BINT_WITH_TAG(x) ((obj_t)((long)(x) | TAG_INT))
#  define BGL_ADDFX_OV(x, y, res) (__builtin_saddl_overflow(BGL_BINT_SANS_TAG(x), BGL_BINT_SANS_TAG(y), (long*)&res) || (res = BGL_BINT_WITH_TAG((long)res), 0))
#  define BGL_SUBFX_OV(x, y, res) (__builtin_ssubl_overflow(BGL_BINT_SANS_TAG(x), BGL_BINT_SANS_TAG(y), (long*)&res) || (res = BGL_BINT_WITH_TAG((long)res), 0))
#  define BGL_MULFX_OV(x, y, res) (__builtin_smull_overflow(BGL_BINT_SANS_TAG(x), CINT(y), (long*)&res) || (res = BGL_BINT_WITH_TAG((long)res), 0))
#endif

#if BGL_NAN_TAGGING || BGL_NUN_TAGGING
#  define BGL_ADDFX_OV(x, y, res) (__builtin_sadd_overflow(CINT(x), CINT(y), (int *)&res) || (res = BINT((long)res), 0))
#  define BGL_SUBFX_OV(x, y, res) (__builtin_ssub_overflow(CINT(x), CINT(y), (int *)&res) || (res = BINT((long)res), 0))
#  define BGL_MULFX_OV(x, y, res) (__builtin_smul_overflow(CINT(x), CINT(y), (int *)&res) || (res = BINT((long)res), 0))
#endif

#if TAG_INT == 0
#   define BGL_ADDFX_SANS_OV(x, y) ((obj_t)(((long)(x)) + ((long)(y))))
#   define BGL_SUBFX_SANS_OV(x, y) ((obj_t)(((long)(x)) - ((long)(y))))
#   define BGL_MULFX_SANS_OV(x, y) ((obj_t)(((long)(x)) * CINT(y)))
#else
#   define BGL_ADDFX_SANS_OV(x, y) (BINT(CINT(x) + CINT(y)))
#   define BGL_SUBFX_SANS_OV(x, y) (BINT(CINT(x) - CINT(y)))
#   define BGL_MULFX_SANS_OV(x, y) (BINT(CINT(x) * CINT(y)))
#endif

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

