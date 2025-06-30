/*=====================================================================*/
/*    .../project/bigloo/bigloo/runtime/Include/bigloo_real_nun.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Mar  6 07:07:32 2016                          */
/*    Last change :  Mon Jun 30 09:19:47 2025 (serrano)                */
/*    Copyright   :  2016-25 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo NuN TAGGING REALs                                         */
/*=====================================================================*/
#ifndef BIGLOO_REAL_NUN_H 
#define BIGLOO_REAL_NUN_H

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
BGL_RUNTIME_DECL union bgl_nunobj bigloo_nan, bigloo_infinity, bigloo_minfinity;

/*---------------------------------------------------------------------*/
/*    tagging                                                          */
/*---------------------------------------------------------------------*/
union bgl_nunobj {
   double real;
   unsigned long bits;
   obj_t obj;
};

#define BGL_NUN_FL_OFFSET (1ULL << 49)

#define BGL_DOUBLE_AS_BITS(d) (((union bgl_nunobj)(d)).bits)
#define BGL_OBJ_AS_BITS(d) (((union bgl_nunobj)(d)).bits)
#define BGL_BITS_AS_DOUBLE(b) (((union bgl_nunobj)(b)).real)
#define BGL_BITS_AS_OBJ(b) (((union bgl_nunobj)(b)).obj)

#define BREAL(d) BGL_BITS_AS_OBJ(BGL_DOUBLE_AS_BITS(d) + BGL_NUN_FL_OFFSET)
#define CREAL(p) BGL_BITS_AS_DOUBLE(BGL_OBJ_AS_BITS(p) - BGL_NUN_FL_OFFSET)

#define BGL_DEFINE_REAL(name, aux, _flonum) \
   static const union bgl_nunobj name = { _flonum }
#define BGL_BIND_REAL(name, aux)

#define DEFINE_REAL(name, aux, _flonum) \
   BGL_DEFINE_REAL(name, aux, _flonum)

#define BGL_REAL_CNST(name) \
   ((obj_t)(name.bits + BGL_NUN_FL_OFFSET))

/*---------------------------------------------------------------------*/
/*    FLONUMP ...                                                      */
/*---------------------------------------------------------------------*/
INLINE bool FLONUMP(obj_t o) {
   unsigned long tag = ((unsigned long)o) >> 48;
   return tag && tag != 0xffff;
}

#define REALP(c) (FLONUMP(c))
#define BGL_REALSP(c, d) (FLONUMP(c) && FLONUMP(d))

#define BGL_FAST_REALP(c) REALP(c)
#define BGL_FAST_REALSP(c, d) (REALP(c) && REALP(d))
#define BGL_REAL_SET(o, v) BREAL(v)

/*---------------------------------------------------------------------*/
/*    alloc                                                            */
/*---------------------------------------------------------------------*/
#define make_real(d) BREAL(d)
#define REAL_TO_DOUBLE(r) CREAL(r)
#define BGL_FAST_REAL_TO_DOUBLE(r) REAL_TO_DOUBLE(r)
#define DOUBLE_TO_REAL(r) make_real(r)
#define FLOAT_TO_REAL(d) (DOUBLE_TO_REAL((double)(d)))
#define REAL_TO_FLOAT(r) ((float)(REAL_TO_DOUBLE(r)))

#define MAKE_REAL(v) make_real(v)
#define BGL_MAKE_INLINE_REAL(v) MAKE_REAL(v)

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

