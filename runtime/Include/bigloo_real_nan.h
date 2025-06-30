/*=====================================================================*/
/*    .../project/bigloo/bigloo/runtime/Include/bigloo_real_nan.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Mar  6 07:07:32 2016                          */
/*    Last change :  Mon Jun 30 09:19:21 2025 (serrano)                */
/*    Copyright   :  2016-25 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo NaN TAGGING REALs                                         */
/*=====================================================================*/
#ifndef BIGLOO_REAL_NAN_H 
#define BIGLOO_REAL_NAN_H

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
BGL_RUNTIME_DECL union bgl_nanobj bigloo_nan, bigloo_infinity, bigloo_minfinity;

/*---------------------------------------------------------------------*/
/*    tagging                                                          */
/*---------------------------------------------------------------------*/
union bgl_nanobj {
   double real;
   obj_t ptr;
};
      
#define BREAL(_n) (((union bgl_nanobj)(_n)).ptr)
#define CREAL(_p) (((union bgl_nanobj)(_p)).real)

#define BGL_REAL_CNST(name) name.ptr

#define BGL_DEFINE_REAL(name, aux, _flonum) \
   static const union bgl_nanobj name = { _flonum }
#define BGL_BIND_REAL(name, aux)

#define DEFINE_REAL(name, aux, _flonum) \
   BGL_DEFINE_REAL(name, aux, _flonum)

#define FLONUMP(c) (((unsigned long)c >> 48 & 0x7ff8) != 0x7ff8)
#define NANP(c) (((unsigned long)c == TAG_QNAN) || ((unsigned long)c == TAG_SNAN))
#define REALP(c) (FLONUMP(c) || NANP(c))
#define BGL_REALSP(x, y) REALP(c) && REALP(d)
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

