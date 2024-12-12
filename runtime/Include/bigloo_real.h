/*=====================================================================*/
/*    .../prgm/project/bigloo/flt/runtime/Include/bigloo_real.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Mar  6 07:07:32 2016                          */
/*    Last change :  Thu Dec 12 10:44:41 2024 (serrano)                */
/*    Copyright   :  2016-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo REALs                                                     */
/*=====================================================================*/
#ifndef BIGLOO_REAL_H 
#define BIGLOO_REAL_H

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
/*    Specific configuration                                           */
/*---------------------------------------------------------------------*/
#if (BGL_TAGGING == BGL_TAGGING_HEAP)
#  include <bigloo_real_heap.h>
#elif (BGL_TAGGING == BGL_TAGGING_NAN)
#  include <bigloo_real_nan.h>
#else
#  include <bigloo_real_flt.h>
#endif

/*---------------------------------------------------------------------*/
/*    api                                                              */
/*---------------------------------------------------------------------*/
#define __DOUBLE_TO_LLONG_BITS(dd) \
  double ddd = dd; \
  BGL_LONGLONG_T result; \
  memcpy(&result, &ddd, sizeof(result)); \
  result;

#define __LLONG_BITS_TO_DOUBLE(ll) \
  BGL_LONGLONG_T lll = ll; \
  double result; \
  memcpy(&result, &lll, sizeof(result)); \
  result;

/* If ints are bigger than floats (which can happen on 64bit machines) */
/* then * we only want to set the least significant bytes of the int.  */
#define __FLOAT_TO_INT_BITS(ff) \
  float fff = ff; \
  int result = 0; \
  int offset = sizeof(int) - sizeof(float); \
  memcpy(((char*)&result) + offset, &fff, sizeof(float)); \
  result;

/* If ints are bigger than floats (which can happen on 64bit machines) */
/* then we only want to set the least significant bytes of the int.    */
#define __INT_BITS_TO_FLOAT(ii) \
  int iii = ii; \
  float result; \
  int offset = sizeof(int) - sizeof(float); \
  memcpy(&result, ((char*)&iii) + offset, sizeof(float)); \
  result;

#if (defined(__GNUC__))
#  define DOUBLE_TO_LLONG_BITS(dd) ({ __DOUBLE_TO_LLONG_BITS(dd) })
#  define LLONG_BITS_TO_DOUBLE(ll) ({ __LLONG_BITS_TO_DOUBLE(ll) })
#  define FLOAT_TO_INT_BITS(ff) ({ __FLOAT_TO_INT_BITS(ff) })
#  define INT_BITS_TO_FLOAT(ii) ({ __INT_BITS_TO_FLOAT(ii) })
#else
BGL_RUNTIME_DECL BGL_LONGLONG_T DOUBLE_TO_LLONG_BITS(double);
BGL_RUNTIME_DECL double LLONG_BITS_TO_DOUBLE(BGL_LONGLONG_T);
BGL_RUNTIME_DECL int FLOAT_TO_INT_BITS(float);
BGL_RUNTIME_DECL float INT_BITS_TO_FLOAT(int);
#endif

#define NEG(x) (- (x))

#define RANDOMFL() ((double)rand()/RAND_MAX)
   
#if BGL_ISOC99 || defined(__USE_ISOC99)
#  define BGL_SIGNBIT(a) signbit(a) 
#  define BGL_NAN ((double) NAN)
#  define BGL_INFINITY ((double) INFINITY)
#  define BGL_IS_FINITE isfinite
#  define BGL_IS_INF isinf
#  define BGL_IS_NAN isnan
#  define BGL_FL_MAX2(a, b) fmax(a, b)
#  define BGL_FL_MIN2(a, b) fmin(a, b)
#  define BGL_FL_ROUND(a) round(a)
#else
#  if BGL_CCDIV0
#    define BGL_NAN (0.0 / 0.0)
#    define BGL_INFINITY (1.0 / 0.0)
#  else
BGL_RUNTIME_DECL double bgl_nan();
BGL_RUNTIME_DECL double bgl_infinity();
#    define BGL_NAN bgl_nan()
#    define BGL_INFINITY bgl_infinity()
#  endif   
#  define BGL_SIGNBIT(r) ((r<0.0)?1:(((1.0/r)<0.0)?1:0))
#  define BGL_IS_FINITE(r) (!(BGL_IS_INF(r) || BGL_IS_NAN(r)))
#  define BGL_IS_INF(r) (r==BGL_INFINITY || r==-BGL_INFINITY)
#  define BGL_IS_NAN(r) ((r) != (r))
/* min and max have to look at -0.0 and +0.0, too */
#  define BGL_FL_MAX2(a, b) (a<b?b:(a==0.0&&b==0.0&&1.0/a<0.0)?b:a)
#  define BGL_FL_MIN2(a, b) (a>b?b:(a==0.0&&b==0.0&&1.0/a>0.0)?b:a)
#  define BGL_FL_ROUND(a) (BGL_SIGNBIT(a)?-(floor(-a+0.5)):floor(a+0.5))
#endif

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

