/*=====================================================================*/
/*    .../project/bigloo/flt/runtime/Include/bigloo_real_heap.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Mar  6 07:07:32 2016                          */
/*    Last change :  Tue Dec 10 10:49:50 2024 (serrano)                */
/*    Copyright   :  2016-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo REALs                                                     */
/*=====================================================================*/
#ifndef BIGLOO_REAL_HEAP_H 
#define BIGLOO_REAL_HEAP_H

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

#define REAL(o) (CREAL(o)->real)
#define REAL_SIZE (sizeof(struct bgl_real))
   
/*---------------------------------------------------------------------*/
/*    tagging                                                          */
/*---------------------------------------------------------------------*/
#if (defined(TAG_REAL))
#   define BREAL(p) ((obj_t)((long)p + TAG_REAL))
#   define CREAL(p) ((obj_t)((long)p - TAG_REAL))
#   define BGL_REAL_CNST(name) name
#   define DEFINE_REAL(name, aux, flonum) \
      static struct { double real; } aux = { flonum }; \
      static const obj_t name = BREAL(&aux)

#   define FLONUMP(c) ((c && ((((long)c) & TAG_MASK) == TAG_REAL)))
#   define REALP(c) FLONUMP(c)

#   define BGL_REAL_SET(o, v) ((REAL(o).val = v), o)
#else
#   define BREAL(p) BREF(p)
#   define CREAL(p) CREF(p)
#   define BGL_REAL_CNST(name) name
#   define DEFINE_REAL(name, aux, flonum) \
      static struct { __CNST_ALIGN header_t header; double real; } \
	 aux = { __CNST_FILLER BGL_MAKE_HEADER(REAL_TYPE, 0), flonum }; \
      static const obj_t name = BREAL(&(aux.header))

#   define FLONUMP(c) (POINTERP(c) && (TYPE(c) == REAL_TYPE))
#   define REALP(c) FLONUMP(c)
   
#   define BGL_REAL_SET(o, v) ((REAL(o).val = v), o)
#endif

#define BGL_FAST_REALP(c) FLONUMP(c)

/*---------------------------------------------------------------------*/
/*    alloc                                                            */
/*---------------------------------------------------------------------*/
#if (!defined(TAG_REAL))
#  define IFN_REAL_TAG(expr) expr
#else
#  define IFN_REAL_TAG(expr)
#endif   

#if (BGL_GC != BGL_SAW_GC)
#  define DOUBLE_TO_REAL(d) (make_real(d))
#else					
BGL_RUNTIME_DECL obj_t bgl_saw_make_real(double);
#  define DOUBLE_TO_REAL(d) (bgl_saw_make_real(d))
#endif

#define REAL_TO_DOUBLE(r) (REAL(r).val)
#define BGL_FAST_REAL_TO_DOUBLE(r) REAL_TO_DOUBLE(r)
#define FLOAT_TO_REAL(d) (DOUBLE_TO_REAL((double)(d)))
#define REAL_TO_FLOAT(r) ((float)(REAL(r).val))

#define BGL_INIT_REAL(an_object, d) \
   IFN_REAL_TAG((an_object)->real.header = BGL_MAKE_HEADER(REAL_TYPE, REAL_SIZE)); \
   (an_object)->real.val = d;

#if (BGL_GC_CUSTOM || !defined(__GNUC__))
#  define MAKE_REAL(v) make_real(v)
#else
#  define MAKE_REAL(v) \
   ({ obj_t an_object = GC_MALLOC_ATOMIC(REAL_SIZE); \
      BGL_INIT_REAL(an_object, v); \
      BREAL(an_object); })
#endif

#define BGL_MAKE_INLINE_REAL(d) \
   an_object = GC_MALLOC_ATOMIC(REAL_SIZE); \
   IFN_REAL_TAG(an_object->real_t.header = BGL_MAKE_HEADER(REAL_TYPE, REAL_SIZE)); \
   an_object->real.val = d; \
   BREAL(an_object)

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

