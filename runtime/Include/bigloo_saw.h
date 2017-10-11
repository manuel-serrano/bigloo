/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Include/bigloo_saw.h         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 08:05:01 2016                          */
/*    Last change :  Wed Oct 11 07:30:26 2017 (serrano)                */
/*    Copyright   :  2016-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo SAW                                                       */
/*=====================================================================*/
#ifndef BIGLOO_SAW_H 
#define BIGLOO_SAW_H

/*---------------------------------------------------------------------*/
/*    Does someone really wants C++ here?                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus_just_for_emacs_indent
}
#endif

#include <bigloo_config.h>
#if( BGL_SAW == 1 ) 

/*---------------------------------------------------------------------*/
/*    extern                                                           */
/*---------------------------------------------------------------------*/
extern void bps_dobackptr( obj_t *field, obj_t value );
extern void bps_bmassign( obj_t *field, obj_t value );

/*---------------------------------------------------------------------*/
/*    MEMROUND                                                         */
/*---------------------------------------------------------------------*/
#define MEMROUND( bytes ) (((bytes + 7) >> TAG_SHIFT) << TAG_SHIFT)

/*---------------------------------------------------------------------*/
/*    saw_fram_header_t ...                                            */
/*---------------------------------------------------------------------*/
typedef struct bgl_saw_frame_header {
  long size;
  char *name;
  struct bgl_saw_frame_header *link;
} bgl_saw_frame_header_t;

/*---------------------------------------------------------------------*/
/*    bgl_saw_nursery_t                                                */
/*---------------------------------------------------------------------*/
typedef struct {
   long size;
   char *heap;
   obj_t **backpool;
   char *alloc;
   obj_t **backptr;
} bgl_saw_nursery_t;

/*---------------------------------------------------------------------*/
/*    bgl_saw_copier_t ...                                             */
/*---------------------------------------------------------------------*/
typedef obj_t (*bgl_saw_copier_t)( obj_t );

/*---------------------------------------------------------------------*/
/*    extern                                                           */
/*---------------------------------------------------------------------*/
extern bgl_saw_nursery_t bgl_saw_nursery;

extern void bgl_saw_gc_add_copier( long, bgl_saw_copier_t );
extern obj_t bgl_saw_gc_copy( obj_t );
extern void bgl_saw_gc();

/*---------------------------------------------------------------------*/
/*    alloc ...                                                        */
/*---------------------------------------------------------------------*/
#define BGL_SAW_CAN_ALLOC( tmp, bytes ) \
   (((bgl_saw_nursery.alloc + MEMROUND( bytes )) < (char *)bgl_saw_nursery.backptr) ? \
    tmp = (obj_t)(bgl_saw_nursery.alloc) : 0)

#define BGL_SAW_ALLOC( bytes ) \
   (bgl_saw_nursery.alloc += MEMROUND( bytes ))

/*---------------------------------------------------------------------*/
/*    backptr                                                          */
/*---------------------------------------------------------------------*/
#define BGL_SAW_BACKPTR( field )				    \
   ((((char *)(bgl_saw_nursery.backptr) <= bgl_saw_nursery.alloc) ? \
     bgl_saw_gc() : 0),						    \
    *(bgl_saw_nursery.backptr) = &(field),			    \
    bgl_saw_nursery.backptr -= 1,				    \
    BUNSPEC)

#define BGL_SAW_OLDYOUNG( old, young, field ) 	\
   ((BOLDP( old ) && BYOUNGP( young )) ? BGL_SAW_BACKPTR( field ) : BUNSPEC)

/*---------------------------------------------------------------------*/
/*    Saw instruction set                                              */
/*---------------------------------------------------------------------*/
#define BGL_RTL_STRING_REF(v,i) ((unsigned char) STRING_REF(v,i))
#define BGL_RTL_EQ(x,y) (x == y)
#define BGL_RTL_GE(x,y) (x >= y)
#define BGL_RTL_LE(x,y) (x <= y)
#define BGL_RTL_GT(x,y) (x > y)
#define BGL_RTL_LT(x,y) (x < y)
#define BGL_RTL_OR(x,y) (x | y)
#define BGL_RTL_AND(x,y) (x & y)
#define BGL_RTL_ADD(x,y) (x + y)
#define BGL_RTL_SUB(x,y) (x - y)
#define BGL_RTL_MUL(x,y) (x * y)
#define BGL_RTL_DIV(x,y) (x / y)
#define BGL_RTL_REM(x,y) (x % y)
#define BGL_RTL_XOR(x,y) (x ^ y)
#define BGL_RTL_RSH(x,y) (x >> y)
#define BGL_RTL_LSH(x,y) (x << y)
#define BGL_RTL_0L() (0L)					

#define BGL_RTL_NOP() (BUNSPEC)
#define BGL_RTL_MOV(r) (r)
#define BGL_RTL_RETURN(r) return(r)
#define BGL_RTL_LOADI(v) (v)
#define BGL_RTL_LOADG(g) (g)
#define BGL_RTL_LOADFUN(g) ((obj_t) g)
#define BGL_RTL_STOREG(g,v) BMASSIGN(g,v)
#define BGL_RTL_TSTOREG(g,v) ((g)=(v))
#define BGL_RTL_GLOBALREF(g) __EVMEANING_ADDRESS(g)
#define BGL_RTL_GO(l) goto l
#define BGL_RTL_IFEQ(l,r) if(!r) goto l
#define BGL_RTL_IFNE(l,r) if(r) goto l
#define BGL_RTL_APPLY(f,r) apply(f,r)
#define BGL_RTL_VALLOC(n) create_vector(n)
#define BGL_RTL_VREF(v,i) VECTOR_REF(v,i)
#define BGL_RTL_VSET(v,i,r) VECTOR_SET(v,i,r)
#define BGL_RTL_VLENGTH(v) VECTOR_LENGTH(v)
#define BGL_RTL_TVALLOC(m,t,n,d) ALLOCATE_TVECTOR(m,t,n,d)
#define BGL_RTL_TVREF(m,t,v,i) TVECTOR_REF(m,v,i)
#define BGL_RTL_TVSET(m,t,v,i,r) TVECTOR_REF(m,v,i)=r
#define BGL_RTL_TVLENGTH(m,t,v) TVECTOR_LENGTH(v)
#define BGL_RTL_CAST(t,e) ((t) e)
#define BGL_RTL_CAST_NULL(t) ((t) 0)
#define BGL_RTL_JUMPEXIT(x,v) JUMP_EXIT(x,v)
#define BGL_RTL_FAIL(p,m,o) FAILURE(p,m,o)
#define BGL_RTL_PROTECTED(x) (x)
#define BGL_RTL_MAKEBOX(v) MAKE_YOUNG_CELL(v)
#define BGL_RTL_BOXREF(r) CELL_REF(r)
#define BGL_RTL_BOXSET(r,v) CELL_SET(r,v)

#define BGL_RTL_PUSH_ENV_EXIT( env, _xit, _ser ) \
   exitd.exit  = _xit; \
   exitd.userp = _ser; \
   exitd.protect0 = BFALSE; \
   exitd.protect1 = BFALSE; \
   exitd.protectn = BNIL; \
   exitd.top_of_frame = BGL_ENV_GET_TOP_OF_FRAME( env ); \
   exitd.prev  = BGL_ENV_EXITD_TOP( env ); \
   exitd.stamp = BGL_ENV_EXITD_STAMP( env ); \
   BGL_ENV_EXITD_TOP_SET( env, (&exitd) );

#define BGL_RTL_PUSH_EXIT( _xit, _ser ) \
   BGL_RTL_PUSH_ENV_EXIT( BGL_CURRENT_DYNAMIC_ENV(), _xit, _ser )

#define BGL_RTL_PUSH_BEFORE( _bfr ) \
   befored.before = _bfr; \
   befored.prev = BGL_BEFORED_TOP(); \
   BGL_BEFORED_TOP_SET( &befored );

#endif
#endif
/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
