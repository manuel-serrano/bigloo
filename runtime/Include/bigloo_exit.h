/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/runtime/Include/bigloo_exit.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Apr 17 07:40:02 2018                          */
/*    Last change :  Sun Jun 20 19:33:38 2021 (serrano)                */
/*    Copyright   :  2018-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo EXITs                                                     */
/*=====================================================================*/
#ifndef BIGLOO_EXIT_H 
#define BIGLOO_EXIT_H

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
/*    SET_EXIT / JUMP_EXIT                                             */
/*---------------------------------------------------------------------*/
#define SET_EXIT( exit ) \
   SETJMP( jmpbuf )
#define JUMP_EXIT( exit, val ) \
   BGL_EXIT_VALUE_SET( val ), LONGJMP( exit, 1 )

#if( !defined( __ia64__ ) )
typedef jmp_buf_t callcc_jmp_buf;
		 
#  define CALLCC_SET_EXIT( exit ) SET_EXIT( exit )
#  define CALLCC_JUMP_EXIT( exit, val ) (JUMP_EXIT( exit, val ), BUNSPEC)
#else
/* IA64 code */
#include <ucontext.h>

struct ia64_rv_t {
   long retval;
   long first_return;
};
		 
typedef struct {
   ucontext_t ctx;
   long backing_store_size;
   void *backing_store;
   struct ia64_rv_t rv;
} callcc_jmp_buf;
		 
extern unsigned long  __libc_ia64_register_backing_store_base;
extern struct ia64_rv_t ia64_getcontext (ucontext_t *) __asm__ ("getcontext");
		 
#  define CALLCC_SET_EXIT( exit ) \
  (jmpbuf.rv = ia64_getcontext( &(jmpbuf.ctx) ), \
     (jmpbuf.rv.first_return ? \
       jmpbuf.backing_store_size = jmpbuf.ctx.uc_mcontext.sc_ar_bsp - \
                                   __libc_ia64_register_backing_store_base, \
       jmpbuf.backing_store = GC_MALLOC( jmpbuf.backing_store_size ), \
       memcpy( jmpbuf.backing_store, \
               (void *)__libc_ia64_register_backing_store_base, \
               jmpbuf.backing_store_size ), \
       0 : 1))

#  define CALLCC_JUMP_EXIT( exit, val ) \
   (BGL_EXIT_VALUE_SET( val ), \
    memcpy( (void *)__libc_ia64_register_backing_store_base, \
            ((callcc_jmp_buf *)exit)->backing_store, \
            ((callcc_jmp_buf *)exit)->backing_store_size), \
    setcontext( &(((callcc_jmp_buf *)exit)->ctx) ), \
    BUNSPEC)
#endif
		 
/*---------------------------------------------------------------------*/
/*    the `bind-exit' linking.                                         */
/*---------------------------------------------------------------------*/
struct exitd {
   void *exit;
#if( !defined( __ia64__ ) )
   long userp;
#endif
   int flags;
   obj_t stamp;
   /* protected blocks */
   union scmobj *protect0;
   union scmobj *protect1;
   union scmobj *protectn;
   /* linking */
   struct bgl_dframe *top_of_frame;
   struct exitd *prev;
};

/* #define EXITD_SYSTEM 0                                              */
#define EXITD_USER 1
#define EXITD_CALLCC 2

#define BGL_EXITD_HAS_TOP_OF_FRAME 1
#define BGL_EXITD_HAS_PROTECT0 2
#define BGL_EXITD_HAS_PROTECT1 4
#define BGL_EXITD_HAS_PROTECTN 8

#if 1 || (BIGLOO_TRACE > 0)
#  define BGL_EXITD_TOP_OF_FRAME_PUSH(env) \
     exitd.flags = BGL_EXITD_HAS_TOP_OF_FRAME; \
     exitd.top_of_frame = BGL_ENV_GET_TOP_OF_FRAME(env)
#  define BGL_EXITD_TOP_OF_FRAME_POP(env) \
     BGL_ENV_SET_TOP_OF_FRAME(env, BGL_ENV_EXITD_TOP(env)->top_of_frame)
#else
#  define BGL_EXITD_TOP_OF_FRAME_PUSH(env) \
     exitd.flags = 0;
#  define BGL_EXITD_TOP_OF_FRAME_POP(env)
#endif

#if( !defined( __ia64__ ) )
#  define BGL_EXITD_USERP_SET(_exitd,_ser) _exitd.userp = _ser
#else
#  define BGL_EXITD_USERP_SET(_exitd,_ser)
#endif

#define PUSH_ENV_EXIT_CALLCC(env, _xit, _ser) \
   struct exitd exitd; \
   exitd.exit = _xit; \
   BGL_EXITD_USERP_SET(exitd,_ser); \
   BGL_EXITD_TOP_OF_FRAME_PUSH(env); \
   exitd.protect0 = BFALSE; \
   exitd.protect1 = BFALSE; \
   exitd.protectn = BNIL; \
   exitd.prev = BGL_ENV_EXITD_TOP(env); \
   exitd.stamp = BGL_ENV_EXITD_STAMP(env); \
   BGL_ENV_EXITD_TOP_SET(env, (&exitd));
   
#define PUSH_ENV_EXIT(env, _xit, _ser) \
   struct exitd exitd; \
   exitd.exit = _xit; \
   BGL_EXITD_USERP_SET(exitd,_ser); \
   BGL_EXITD_TOP_OF_FRAME_PUSH(env); \
   exitd.protect0 = BFALSE; \
   exitd.protect1 = BFALSE; \
   exitd.protectn = BNIL; \
   exitd.prev = BGL_ENV_EXITD_TOP(env); \
   BGL_ENV_EXITD_TOP_SET(env, (&exitd));
   
#define PUSH_EXIT(_xit, _ser) \
   PUSH_ENV_EXIT(BGL_CURRENT_DYNAMIC_ENV(), _xit, _ser)

#define POP_ENV_EXIT(env) \
   BGL_EXITD_TOP_OF_FRAME_POP(env); \
   BGL_ENV_EXITD_TOP_SET(env, BGL_ENV_EXITD_TOP(env)->prev)

#define POP_EXIT() \
   POP_ENV_EXIT(BGL_CURRENT_DYNAMIC_ENV())

#define EXITD_TO_EXIT( ptr ) \
   ((struct exitd *)(ptr))->exit

#if( !defined( __ia64__ ) )
#  define EXITD_CALLCCP(ptr) (((struct exitd *)(ptr))->userp == EXITD_CALLCC)
#else
#  define EXITD_CALLCCP(ptr) 0
#endif

#define EXITD_STAMP( ptr ) \
   (((struct exitd *)(ptr))->stamp)

#define EXITD_FLAGS( ptr ) \
   (((struct exitd *)(ptr))->flags)

#define BGL_EXITD_BOTTOMP( extd ) \
   (((struct exitd *)(extd)) == BGL_ENV_EXITD_BOTTOM( BGL_CURRENT_DYNAMIC_ENV() ))

#define BGL_EXITD_PROTECT0(ptr) \
   ((((struct exitd *)(ptr))->flags & BGL_EXITD_HAS_PROTECT0) \
    ? (((struct exitd *)(ptr))->protect0) : BFALSE)
   
#define BGL_EXITD_PROTECT1(ptr) \
   ((((struct exitd *)(ptr))->flags & BGL_EXITD_HAS_PROTECT1) \
    ? (((struct exitd *)(ptr))->protect1) : BFALSE)
   
#define BGL_EXITD_PROTECTN(ptr) \
   ((((struct exitd *)(ptr))->flags & BGL_EXITD_HAS_PROTECTN) \
    ? (((struct exitd *)(ptr))->protectn) : BNIL)

#define BGL_EXITD_PROTECT0_SET(extd, p) \
   ((((struct exitd *)(extd))->flags |= BGL_EXITD_HAS_PROTECT0), \
    (((struct exitd *)(extd))->protect0) = (p))
   
#define BGL_EXITD_PROTECT1_SET(extd, p) \
   ((((struct exitd *)(extd))->flags |= BGL_EXITD_HAS_PROTECT1), \
    (((struct exitd *)(extd))->protect1) = (p))
   
#define BGL_EXITD_PROTECTN_SET(extd, p) \
   ((((struct exitd *)(extd))->flags |= BGL_EXITD_HAS_PROTECTN), \
    (((struct exitd *)(extd))->protectn) = (p))

#define BGL_EXITD_PUSH_PROTECT(extd, p) \
   BGL_EXITD_PROTECT0(extd) == BFALSE ? BGL_EXITD_PROTECT0_SET(extd, p) : \
   BGL_EXITD_PROTECT1(extd) == BFALSE ? BGL_EXITD_PROTECT1_SET(extd, p) : \
      BGL_EXITD_PROTECTN_SET(extd, MAKE_STACK_PAIR(p, BGL_EXITD_PROTECTN(extd)))
   
#define BGL_EXITD_POP_PROTECT(extd) \
   BGL_EXITD_PROTECT1(extd) == BFALSE ? \
      BGL_EXITD_PROTECT0_SET(extd, BFALSE) :	\
      NULLP(BGL_EXITD_PROTECTN(extd)) ? \
        BGL_EXITD_PROTECT1_SET(extd, BFALSE) : \
        BGL_EXITD_PROTECTN_SET(extd, CDR(BGL_EXITD_PROTECTN(extd)))
   
/*---------------------------------------------------------------------*/
/*    `dynamic-wind' before thunk linking.                             */
/*---------------------------------------------------------------------*/
struct befored {
   obj_t before;
   struct befored *prev;
};

#define PUSH_BEFORE( _bfr ) \
   struct befored befored; \
   befored.before = _bfr; \
   befored.prev = BGL_BEFORED_TOP(); \
   BGL_BEFORED_TOP_SET( &befored );

#define POP_BEFORE() \
   BGL_BEFORED_TOP_SET( BGL_BEFORED_TOP()->prev )

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

