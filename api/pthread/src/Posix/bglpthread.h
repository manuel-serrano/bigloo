/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Posix/bglpthread.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 22 11:01:20 2002                          */
/*    Last change :  Fri Dec 14 19:55:59 2012 (serrano)                */
/*    Copyright   :  2002-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The C headers for Bigloo pthreads.                               */
/*=====================================================================*/
#include <pthread.h>
#include <bigloo.h>

#if( !defined( BGL_PTHREAD_H ) )
#define BGL_PTHREAD_H 1

/*---------------------------------------------------------------------*/
/*    bglpthread_t                                                     */
/*---------------------------------------------------------------------*/
typedef struct bglpthread {
   obj_t name;
   obj_t thunk;
   pthread_mutex_t mutex;
   pthread_cond_t condvar;
   pthread_t pthread;
   obj_t bglthread;
   obj_t env;
   obj_t specific;
   obj_t cleanup;
   int status;
} *bglpthread_t;

/*---------------------------------------------------------------------*/
/*    bglpmutex_t                                                      */
/*---------------------------------------------------------------------*/
typedef struct bglpmutex {
   /* the actual pmutex (must be first field) */
   pthread_mutex_t pmutex;
   /* the Bigloo mutex pointing to that mutex */
   obj_t bmutex;           
   obj_t specific;
#if !BGL_POSIX_CONDV_TIMEDWAIT
   int locked;
#endif   
} *bglpmutex_t;

/*---------------------------------------------------------------------*/
/*    bglpspinlock_t                                                   */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_SPINLOCK )
typedef struct bglpspinlock {
   /* the actual pmutex (must be first field) */
   pthread_spinlock_t pmutex;
   /* the Bigloo mutex pointing to that mutex */
   obj_t bmutex;           
} *bglpspinlock_t;
#endif      

/*---------------------------------------------------------------------*/
/*    bglpcondvar_t                                                    */
/*---------------------------------------------------------------------*/
typedef struct bglpcondvar {
   pthread_cond_t pcondvar;
   obj_t specific;
} *bglpcondvar_t;

/*---------------------------------------------------------------------*/
/*    Macros                                                           */
/*---------------------------------------------------------------------*/
#define BGLPTH_THREAD_SPECIFIC( t ) \
  ((((bglpthread_t)(t))->specific))
#define BGLPTH_THREAD_SPECIFIC_SET( t, v ) \
  ((((bglpthread_t)(t))->specific) = (v))

#define BGLPTH_THREAD_CLEANUP( t ) \
  ((((bglpthread_t)(t))->cleanup))

#define BGLPTH_THREAD_CLEANUP_SET( t, v ) \
  ((((bglpthread_t)(t))->cleanup = (v)))

#define BGLPTH_MUTEX_SPECIFIC( m ) \
   (((bglpmutex_t)(BGL_MUTEX_SYSMUTEX( m ) ))->specific)
#define BGLPTH_MUTEX_SPECIFIC_SET( m, v ) \
   ((((bglpmutex_t)(BGL_MUTEX_SYSMUTEX( m ) ))->specific) = (v))

#define BGLPTH_CONDVAR_BGLPCONDVAR( o ) \
   ((bglpcondvar_t)(BGL_CONDVAR( o ).condvar))
#define BGLPTH_CONDVAR_PCONDVAR( o ) \
   (&(BGLPTH_CONDVAR_BGLPCONDVAR( o )->pcondvar))

#define BGLPTH_CONDVAR_SPECIFIC( m ) \
  (BGLPTH_CONDVAR_BGLPCONDVAR( m )->specific)
#define BGLPTH_CONDVAR_SPECIFIC_SET( m, v ) \
  (BGLPTH_CONDVAR_SPECIFIC( m ) = (v))

/*---------------------------------------------------------------------*/
/*    Prototypes                                                       */
/*---------------------------------------------------------------------*/
extern obj_t bglpth_make_mutex( obj_t );

extern obj_t bglpth_make_condvar( obj_t );

extern bglpthread_t bglpth_current_pthread();

#endif
