/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Posix/bglpthread.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 22 11:01:20 2002                          */
/*    Last change :  Thu Sep 16 07:28:32 2010 (serrano)                */
/*    Copyright   :  2002-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The C headers for Bigloo pthreads.                               */
/*=====================================================================*/
#include <pthread.h>
#include <bigloo.h>

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
   obj_t mutexes;
} *bglpthread_t;

/*---------------------------------------------------------------------*/
/*    bglpmutex_t                                                      */
/*---------------------------------------------------------------------*/
typedef struct bglpmutex {
   pthread_mutex_t pmutex;
   bglpthread_t thread;
   bool_t locked;
   obj_t specific;
   obj_t prev;
   obj_t next;
} *bglpmutex_t;

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
  (BGLPTH_MUTEX_BGLPMUTEX( m )->specific)
#define BGLPTH_MUTEX_SPECIFIC_SET( m, v ) \
  (BGLPTH_MUTEX_SPECIFIC( m ) = (v))

#define BGLPTH_MUTEX_BGLPMUTEX( o ) \
   ((bglpmutex_t)((o)->mutex_t.mutex))
#define BGLPTH_MUTEX_PMUTEX( o ) \
   (&(BGLPTH_MUTEX_BGLPMUTEX( o )->pmutex))

#define BGLPTH_CONDVAR_BGLPCONDVAR( o ) \
   ((bglpcondvar_t)((o)->condvar_t.condvar))
#define BGLPTH_CONDVAR_PCONDVAR( o ) \
   (&(BGLPTH_CONDVAR_BGLPCONDVAR( o )->pcondvar))

#define BGLPTH_CONDVAR_SPECIFIC( m ) \
  (BGLPTH_CONDVAR_BGLPCONDVAR( m )->specific)
#define BGLPTH_CONDVAR_SPECIFIC_SET( m, v ) \
  (BGLPTH_CONDVAR_SPECIFIC( m ) = (v))

/*---------------------------------------------------------------------*/
/*    Prototypes                                                       */
/*---------------------------------------------------------------------*/
extern bool_t bglpth_mutex_lock( obj_t );
extern bool_t bglpth_mutex_unlock( obj_t );
extern void bglpth_mutexes_unlock( bglpthread_t );
extern void bglpth_mutex_mark_locked( obj_t, bglpmutex_t );
extern void bglpth_mutex_mark_unlocked( obj_t, bglpmutex_t );

extern bglpthread_t bglpth_current_pthread();
