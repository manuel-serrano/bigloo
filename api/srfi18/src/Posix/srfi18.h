/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/srfi18/src/Posix/srfi18.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 22 11:01:20 2002                          */
/*    Last change :  Tue Dec 11 16:20:42 2012 (serrano)                */
/*    Copyright   :  2002-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The C headers for Bigloo pthreads.                               */
/*=====================================================================*/
#include <pthread.h>
#include <bigloo.h>
#include <bglpthread.h>

#if( !defined( BGL_SRFI18_H ) )
#define BGL_SRFI18_H 1

/*---------------------------------------------------------------------*/
/*    srfi18thread_t                                                   */
/*---------------------------------------------------------------------*/
typedef struct srfi18thread {
   struct bglpthread bglpthread;
   struct srfi18mutex *mutexes;
} *srfi18thread_t;

/*---------------------------------------------------------------------*/
/*    srfi18mutex_t                                                    */
/*---------------------------------------------------------------------*/
typedef struct srfi18mutex {
   struct bglpmutex mutex;
   srfi18thread_t thread;
   bool_t marked;
   struct srfi18mutex *prev;
   struct srfi18mutex *next;
} *srfi18mutex_t;

/*---------------------------------------------------------------------*/
/*    Prototypes                                                       */
/*---------------------------------------------------------------------*/
extern obj_t srfi18_make_mutex( obj_t );
extern bool_t srfi18_mutex_lock( void * );
extern bool_t srfi18_mutex_unlock( void * );

extern void srfi18_mutex_mark_locked( srfi18mutex_t, srfi18thread_t );
extern void srfi18_mutex_mark_unlocked( srfi18mutex_t );

extern obj_t srfi18_make_condvar( obj_t );

#endif
