/*=====================================================================*/
/*    .../project/bigloo/bigloo/api/srfi18/src/Posix/cthread.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 22 12:12:04 2002                          */
/*    Last change :  Wed Sep 25 13:50:13 2019 (serrano)                */
/*    Copyright   :  2002-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C utilities for native Bigloo pthreads implementation.           */
/*=====================================================================*/
#include <pthread.h>
#include <sched.h>
#include <stdlib.h>
#include <string.h>

#define GC_PRIVATE_H
#include <gc.h>
#include <bglpthread.h>
#include <srfi18.h>

#if BGL_HAVE_SIGACTION
#include <signal.h>
#endif

/*---------------------------------------------------------------------*/
/*    extern                                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL void srfi18_mutexes_abandon();
BGL_RUNTIME_DECL void bglpth_thread_run();
BGL_RUNTIME_DECL void bglpth_thread_env_create();

/*---------------------------------------------------------------------*/
/*    srfi18read_t                                                     */
/*    srfi18_thread_new ...                                            */
/*---------------------------------------------------------------------*/
srfi18thread_t
srfi18_thread_new( obj_t thunk ) {
   srfi18thread_t t = (srfi18thread_t)GC_MALLOC( sizeof( struct srfi18thread ) );

   pthread_mutex_init( &(t->bglpthread.mutex), 0L );
   pthread_cond_init( &(t->bglpthread.condvar), 0L );

   t->bglpthread.thunk = thunk;
   t->bglpthread.specific = BUNSPEC;
   t->bglpthread.cleanup = BUNSPEC;
   t->bglpthread.status = 0;

   t->mutexes = 0;
   
   return t;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    srfi18_thread_cleanup ...                                        */
/*---------------------------------------------------------------------*/
void
srfi18_thread_cleanup( void *arg ) {
   /* abandon all locked mutexes */
   srfi18_mutexes_abandon( (srfi18thread_t)arg );
}

/*---------------------------------------------------------------------*/
/*    static void *                                                    */
/*    srfi18_thread_run ...                                            */
/*---------------------------------------------------------------------*/
static void *
srfi18_thread_run( void *arg ) {
   srfi18thread_t self = (srfi18thread_t)arg;
   
   pthread_cleanup_push( srfi18_thread_cleanup, arg );

   bglpth_thread_run( arg );
   
   pthread_cleanup_pop( 1 );

   return (void *)self;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    srfi18_thread_start ...                                          */
/*---------------------------------------------------------------------*/
void
srfi18_thread_start( srfi18thread_t thread, obj_t bglthread, bool_t dt ) {
   pthread_attr_t a;
   int ret;

   pthread_attr_init( &a );

   if( dt ) pthread_attr_setdetachstate( &a, PTHREAD_CREATE_DETACHED );

   bglpth_thread_env_create( thread, bglthread );
   
   if( ret = pthread_create( &(thread->bglpthread.pthread), &a, srfi18_thread_run, thread ) )
      FAILURE( string_to_bstring( "thread-start!" ),
	       string_to_bstring( "Cannot start thread" ),
	       string_to_bstring( strerror( ret ) ) );
}
