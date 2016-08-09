/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/srfi18/src/Posix/ccondvar.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Tue Aug  9 10:55:29 2016 (serrano)                */
/*    Copyright   :  2004-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Posix condition variable implementation                      */
/*=====================================================================*/
#include <pthread.h>
#include <sched.h>
#include <stdlib.h>
#include <string.h>

#define GC_PRIVATE_H
#include <gc.h>
#include <bglpthread.h>
#include <srfi18.h>

#include <sys/time.h>

#if defined( _MSC_VER ) || defined( _MINGW_VER )
#  include <sys/timeb.h>
#endif

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL void bgl_condvar_init_register( obj_t (*)( obj_t ) );
BGL_RUNTIME_DECL obj_t bgl_create_condvar( obj_t );
BGL_RUNTIME_DECL void bglpth_condvar_init();

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    srfi18_condvar_wait ...                                          */
/*---------------------------------------------------------------------*/
bool_t
srfi18_condvar_wait( obj_t cv, obj_t m ) {
   bglpmutex_t mut = (bglpmutex_t)BGL_MUTEX_SYSMUTEX( m );
   srfi18mutex_t mut18 = (srfi18mutex_t)mut;
   srfi18thread_t thread = mut18->thread;
   bool_t res;

   mut18->locked = 0;
   srfi18_mutex_mark_unlocked( mut18 );
   res = pthread_cond_wait( BGLPTH_CONDVAR_PCONDVAR( cv ), &(mut->pmutex) );
   
   srfi18_mutex_mark_locked( mut18, thread );
   mut18->locked = 1;
   
   return !res;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    srfi18_condvar_timed_wait ...                                    */
/*---------------------------------------------------------------------*/
bool_t
srfi18_condvar_timed_wait( obj_t cv, obj_t m, long ms ) {
   bglpmutex_t mut = (bglpmutex_t)BGL_MUTEX_SYSMUTEX( m );
   srfi18mutex_t mut18 = (srfi18mutex_t)mut;
   srfi18thread_t thread = mut18->thread;
   struct timespec timeout;
   bool_t res;
   
#if defined( _MINGW_VER ) || defined( _MSC_VER )
   struct timeb tb;
   ftime( &tb );
   timeout.tv_sec = tb.time + (ms / 1000);
   timeout.tv_nsec = (tb.millitm * 1000000) + ((ms % 1000) * 1000000); 
#else
   struct timeval now;
   gettimeofday( &now, 0 );
   timeout.tv_sec = now.tv_sec + (ms / 1000);
   timeout.tv_nsec = (now.tv_usec * 1000) + ((ms % 1000) * 1000000);
#endif

   mut18->locked = 0;
   srfi18_mutex_mark_unlocked( mut18 );
   res = pthread_cond_timedwait( BGLPTH_CONDVAR_PCONDVAR( cv ),
				 &(mut->pmutex),
				 &timeout );
   srfi18_mutex_mark_locked( mut18, thread );
   mut18->locked = 1;
   
   return !res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    srfi18_condvar_init ...                                          */
/*---------------------------------------------------------------------*/
obj_t
srfi18_condvar_init( obj_t cv ) {
   bglpth_condvar_init( cv );

   BGL_CONDVAR( cv ).syswait = &srfi18_condvar_wait;
   BGL_CONDVAR( cv ).systimedwait = &srfi18_condvar_timed_wait;
      
   return cv;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    srfi18_make_condvar ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
srfi18_make_condvar( obj_t name ) {
   return srfi18_condvar_init( bgl_create_condvar( name ) );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    srfi18_setup_condvar ...                                         */
/*---------------------------------------------------------------------*/
void
srfi18_setup_condvar() {
   bgl_condvar_init_register( &srfi18_condvar_init );
}
