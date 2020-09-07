/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Posix/bglpcondvar.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Mon Jun 24 15:28:36 2013 (serrano)                */
/*    Copyright   :  2004-13 Manuel Serrano                            */
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

#include <sys/time.h>

#if defined( _MSC_VER ) || defined( _MINGW_VER )
#  include <sys/timeb.h>
#endif

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL void bgl_condvar_init_register( obj_t (*)( obj_t ) );
BGL_RUNTIME_DECL obj_t bgl_create_condvar( obj_t );

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_condvar_wait ...                                          */
/*---------------------------------------------------------------------*/
bool_t
bglpth_condvar_wait( obj_t cv, obj_t m ) {
   bglpmutex_t mut = (bglpmutex_t)BGL_MUTEX_SYSMUTEX( m );
   bool_t res;

   res = pthread_cond_wait( BGLPTH_CONDVAR_PCONDVAR( cv ), &(mut->pmutex) );
   
   return !res;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_condvar_timed_wait ...                                    */
/*---------------------------------------------------------------------*/
bool_t
bglpth_condvar_timed_wait( obj_t cv, obj_t m, long ms ) {
   bglpmutex_t mut = (bglpmutex_t)BGL_MUTEX_SYSMUTEX( m );
   struct timespec timeout;
   long usec;

#if defined( _MINGW_VER ) || defined( _MSC_VER )
   struct timeb tb;
   ftime( &tb );

   usec = (tb.millitm + ms) * 1000;

   timeout.tv_nsec = (usec % 1000000) * 1000;
   timeout.tv_sec = tb.time + (usec / 1000000);
#else
   clock_gettime(CLOCK_REALTIME, &timeout);

   timeout.tv_nsec += (ms % 1000) * 1000000;
   timeout.tv_sec += (ms / 1000);
#endif

   return !pthread_cond_timedwait( BGLPTH_CONDVAR_PCONDVAR( cv ),
				   &(mut->pmutex),
				   &timeout );
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_condvar_signal ...                                        */
/*---------------------------------------------------------------------*/
bool_t
bglpth_condvar_signal( obj_t cv ) {
   return !pthread_cond_signal( BGLPTH_CONDVAR_PCONDVAR( cv ) );
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_condvar_broadcast ...                                     */
/*---------------------------------------------------------------------*/
bool_t
bglpth_condvar_broadcast( obj_t cv ) {
   return !pthread_cond_broadcast( BGLPTH_CONDVAR_PCONDVAR( cv ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_condvar_init ...                                          */
/*---------------------------------------------------------------------*/
obj_t
bglpth_condvar_init( obj_t cv ) {
   bglpcondvar_t co = (bglpcondvar_t)GC_MALLOC( sizeof( struct bglpcondvar ) );

   co->specific = BUNSPEC;

   BGL_CONDVAR( cv ).syswait = &bglpth_condvar_wait;
   BGL_CONDVAR( cv ).systimedwait = &bglpth_condvar_timed_wait;
   BGL_CONDVAR( cv ).syssignal = &bglpth_condvar_signal;
   BGL_CONDVAR( cv ).sysbroadcast = &bglpth_condvar_broadcast;
      
   BGL_CONDVAR( cv ).condvar = co;

   if( pthread_cond_init( &(co->pcondvar), 0L ) )
      FAILURE( string_to_bstring( "make-condition-variable" ),
	       string_to_bstring( "Cannot create condition-variable" ),
	       string_to_bstring( strerror( errno ) ) );

   return cv;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_make_condvar ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bglpth_make_condvar( obj_t name ) {
   return bglpth_condvar_init( bgl_create_condvar( name ) );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_condvar ...                                         */
/*---------------------------------------------------------------------*/
void
bglpth_setup_condvar() {
   bgl_condvar_init_register( &bglpth_condvar_init );
}
