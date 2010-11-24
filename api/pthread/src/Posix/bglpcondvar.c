/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Posix/bglpcondvar.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Wed Nov 24 06:58:21 2010 (serrano)                */
/*    Copyright   :  2004-10 Manuel Serrano                            */
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
BGL_RUNTIME_DECL void bgl_condvar_wait_register( bool_t (*)( obj_t, obj_t ) );
BGL_RUNTIME_DECL void bgl_condvar_timed_wait_register( bool_t (*)( obj_t, obj_t, long ) );
BGL_RUNTIME_DECL void bgl_condvar_signal_register( bool_t (*)( obj_t ) );
BGL_RUNTIME_DECL void bgl_condvar_broadcast_register( bool_t (*)( obj_t ) );

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_condvar_init ...                                          */
/*---------------------------------------------------------------------*/
obj_t
bglpth_condvar_init( obj_t cv ) {
   bglpcondvar_t co = (bglpcondvar_t)GC_MALLOC( sizeof( struct bglpcondvar ) );

   co->specific = BUNSPEC;
   cv->condvar_t.condvar = co;

   if( pthread_cond_init( &(co->pcondvar), 0L ) )
      FAILURE( string_to_bstring( "make-condition-variable" ),
	       string_to_bstring( "Cannot create condition-variable" ),
	       string_to_bstring( strerror( errno ) ) );

   return cv;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_condvar_wait ...                                          */
/*---------------------------------------------------------------------*/
bool_t
bglpth_condvar_wait( obj_t cv, obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );
   bool_t res;

   mut->thread = 0L;
   bglpth_mutex_mark_unlocked( m, mut );
   res = !pthread_cond_wait( BGLPTH_CONDVAR_PCONDVAR( cv ), &(mut->pmutex) );

   if( res ) bglpth_mutex_mark_locked( m, mut );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_condvar_timed_wait ...                                    */
/*---------------------------------------------------------------------*/
bool_t
bglpth_condvar_timed_wait( obj_t cv, obj_t m, long ms ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );
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
   
   bglpth_mutex_mark_unlocked( m, mut );
   res = !pthread_cond_timedwait( BGLPTH_CONDVAR_PCONDVAR( cv ),
				  &(mut->pmutex),
				  &timeout );
   
   bglpth_mutex_mark_locked( m, mut );
   
   return res;
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
/*    void                                                             */
/*    bglpth_setup_condvar ...                                         */
/*---------------------------------------------------------------------*/
void
bglpth_setup_condvar() {
   bgl_condvar_init_register( &bglpth_condvar_init );
   bgl_condvar_wait_register( &bglpth_condvar_wait );
   bgl_condvar_timed_wait_register( &bglpth_condvar_timed_wait );
   bgl_condvar_signal_register( &bglpth_condvar_signal );
   bgl_condvar_broadcast_register( &bglpth_condvar_broadcast );
}
