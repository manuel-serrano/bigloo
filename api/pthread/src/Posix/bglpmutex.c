/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Posix/bglpmutex.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Fri Dec 31 12:21:13 2010 (serrano)                */
/*    Copyright   :  2004-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Posix mutex implementation                                   */
/*=====================================================================*/
#ifdef  _MINGW_VER
#  include <sys/timeb.h>
#endif
#include <pthread.h>
#include <sched.h>
#include <stdlib.h>
#include <string.h>

#define GC_PRIVATE_H
#include <gc.h>
#include <bglpthread.h>

#include <sys/time.h>
#include <time.h>

#ifdef _MSC_VER
#  include <sys/timeb.h>
#endif

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL void bgl_mutex_init_register( obj_t (*)(obj_t) );
BGL_RUNTIME_DECL void bgl_mutex_lock_register( bool_t (*)(obj_t) );
BGL_RUNTIME_DECL void bgl_mutex_timed_lock_register( bool_t (*)(obj_t, long) );
BGL_RUNTIME_DECL void bgl_mutex_unlock_register( bool_t (*)(obj_t) );
BGL_RUNTIME_DECL void bgl_mutex_state_register( obj_t (*)(obj_t) );

BGL_RUNTIME_DECL void bgl_sleep( long );

/*---------------------------------------------------------------------*/
/*    Mutex symbols                                                    */
/*---------------------------------------------------------------------*/
static obj_t sym_not_owned = 0L;
static obj_t sym_abandoned = 0L;
static obj_t sym_not_abandoned = 0L;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_mutex_symbols_init ...                                       */
/*---------------------------------------------------------------------*/
static void
bgl_mutex_symbols_init() {
   if( !sym_not_owned ) {
      sym_not_owned = string_to_symbol( "not-owned" );
      sym_abandoned = string_to_symbol( "abandoned" );
      sym_not_abandoned = string_to_symbol( "not-abandoned" );
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    mutex_unlink ...                                                 */
/*---------------------------------------------------------------------*/
static void
mutex_unlink( obj_t m ) {
   obj_t prev = BGLPTH_MUTEX_BGLPMUTEX( m )->prev;
   obj_t next = BGLPTH_MUTEX_BGLPMUTEX( m )->next;

   if( prev ) {
      BGLPTH_MUTEX_BGLPMUTEX( m )->prev = 0;
      BGLPTH_MUTEX_BGLPMUTEX( prev )->next = next;
   }

   if( next ) {
      BGLPTH_MUTEX_BGLPMUTEX( m )->next = 0;
      BGLPTH_MUTEX_BGLPMUTEX( next )->prev = prev;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_mutex_mark_unlocked ...                                   */
/*---------------------------------------------------------------------*/
void
bglpth_mutex_mark_unlocked( obj_t m, bglpmutex_t mut ) {
   /* unlink the mutex */
   mutex_unlink( m );
   
   /* mark the mutex has free */
   mut->locked = 0;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglpth_mutex_unlock_sans_thread ...                              */
/*---------------------------------------------------------------------*/
static void
bglpth_mutex_unlock_sans_thread( obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );

   /* mark the Bigloo state of the lock */
   bglpth_mutex_mark_unlocked( m, mut );
   
   /* physically unlock it */
   pthread_mutex_unlock( &(mut->pmutex) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_mutex_state ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bglpth_mutex_state( obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );

   bgl_mutex_symbols_init();
   
   if( mut->locked ) {
      if( mut->thread ) {
	 return mut->thread->bglthread;
      }
      
      return sym_not_owned;
   } else {
      if( mut->thread ) {
	 return sym_abandoned;
      }
      else 
	 return sym_not_abandoned;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_mutex_init ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bglpth_mutex_init( obj_t m ) {
   bglpmutex_t mut = (bglpmutex_t)GC_MALLOC( sizeof( struct bglpmutex ) );

   mut->thread = 0L;
   mut->locked = 0;
   mut->specific = BUNSPEC;

   m->mutex_t.mutex = mut;
   if( pthread_mutex_init( &(mut->pmutex), 0L ) )
      FAILURE( string_to_bstring( "make-mutex" ),
	       string_to_bstring( "Cannot create mutex" ),
	       string_to_bstring( strerror( errno ) ) );

   mut->next = 0;
   mut->prev = 0;
   
   return m;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_mutexes_unlock ...                                        */
/*---------------------------------------------------------------------*/
void
bglpth_mutexes_unlock( bglpthread_t thread ) {
   obj_t w = thread->mutexes;
   
   while( w ) {
      obj_t n = BGLPTH_MUTEX_BGLPMUTEX( w )->next;

      bglpth_mutex_unlock_sans_thread( w );
      w = n;
   }
}
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    register_mutex ...                                               */
/*    -------------------------------------------------------------    */
/*    It is mandatory that mutex does not allocate anything because    */
/*    it might happen cases where bglpth_mutex_lock is called from     */
/*    while the GC is already allocating something (because of Unix    */
/*    signals, for instance raised on process termination).            */
/*---------------------------------------------------------------------*/
static void
register_mutex( obj_t m, bglpthread_t thread ) {
   if( thread->mutexes ) { 
      BGLPTH_MUTEX_BGLPMUTEX( m )->next = thread->mutexes;
      BGLPTH_MUTEX_BGLPMUTEX( thread->mutexes )->prev = m;
   }
   thread->mutexes = m;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_mutex_mark_locked ...                                     */
/*---------------------------------------------------------------------*/
void
bglpth_mutex_mark_locked( obj_t m, bglpmutex_t mut ) {
   bglpthread_t cth = bglpth_current_pthread();

   mut->locked = 1;
   
   if( cth && (mut->thread != cth) ) {
      mut->thread = cth;
      register_mutex( m, cth );
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_mutex_lock ...                                            */
/*---------------------------------------------------------------------*/
bool_t
bglpth_mutex_lock( obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m ); 
   bool_t res = !pthread_mutex_lock( &(mut->pmutex) );
   
   if( res ) bglpth_mutex_mark_locked( m, mut );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_mutex_timed_lock ...                                      */
/*---------------------------------------------------------------------*/
bool_t
bglpth_mutex_timed_lock( obj_t m, long ms ) {
#if BGL_HAVE_MUTEX_TIMEDLOCK
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );
   struct timespec timeout;
   bool_t res;
#if defined( _MINGW_VER ) || defined( _MSC_VER )
   struct timeb tb;
   ftime( &tb );
   timeout.tv_sec = tb.time + (ms / 1000);
   timeout.tv_nsec = (tb.millitm * 1000000) + ((ms % 1000) * 100000); 
#else
   struct timeval now;
   gettimeofday( &now, 0 );
   timeout.tv_sec = now.tv_sec + (ms / 1000);
   timeout.tv_nsec = (now.tv_usec * 1000) + ((ms % 1000) * 100000);
   gettimeofday( &now, 0 );
#endif

   res = !pthread_mutex_timedlock( &(mut->pmutex), &timeout );

   if( res  ) {
      bglpth_mutex_mark_locked( m, mut );
   } else {
      mut->thread = 0L;
   }

   return res;
#else
   int res;
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );
   while( (res=(pthread_mutex_trylock( &(mut->pmutex) ) == EBUSY ))
	  && (ms > 0) ) {
      ms -= 100;
      bgl_sleep( 100 * 1000 );
   }

   if( !res  ) {
      bglpth_mutex_mark_locked( m, mut );
   } else {
      mut->thread = 0L;
   }

   return !res;
#endif
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_mutex_unlock ...                                          */
/*---------------------------------------------------------------------*/
bool_t
bglpth_mutex_unlock( obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );

   if( mut->locked ) {
      /* unlink the mutex */
      mutex_unlink( m );
      
      /* mark the mutex has free */
      mut->thread = 0L;
      mut->locked = 0;

      /* physically unlock it */
      return !pthread_mutex_unlock( &(mut->pmutex) );
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_mutex ...                                           */
/*---------------------------------------------------------------------*/
void
bglpth_setup_mutex() {
   bgl_mutex_init_register( &bglpth_mutex_init );
   bgl_mutex_lock_register( &bglpth_mutex_lock );
   bgl_mutex_timed_lock_register( &bglpth_mutex_timed_lock );
   bgl_mutex_unlock_register( &bglpth_mutex_unlock );
   bgl_mutex_state_register( &bglpth_mutex_state );
}
