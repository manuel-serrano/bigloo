 /*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Posix/bglpmutex.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Sun Dec  9 18:31:48 2012 (serrano)                */
/*    Copyright   :  2004-12 Manuel Serrano                            */
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
BGL_RUNTIME_DECL obj_t bgl_create_mutex( obj_t );
BGL_RUNTIME_DECL void bgl_sleep( long );

/*---------------------------------------------------------------------*/
/*    Mutex symbols                                                    */
/*---------------------------------------------------------------------*/
static obj_t sym_locked = 0L;
static obj_t sym_unlocked = 0L;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_mutex_symbols_init ...                                       */
/*---------------------------------------------------------------------*/
static void
bgl_mutex_symbols_init() {
   if( !sym_locked ) {
      sym_locked = string_to_symbol( "locked" );
      sym_unlocked = string_to_symbol( "unlocked" );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_mutex_state ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bglpth_mutex_state( void *m ) {
   bglpmutex_t mut = (bglpmutex_t)m;
   obj_t o = mut->bmutex;

   bgl_mutex_symbols_init();
   
   if( BGL_MUTEX_LOCKED( o ) ) {
      return sym_locked;
   } else {
      return sym_unlocked;
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_mutex_timed_lock ...                                      */
/*---------------------------------------------------------------------*/
bool_t
bglpth_mutex_timed_lock( void *m, long ms ) {
   bglpmutex_t mut = (bglpmutex_t)m;
   
#if BGL_HAVE_MUTEX_TIMEDLOCK
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

   return pthread_mutex_timedlock( &(mut->pmutex), &timeout );
#else
   int res;

   while( (res=(pthread_mutex_trylock( &mut ) == EBUSY )) && (ms > 0) ) {
      ms -= 100;
      bgl_sleep( 100 * 1000 );
   }

   return res;
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_mutex_init ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bglpth_mutex_init( obj_t m ) {
   bglpmutex_t mut = (bglpmutex_t)GC_MALLOC( sizeof( struct bglpmutex ) );

   mut->bmutex = m;
   mut->specific = BUNSPEC;

   BGL_MUTEX( m ).syslock = &pthread_mutex_lock;
   BGL_MUTEX( m ).systimedlock = &bglpth_mutex_timed_lock;
   BGL_MUTEX( m ).sysunlock = &pthread_mutex_unlock;
   BGL_MUTEX( m ).sysstate = &bglpth_mutex_state;

   BGL_MUTEX_SYSMUTEX( m ) = mut;
   BGL_MUTEX_LOCKED( m ) = 0;

   if( pthread_mutex_init( &(mut->pmutex), 0L ) )
      FAILURE( string_to_bstring( "make-mutex" ),
	       string_to_bstring( "Cannot create mutex" ),
	       string_to_bstring( strerror( errno ) ) );

   return m;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_spinlock_init ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bglpth_spinlock_init( obj_t m ) {
#if( BGL_HAVE_SPINLOCK )
   bglpspinlock_t mut = (bglpspinlock_t)GC_MALLOC( sizeof( struct bglpspinlock ) );

   mut->bmutex = m;

   BGL_MUTEX( m ).syslock = &pthread_spin_lock;
   BGL_MUTEX( m ).sysunlock = &pthread_spin_unlock;
   BGL_MUTEX( m ).systimedlock = 0;
   BGL_MUTEX( m ).sysstate = 0;

   BGL_MUTEX_SYSMUTEX( m ) = mut;
   BGL_MUTEX_LOCKED( m ) = 0;

   if( pthread_spin_init( &(mut->pmutex), 0L ) )
      FAILURE( string_to_bstring( "make-mutex" ),
	       string_to_bstring( "Cannot create mutex" ),
	       string_to_bstring( strerror( errno ) ) );

   return m;
#else
   return bglpth_mutex_init( m );
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_make_mutex ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bglpth_make_mutex( obj_t name ) {
   return bglpth_mutex_init( bgl_create_mutex( name ) );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_mutex ...                                           */
/*---------------------------------------------------------------------*/
void
bglpth_setup_mutex() {
   bgl_mutex_init_register( &bglpth_mutex_init );
}
