/*=====================================================================*/
/*    .../project/bigloo/bigloo/api/pthread/src/Posix/bglpmutex.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Tue Apr 17 08:21:38 2018 (serrano)                */
/*    Copyright   :  2004-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Posix mutex implementation                                   */
/*=====================================================================*/
#define _GNU_SOURCE 500

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
BGL_RUNTIME_DECL void bgl_spinlock_init_register( obj_t (*)(obj_t) );
BGL_RUNTIME_DECL obj_t bgl_create_mutex( obj_t );
BGL_RUNTIME_DECL void bgl_sleep( long );

#if( defined( BGL_INLINE_MUTEX ) )
BGL_RUNTIME_DECL void bgl_create_mutex_register( obj_t (*)(obj_t) );
BGL_RUNTIME_DECL void bgl_create_spinlock_register( obj_t (*)(obj_t) );
#endif

/*---------------------------------------------------------------------*/
/*    Mutex symbols                                                    */
/*---------------------------------------------------------------------*/
static obj_t sym_locked = 0L;
static obj_t sym_unlocked = 0L;

DEFINE_STRING( pthread_backend, ___0, "pthread", sizeof( "pthread" ) );

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

   bgl_mutex_symbols_init();
   
#if BGL_POSIX_CONDV_TIMEDWAIT 
   
   if( pthread_mutex_trylock( m ) ) {
      /* try lock failed, another thread owns that lock */
      return sym_locked;
   } else {
#  if BGL_HAVE_MUTEX_RECURSIVE
      pthread_cond_t cv;
      struct timespec timeout;
      int r;
      
      timeout.tv_sec = 0;
      timeout.tv_nsec = 0;
      
      pthread_cond_init( &cv, 0L );
      
      pthread_mutex_unlock( m );
      
      r = pthread_cond_timedwait( &cv, m, &timeout );

      if( ETIMEDOUT == r ) {
         return sym_locked;
      } else {
         return sym_unlocked;
      }
#  else
      return sym_unlocked;
#  endif      
   }
   
#else
   if( mut->locked > 0 ) {
      return sym_locked;
   } else {
      return sym_unlocked;
   }
#endif   
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_mutex_timed_lock ...                                      */
/*---------------------------------------------------------------------*/
bool_t
bglpth_mutex_timed_lock( void *m, long ms ) {
   bglpmutex_t mut = (bglpmutex_t)m;
   int res;
   long usec;
   
#if BGL_HAVE_MUTEX_TIMEDLOCK
   struct timespec timeout;

#  if defined( _MINGW_VER ) || defined( _MSC_VER )
   struct timeb tb;
   ftime( &tb );

   usec = (tb.millitm + ms) * 1000;

   timeout.tv_nsec = (usec % 1000000) * 1000;
   timeout.tv_sec = tb.time + (usec / 1000000);
#  else
   clock_gettime(CLOCK_REALTIME, &timeout);

   timeout.tv_nsec += (ms % 1000) * 1000000;
   timeout.tv_sec += (ms / 1000);
#  endif

   res = pthread_mutex_timedlock( &(mut->pmutex), &timeout );
#else

   while( (res=(pthread_mutex_trylock( &(mut->pmutex) ) == EBUSY )) && (ms > 0) ) {
      ms -= 100;
      bgl_sleep( 100 * 1000 );
   }
#endif
   
#if !BGL_POSIX_CONDV_TIMEDWAIT
   if( !res )
      mut->locked++;
#endif
   return res;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bglpth_mutex_lock ...                                            */
/*---------------------------------------------------------------------*/
#if !BGL_POSIX_CONDV_TIMEDWAIT
static int
bglpth_mutex_lock( void *m ) {
   int r;
   bglpmutex_t mut = (bglpmutex_t)m;

   if( r = pthread_mutex_lock( m ) ) {
      return r;
   } else {
      mut->locked++;
      return 0;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bglpth_mutex_trylock ...                                         */
/*---------------------------------------------------------------------*/
#if !BGL_POSIX_CONDV_TIMEDWAIT
static int
bglpth_mutex_trylock( void *m ) {
   int r;
   bglpmutex_t mut = (bglpmutex_t)m;

   if( r = pthread_mutex_trylock( m ) ) {
      return r;
   } else {
      mut->locked++;
      return 0;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bglpth_mutex_unlock ...                                          */
/*---------------------------------------------------------------------*/
#if !BGL_POSIX_CONDV_TIMEDWAIT
static int
bglpth_mutex_unlock( void *m ) {
   int r;
   bglpmutex_t mut = (bglpmutex_t)m;

   mut->locked--;
   
   if( r = pthread_mutex_unlock( m ) ) {
      mut->locked++;
      return r;
   } else {
      return 0;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_mutex_init ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bglpth_mutex_init( obj_t o ) {
#if defined( BGL_HAVE_MUTEX_RECURSIVE )   
   pthread_mutexattr_t attr;
#endif
   bglpmutex_t mut =
#if( defined( BGL_INLINE_MUTEX ) )   
      (bglpmutex_t)BGL_MUTEX_SYSMUTEX( o );
#else   
      (bglpmutex_t)GC_MALLOC( sizeof( struct bglpmutex ) );
#endif
   
   mut->bmutex = o;
   mut->specific = BUNSPEC;

#if BGL_POSIX_CONDV_TIMEDWAIT
   BGL_MUTEX( o ).syslock = &pthread_mutex_lock;
   BGL_MUTEX( o ).systrylock = &pthread_mutex_trylock;
   BGL_MUTEX( o ).sysunlock = &pthread_mutex_unlock;
#else   
   BGL_MUTEX( o ).syslock = &bglpth_mutex_lock;
   BGL_MUTEX( o ).systrylock = &bglpth_mutex_trylock;
   BGL_MUTEX( o ).sysunlock = &bglpth_mutex_unlock;
#endif   
   BGL_MUTEX( o ).systimedlock = &bglpth_mutex_timed_lock;
   BGL_MUTEX( o ).sysstate = &bglpth_mutex_state;
   BGL_MUTEX( o ).backend = pthread_backend;

#if !defined( BGL_INLINE_MUTEX )
   BGL_MUTEX_SYSMUTEX( o ) = mut;
#endif   

#if defined( BGL_HAVE_MUTEX_RECURSIVE ) 
   pthread_mutexattr_init( &attr );
   pthread_mutexattr_settype( &attr, PTHREAD_MUTEX_RECURSIVE );

   if( pthread_mutex_init( &(mut->pmutex), &attr ) )
#else
   if( pthread_mutex_init( &(mut->pmutex), 0 ) )
#endif      
      FAILURE( string_to_bstring( "make-mutex" ),
	       string_to_bstring( "Cannot create mutex" ),
	       string_to_bstring( strerror( errno ) ) );

   return o;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_spinlock_init ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bglpth_spinlock_init( obj_t o ) {
#if( BGL_HAVE_SPINLOCK )
   bglpspinlock_t mut =
#if( defined( BGL_INLINE_MUTEX ) )
      (bglpspinlock_t)BGL_MUTEX_SYSMUTEX( o );
#else      
      (bglpspinlock_t)GC_MALLOC( sizeof( struct bglpspinlock ) );
#endif      

   mut->bmutex = o;

   BGL_MUTEX( o ).syslock = &pthread_spin_lock;
   BGL_MUTEX( o ).systrylock = &pthread_spin_trylock;
   BGL_MUTEX( o ).sysunlock = &pthread_spin_unlock;
   BGL_MUTEX( o ).systimedlock = 0;
   BGL_MUTEX( o ).sysstate = 0;

#if( !defined( BGL_INLINE_MUTEX ) )   
   BGL_MUTEX_SYSMUTEX( o ) = mut;
#endif   

   if( pthread_spin_init( &(mut->pmutex), 0L ) )
      FAILURE( string_to_bstring( "make-mutex" ),
	       string_to_bstring( "Cannot create mutex" ),
	       string_to_bstring( strerror( errno ) ) );

   return o;
#else
   return bglpth_mutex_init( o );
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_create_mutex ...                                          */
/*---------------------------------------------------------------------*/
#if( defined( BGL_INLINE_MUTEX ) )   
obj_t
bglpth_create_mutex( obj_t name ) {
   obj_t m = GC_MALLOC( BGL_MUTEX_SIZE + sizeof( struct bglpmutex ) );

   m->mutex.header = MAKE_HEADER( MUTEX_TYPE, BGL_MUTEX_SIZE );
   m->mutex.name = name;

   return BREF( m );
}
#endif
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_create_spinlock ...                                       */
/*---------------------------------------------------------------------*/
#if( defined( BGL_INLINE_MUTEX ) )   
obj_t
bglpth_create_spinlock( obj_t name ) {
#if( BGL_HAVE_SPINLOCK )
   obj_t m = GC_MALLOC( BGL_MUTEX_SIZE + sizeof( struct bglpspinlock ) );

   m->mutex.header = MAKE_HEADER( MUTEX_TYPE, BGL_MUTEX_SIZE );
   m->mutex.name = name;

   return BREF( m );
#else
   return bglpth_create_mutex( name );
#endif   
}
#endif
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bglpth_make_mutex ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bglpth_make_mutex( obj_t name ) {
#if( defined( BGL_INLINE_MUTEX ) )   
   return bglpth_mutex_init( bglpth_create_mutex( name ) );
#else
   return bglpth_mutex_init( bgl_create_mutex( name ) );
#endif
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_mutex ...                                           */
/*---------------------------------------------------------------------*/
void
bglpth_setup_mutex() {
   bgl_mutex_init_register( &bglpth_mutex_init );
   bgl_spinlock_init_register( &bglpth_spinlock_init );
   
#if( defined( BGL_INLINE_MUTEX ) )
   bgl_create_mutex_register( &bglpth_create_mutex );
   bgl_create_spinlock_register( &bglpth_create_spinlock );
#endif   
}
