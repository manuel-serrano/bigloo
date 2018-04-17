/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/api/srfi18/src/Posix/cmutex.c     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Tue Apr 17 08:22:31 2018 (serrano)                */
/*    Copyright   :  2004-18 Manuel Serrano                            */
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
#include <srfi18.h>

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
BGL_RUNTIME_DECL void bgl_create_mutex_register();

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
/*    register_mutex ...                                               */
/*    -------------------------------------------------------------    */
/*    It is mandatory that mutexes do not allocate anything because    */
/*    it might happen that srfi18_mutex_lock is called while the GC    */
/*    is in the middle of a collection (for instance during signal     */
/*    handling).                                                       */
/*---------------------------------------------------------------------*/
static void
register_mutex( srfi18mutex_t m, srfi18thread_t thread ) {
   if( thread->mutexes ) {
      m->next = thread->mutexes;
      thread->mutexes->prev = m;
   }
   thread->mutexes = m;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    unregister_mutex ...                                             */
/*---------------------------------------------------------------------*/
static void
unregister_mutex( srfi18mutex_t m ) {
   srfi18mutex_t prev = m->prev;
   srfi18mutex_t next = m->next;

   if( prev ) {
      m->prev = 0;
      prev->next = next;
   } else {
      if( m->thread ) {
	 m->thread->mutexes = next;
      }
   }

   if( next ) {
      m->next = 0;
      next->prev = prev;
   }

   m->thread = 0;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    srfi18_mutex_mark_unlocked ...                                   */
/*---------------------------------------------------------------------*/
void
srfi18_mutex_mark_unlocked( srfi18mutex_t m ) {
   /* unregister the mutex */
   unregister_mutex( m );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    srfi18_mutex_mark_locked ...                                     */
/*---------------------------------------------------------------------*/
void
srfi18_mutex_mark_locked( srfi18mutex_t mut, srfi18thread_t thread ) {
   obj_t o = mut->mutex.bmutex;
   
   if( mut->locked  ) {
      if( mut->thread != thread ) {
	 FAILURE( string_to_bstring( "mutex-lock" ),
		  string_to_bstring( "mutex illegally locked" ),
		  o );
      }
   } else {
      if( mut->thread != thread ) {
	 mut->thread = thread;
	 if( thread ) {
	    register_mutex( mut, thread );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    srfi18_mutex_state ...                                           */
/*---------------------------------------------------------------------*/
obj_t
srfi18_mutex_state( void *m ) {
   srfi18mutex_t mut = (srfi18mutex_t)m;
   obj_t o = mut->mutex.bmutex;

   bgl_mutex_symbols_init();
   
   if( mut->locked ) {
      if( mut->thread ) {
	 return mut->thread->bglpthread.bglthread;
      } else {
	 return sym_not_owned;
      }
   } else {
      if( mut->thread ) {
	 return sym_abandoned;
      } else {
	 return sym_not_abandoned;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    srfi18_mutex_unlock_sans_thread ...                              */
/*---------------------------------------------------------------------*/
static void
srfi18_mutex_unlock_sans_thread( srfi18mutex_t m ) {
   srfi18thread_t thread = m->thread;

   /* mark the Bigloo state of the lock */
   srfi18_mutex_mark_unlocked( m );
   m->locked = 0;
   /* assign the thread field to mark the abandoned mutex state */
   m->thread = thread;
   
   /* physically unlock it */
   pthread_mutex_unlock( &(m->mutex.pmutex) );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    srfi18_mutexes_abandon ...                                       */
/*---------------------------------------------------------------------*/
void
srfi18_mutexes_abandon( srfi18thread_t thread ) {
   srfi18mutex_t w = thread->mutexes;

   while( w ) {
      srfi18mutex_t n = w->next;

      srfi18_mutex_unlock_sans_thread( w );
      w = n;
   }
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    srfi18_mutex_lock ...                                            */
/*---------------------------------------------------------------------*/
int
srfi18_mutex_lock( void *m ) {
   srfi18mutex_t mut = (srfi18mutex_t)m;

   if( pthread_mutex_lock( m ) ) {
      return 1;
   } else {
      srfi18_mutex_mark_locked( mut, (srfi18thread_t)bglpth_current_pthread() );
      mut->locked = 1;
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    srfi18_mutex_timed_lock ...                                      */
/*---------------------------------------------------------------------*/
int
srfi18_mutex_timed_lock( void *m, long ms ) {
   srfi18mutex_t mut = (srfi18mutex_t)m;
   
#if BGL_HAVE_MUTEX_TIMEDLOCK
   struct timespec timeout;
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

   if( pthread_mutex_timedlock( m, &timeout ) ) {
      return 1;
   } else {
      srfi18_mutex_mark_locked( mut, (srfi18thread_t)bglpth_current_pthread() );
      mut->locked = 1;
      return 0;
   }
#else
   int res;
   
   while( (res=(pthread_mutex_trylock( &(mut->mutex.pmutex) ) == EBUSY ))
	  && (ms > 0) ) {
      ms -= 100;
      bgl_sleep( 100 * 1000 );
   }

   if( !res  ) {
      srfi18_mutex_mark_locked( mut, (srfi18thread_t)bglpth_current_pthread() );
   }

   return res;
#endif
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    srfi18_mutex_unlock ...                                          */
/*---------------------------------------------------------------------*/
int
srfi18_mutex_unlock( void *m ) {
   srfi18mutex_t mut = (srfi18mutex_t)m;
   srfi18thread_t thread = mut->thread;
      
   srfi18_mutex_mark_unlocked( mut );
   mut->locked = 0;
      
   /* physically unlock it */
   if( pthread_mutex_unlock( m ) ) {
      /* unlock has failed, re-mark as locked */
      srfi18_mutex_mark_locked(mut, thread );
      mut->locked = 1;
      return 1;
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    srfi18_mutex_init ...                                            */
/*---------------------------------------------------------------------*/
obj_t
srfi18_mutex_init( obj_t o ) {
   srfi18mutex_t mut =
#if( defined( BGL_INLINE_MUTEX ) )   
      (srfi18mutex_t)BGL_MUTEX_SYSMUTEX( o );
#else
      (srfi18mutex_t)GC_MALLOC( sizeof( struct srfi18mutex ) );
#endif   

   mut->mutex.bmutex = o;
   mut->mutex.specific = BUNSPEC;
   mut->locked = 0;

   BGL_MUTEX( o ).syslock = &srfi18_mutex_lock;
   BGL_MUTEX( o ).systimedlock = &srfi18_mutex_timed_lock;
   BGL_MUTEX( o ).sysunlock = &srfi18_mutex_unlock;
   BGL_MUTEX( o ).sysstate = &srfi18_mutex_state;

#if( !defined( BGL_INLINE_MUTEX ) )   
      BGL_MUTEX_SYSMUTEX( o ) = mut;
#endif
      
   if( pthread_mutex_init( &(mut->mutex.pmutex), 0L ) )
      FAILURE( string_to_bstring( "make-mutex" ),
	       string_to_bstring( "Cannot create mutex" ),
	       string_to_bstring( strerror( errno ) ) );

   mut->next = 0;
   mut->prev = 0;
   
   return o;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    srfi18_create_mutex ...                                          */
/*---------------------------------------------------------------------*/
#if( defined( BGL_INLINE_MUTEX ) )   
obj_t
srfi18_create_mutex( obj_t name ) {
   obj_t m = GC_MALLOC( BGL_MUTEX_SIZE + sizeof( struct srfi18mutex ) );

   m->mutex.header = MAKE_HEADER( MUTEX_TYPE, BGL_MUTEX_SIZE );
   m->mutex.name = name;

   return BREF( m );
}
#endif
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    srfi18_make_mutex ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
srfi18_make_mutex( obj_t name ) {
#if( defined( BGL_INLINE_MUTEX ) )
   return srfi18_mutex_init( srfi18_create_mutex( name ) );
#else   
   return srfi18_mutex_init( bgl_create_mutex( name ) );
#endif   
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    srfi18_setup_mutex ...                                           */
/*---------------------------------------------------------------------*/
void
srfi18_setup_mutex() {
   bgl_mutex_init_register( &srfi18_mutex_init );
   
 #if( defined( BGL_INLINE_MUTEX ) )   
   bgl_create_mutex_register( &srfi18_create_mutex );
#endif   
}
