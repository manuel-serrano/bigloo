 /*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Posix/bglpmutex.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Sun Dec  9 02:11:43 2012 (serrano)                */
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
/*    It is mandatory that mutex does not allocate anything because    */
/*    it might happen cases where bglpth_mutex_lock is called from     */
/*    while the GC is already allocating something (because of Unix    */
/*    signals, for instance raised on process termination).            */
/*---------------------------------------------------------------------*/
#if( BGL_STRICT_SRFI18 )
static void
register_mutex( obj_t m, bglpthread_t thread ) {
   if( thread->mutexes ) {
      BGLPTH_MUTEX_BGLPMUTEX( m )->next = thread->mutexes;
      BGLPTH_MUTEX_BGLPMUTEX( thread->mutexes )->prev = m;
   }
   thread->mutexes = m;
}
#endif

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    unregister_mutex ...                                             */
/*---------------------------------------------------------------------*/
#if( BGL_STRICT_SRFI18 )
static void
unregister_mutex( obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );
   obj_t prev = mut->prev;
   obj_t next = mut->next;

   if( prev ) {
      mut->prev = 0;
      BGLPTH_MUTEX_BGLPMUTEX( prev )->next = next;
   } else {
      if( mut->thread ) {
	 mut->thread->mutexes = next;
      }
   }

   if( next ) {
      mut->next = 0;
      BGLPTH_MUTEX_BGLPMUTEX( next )->prev = prev;
   }

   mut->thread = 0;
}
#endif

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_mutex_mark_unlocked ...                                   */
/*---------------------------------------------------------------------*/
void
bglpth_mutex_mark_unlocked( obj_t m, bglpmutex_t mut ) {
   /* unregister the mutex */
#if( BGL_STRICT_SRFI18 )
   unregister_mutex( m );
#endif
   
   /* mark the mutex has free */
   mut->marked = 0;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_mutex_mark_locked ...                                     */
/*---------------------------------------------------------------------*/
void
bglpth_mutex_mark_locked( obj_t m, bglpmutex_t mut, bglpthread_t thread ) {
   if( mut->marked ) {
      if( mut->thread != thread ) {
	 FAILURE( string_to_bstring( "mutex-lock" ),
		  string_to_bstring( "mutex illegal locked" ),
		  m );
      }
   } else {
      mut->marked = 1;

      if( mut->thread != thread ) {
	 mut->thread = thread;
#if( BGL_STRICT_SRFI18 )
	 if( thread ) {
	    register_mutex( m, thread );
	 }
#endif	 
      }
   }
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
/*    bglpth_mutex_unlock_sans_thread ...                              */
/*---------------------------------------------------------------------*/
#if( BGL_STRICT_SRFI18 )   
static void
bglpth_mutex_unlock_sans_thread( obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );
   bglpthread_t thread = mut->thread;

   /* mark the Bigloo state of the lock */
   bglpth_mutex_mark_unlocked( m, mut );
   
   /* assign the thread field to mark the abandoned mutex state */
   mut->thread = thread;
   mut->marked = 0;
   
   /* physically unlock it */
   pthread_mutex_unlock( &(mut->pmutex) );
}
#endif   

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_mutexes_abandon ...                                       */
/*---------------------------------------------------------------------*/
void
bglpth_mutexes_abandon( bglpthread_t thread ) {
#if( BGL_STRICT_SRFI18 )   
   obj_t w = thread->mutexes;

   while( w ) {
      obj_t n = BGLPTH_MUTEX_BGLPMUTEX( w )->next;
      
      bglpth_mutex_unlock_sans_thread( w );
      w = n;
   }
#endif   
}
   
/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglpth_mutex_lock ...                                            */
/*---------------------------------------------------------------------*/
bool_t
bglpth_mutex_lock( obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );

   if( pthread_mutex_lock( &(mut->pmutex) ) ) {
      return 0;
   } else {
      mut->locked = 1;
      return 1;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_mutex_mark ...                                            */
/*---------------------------------------------------------------------*/
void
bglpth_mutex_mark( obj_t m ) {
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );
   fprintf( stderr, "%s:%d bglpth_mutex_mark m=%p mut=%p\n", __FILE__, __LINE__, m, mut );
   bglpth_mutex_mark_locked( m, mut, bglpth_current_pthread() );
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

   if( pthread_mutex_timedlock( &(mut->pmutex), &timeout ) ) {
      return 0;
   } else {
      mut->locked = 1;
      return 1;
   }
#else
   int res;
   bglpmutex_t mut = BGLPTH_MUTEX_BGLPMUTEX( m );
   while( (res=(pthread_mutex_trylock( &(mut->pmutex) ) == EBUSY ))
	  && (ms > 0) ) {
      ms -= 100;
      bgl_sleep( 100 * 1000 );
   }

   if( !res  ) {
      mut->locked = 1;
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
      /* unregister the mutex, must done when locked */
      bglpthread_t thread = mut->thread;

      if( mut->marked ) bglpth_mutex_mark_unlocked( m, mut );
      mut->locked = 0;
      
      /* physically unlock it */
      if( pthread_mutex_unlock( &(mut->pmutex) ) ) {
	 /* unlock has failed, re-mark as locked */
	 bglpth_mutex_mark_locked( m, mut, thread );
	 mut->locked = 1;
	 return 0;
      } else {
	 return 1;
      }
   } else {
      return 0;
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

   BGL_MUTEX( m ).syslock = &bglpth_mutex_lock;
   BGL_MUTEX( m ).sysmark = &bglpth_mutex_mark;
   BGL_MUTEX( m ).systimedlock = &bglpth_mutex_timed_lock;
   BGL_MUTEX( m ).sysunlock = &bglpth_mutex_unlock;
   BGL_MUTEX( m ).sysstate = &bglpth_mutex_state;

   BGL_MUTEX( m ).mutex = mut;
   
   if( pthread_mutex_init( &(mut->pmutex), 0L ) )
      FAILURE( string_to_bstring( "make-mutex" ),
	       string_to_bstring( "Cannot create mutex" ),
	       string_to_bstring( strerror( errno ) ) );

#if( BGL_STRICT_SRFI18 )
   mut->next = 0;
   mut->prev = 0;
#endif
   
   return m;
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
