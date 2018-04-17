/*=====================================================================*/
/*    .../bigloo/bigloo/api/pthread/src/Posix/bglpsemaphore.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  3 07:58:16 2004                          */
/*    Last change :  Tue Apr 17 08:21:57 2018 (serrano)                */
/*    Copyright   :  2004-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Posix semaphore implementation                               */
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
/*    obj_t                                                            */
/*    bgl_open_semaphore ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_semaphore( obj_t name,
		    bool_t create, bool_t excl,
		    long mode,
		    long val ) {
#if BGL_HAVE_SEMAPHORE   
   obj_t sem = GC_MALLOC( BGL_SEMAPHORE_SIZE );
   long flag = (create?O_CREAT:0) | (excl?O_EXCL:0);
   
   sem->semaphore.header = MAKE_HEADER( SEMAPHORE_TYPE, 0 );
   sem->semaphore.name = name;

   sem->semaphore.semaphore =
      sem_open( BSTRING_TO_STRING( name ), flag, mode, val );

   if( sem->semaphore.semaphore == SEM_FAILED ) {
      C_SYSTEM_FAILURE( BGL_ERROR, "open-semaphore",
			strerror( errno ),
			name );
   }
   return BREF( sem );
#else
   C_SYSTEM_FAILURE( BGL_ERROR, "open-semaphore",
		     "semaphore not supported by architecture", name );
   return 0;
#endif   
}

/* {*---------------------------------------------------------------------*} */
/* {*    long                                                             *} */
/* {*    bgl_semaphore_timed_wait ...                                     *} */
/* {*---------------------------------------------------------------------*} */
/* long                                                                */
/* bgl_semaphore_timed_wait( obj_t s, long ms ) {                      */
/*    struct timespec timeout;                                         */
/*    long usec;                                                       */
/*                                                                     */
/* #  if defined( _MINGW_VER ) || defined( _MSC_VER )                  */
/*    struct timeb tb;                                                 */
/*    ftime( &tb );                                                    */
/*                                                                     */
/*    usec = (tb.millitm + ms) * 1000;                                 */
/*                                                                     */
/*    timeout.tv_nsec = (usec % 1000000) * 1000;                       */
/*    timeout.tv_sec = tb.time + (usec / 1000000);                     */
/* #  else                                                             */
/*    struct timeval now;                                              */
/*    gettimeofday( &now, 0 );                                         */
/*                                                                     */
/*    usec = (now.tv_usec + ms * 1000);                                */
/*    timeout.tv_nsec = (usec % 1000000) * 1000;                       */
/*    timeout.tv_sec = now.tv_sec + (usec / 1000000);                  */
/* #  endif                                                            */
/*                                                                     */
/*    return sem_timedwait( BGL_SEMAPHORE_SEM( s ), &timeout );        */
/* }                                                                   */

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_semaphore_value ...                                          */
/*---------------------------------------------------------------------*/
int
bgl_semaphore_value( obj_t s ) {
#if BGL_HAVE_SEMAPHORE   
   int val;
   sem_getvalue( BGL_SEMAPHORE_SEM( s ), &val );
   return val;
#else
   return 0;
#endif   
   
}
