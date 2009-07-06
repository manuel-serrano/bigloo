/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/csleep.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  Christian Loitsch                                 */
/*    Creation    :  Fri May  2 14:34:35 2003                          */
/*    Last change :  Wed Dec 17 13:31:26 2008 (serrano)                */
/*    Copyright   :  2003-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The implementation of sleep                                      */
/*=====================================================================*/
#if defined( _MSC_VER) || defined( _MINGW_VER ) 
#  define _BGL_WIN32_VER
#endif

#include <bigloo_config.h>
#ifdef _BGL_WIN32_VER
#  include "windows.h"
#else
#  include <time.h>
#  if( BGL_HAVE_SELECT )
#    include <sys/time.h>
#    include <unistd.h>
#  endif
#  if( defined ( BGL_SLEEP ) )
#    include <unistd.h>
#  endif
#endif
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_sleep ...                                                    */
/*    -------------------------------------------------------------    */
/*    Win32 version                                                    */
/*---------------------------------------------------------------------*/
#ifdef _BGL_WIN32_VER
BGL_RUNTIME_DEF
void
bgl_sleep( long microsecs ) {
  Sleep( microsecs / 1000 );
}
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_sleep ...                                                    */
/*    -------------------------------------------------------------    */
/*    Nano version                                                     */
/*---------------------------------------------------------------------*/
#elif BGL_NANOSLEEP
void
bgl_sleep( long microsecs ) {
   if( microsecs <= 0 ) {
      return;
   } else {
      struct timespec t1;
      struct timespec t2;

      t1.tv_sec = microsecs / 1000000;
      t1.tv_nsec = ( microsecs % 1000000 ) * 1000;

      while ( nanosleep( &t1, &t2 ) && (t1.tv_sec || t1.tv_nsec) ) {
	 t1 = t2;
      }
      return;
   }
}

#elif( BGL_HAVE_SELECT )
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_sleep ...                                                    */
/*    -------------------------------------------------------------    */
/*    Select version                                                   */
/*---------------------------------------------------------------------*/
void
bgl_sleep( long microsecs ) {
   if( microsecs <= 0 ) {
      return;
   } else {
      struct timeval temps;
      fd_set empty;
      int counter;

      FD_ZERO( &empty );

      counter = microsecs / 20000;  /* 20 milliSecs */
      temps.tv_sec = 0;
      temps.tv_usec = 20000;

      while ( counter ) {
	 if (! (-1 == select ( 0, &empty, &empty, &empty, &temps )))
	    counter--;
	 temps.tv_sec = 0;
	 temps.tv_usec = 20000;
      }

      do {
	 temps.tv_sec = 0;
	 temps.tv_usec = microsecs % 20000;
      } while (-1 == select ( 0, &empty, &empty, &empty, &temps ));

      return;
   }
}

#elif( BGL_SLEEP )
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_sleep ...                                                    */
/*    -------------------------------------------------------------    */
/*    Plain version                                                    */
/*---------------------------------------------------------------------*/
void
bgl_sleep( long microsecs ) {
   if( microsecs <= 0 ) {
      return;
   } else {
      long secs = (microsecs + 999999) / 1000000;

      while ( secs ) {
	 secs = sleep( secs );
      }
   }
   return;
}

#else
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_sleep ...                                                    */
/*    -------------------------------------------------------------    */
/*    Plain version                                                    */
/*---------------------------------------------------------------------*/
void
bgl_sleep( long microsecs ) {
   return;
}
#endif
