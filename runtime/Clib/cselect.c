/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cselect.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 21 16:02:12 2017                          */
/*    Last change :  Sat Jun 24 09:37:05 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Bigloo select                                                    */
/*=====================================================================*/

#include <stddef.h>
#include <bigloo_config.h>
#include <time.h>
#if( BGL_HAVE_SELECT )
#  include <sys/time.h>
#  include <unistd.h>
#endif
#include <fcntl.h>
#include <memory.h>
#include <errno.h>
#include <bigloo.h>

#if POSIX_FILE_OPS
#  define _FILENO (int)fileno
#else
#  define _FILENO
#endif

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    objfd ...                                                        */
/*---------------------------------------------------------------------*/
static int
objfd( obj_t f, int dir ) {
   if( INPUT_PORTP( f ) ) {
      if( dir == -1 ) {
	 return _FILENO( PORT_FILE( f ) );
      }
   } else if( OUTPUT_PORTP( f ) ) {
      if( dir == 1 ) {
	 switch( OUTPUT_PORT( f ).stream_type ) {
	    case BGL_STREAM_TYPE_FD:
	       return PORT_FD( SOCKET_OUTPUT( f ) );
	    case BGL_STREAM_TYPE_FILE:
	       return _FILENO( PORT_FILE( f ) );
	 }
      }
   } else if( SOCKETP( f ) ) {
      if( dir == -1 ) {
	 return objfd( SOCKET_INPUT( f ), dir );
      } else {
	 return objfd( SOCKET_OUTPUT( f ), dir );
      }
   } else if( BGL_MMAPP( f ) ) {
      return BGL_MMAP( f ).fd;
   }

   return -1;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_select ...                                                   */
/*---------------------------------------------------------------------*/
obj_t
bgl_select( long timeo, obj_t readfs, obj_t writefs, obj_t exceptfs ) {
#if( BGL_HAVE_SELECT )
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   fd_set readfds;
   fd_set writefds;
   fd_set exnfds;

   obj_t fs;
   int fd = -1;
   int res;

   FD_ZERO( &writefds );
   FD_ZERO( &readfds );
   FD_ZERO( &exnfds );

   // read files
   for( fs = readfs; PAIRP( fs ); fs = CDR( fs ) ) {
      obj_t f = CAR( fs );
      int d = objfd( f, -1 );

      if( d > 0 ) {
	 FD_SET( d, &readfds );
	 if( d > fd ) fd = d;
      }
   }

   // write files
   for( fs = writefs; PAIRP( fs ); fs = CDR( fs ) ) {
      obj_t f = CAR( fs );
      int d = objfd( f, 1 );

      if( fd > 0 ) {
	 FD_SET( d, &writefds );
	 if( d > fd ) fd = d;
      }
   }
	 
   // except files
   for( fs = exceptfs; PAIRP( fs ); fs = CDR( fs ) ) {
      obj_t f = CAR( fs );
      int dr = objfd( f, -1 );
      int dw = objfd( f, 1 );

      if( dr > 0 ) {
	 FD_SET( dr, &exnfds );
	 if( dr > fd ) fd = dr;
      }
      if( dw > 0 ) {
	 FD_SET( dw, &exnfds );
	 if( dw > fd ) fd = dw;
      }
   }

   if( fd > FD_SETSIZE ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "select", "file too big", readfs );
   }
   
   if( timeo > 0 ) {
      struct timeval timeout;
      timeout.tv_sec = timeo / 1000000;;
      timeout.tv_usec = timeo % 1000000;

      res = select( fd + 1, &readfds, &writefds, &exnfds, &timeout );
   } else {
      res = select( fd + 1, &readfds, &writefds, &exnfds, 0 );
   }

   if( res == -1 ) {
      obj_t errobj = BNIL;
      errobj = MAKE_PAIR( exceptfs, errobj );
      errobj = MAKE_PAIR( writefs, errobj );
      errobj = MAKE_PAIR( readfs, errobj );
      errobj = MAKE_PAIR( BINT( timeo ), errobj );
      C_SYSTEM_FAILURE( BGL_ERROR, "select", strerror( errno ), errobj );
   } else if( res == 0 ) {
      BGL_ENV_MVALUES_NUMBER_SET( env, 3 );
      BGL_ENV_MVALUES_VAL_SET( env, 1, BNIL );
      BGL_ENV_MVALUES_VAL_SET( env, 2, BNIL );

      return BNIL;
   } else {
      obj_t readr = BNIL, writer = BNIL, exceptr = BNIL;

      // read files
      for( fs = readfs; PAIRP( fs ); fs = CDR( fs ) ) {
	 obj_t f = CAR( fs );
	 int fd = objfd( f, -1 );
	 if( fd > 0 && FD_ISSET( fd, &readfds ) ) {
	    readr = MAKE_PAIR( f, readr );
	 }
      }
      
      // write files
      for( fs = writefs; PAIRP( fs ); fs = CDR( fs ) ) {
	 obj_t f = CAR( fs );
	 int fd = objfd( f, -1 );
	 if( fd > 0 && FD_ISSET( fd, &writefds ) ) {
	    writer = MAKE_PAIR( f, writer );
	 }
      }
      
      // except files
      for( fs = exceptfs; PAIRP( fs ); fs = CDR( fs ) ) {
	 obj_t f = CAR( fs );
	 int fdr = objfd( f, -1 );
	 int fdw = objfd( f, 1 );
	 
	 if( fdr > 0 && FD_ISSET( fdr, &exnfds ) ) {
	    exceptr = MAKE_PAIR( f, exceptr );
	 }
	 if( fd > 0 && FD_ISSET( fd, &exnfds ) ) {
	    exceptr = MAKE_PAIR( f, exceptr );
	 }
      }
      
      BGL_ENV_MVALUES_NUMBER_SET( env, 3 );
      BGL_ENV_MVALUES_VAL_SET( env, 1, writer );
      BGL_ENV_MVALUES_VAL_SET( env, 2, exceptr );

      return readr;
   }
#else
   BGL_ENV_MVALUES_NUMBER_SET( env, 3 );
   BGL_ENV_MVALUES_VAL_SET( env, 1, BNIL );
   BGL_ENV_MVALUES_VAL_SET( env, 2, BNIL );
   
   return BNIL;
#endif
}

