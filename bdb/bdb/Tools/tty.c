/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bdb/bdb/Tools/tty.c                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Aug 12 08:11:44 1999                          */
/*    Last change :  Wed Jan  9 08:51:31 2008 (serrano)                */
/*    -------------------------------------------------------------    */
/*    TTY management.                                                  */
/*=====================================================================*/
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <termios.h>
#include <stdlib.h>

/*---------------------------------------------------------------------*/
/*    The standard bdb getchar function.                               */
/*---------------------------------------------------------------------*/
#if( defined( fgetc ) )
int my_fgetc( FILE *stream ) {
   return fgetc( stream );
}
int (*bdb_getc)(FILE *) = my_fgetc;
#else
int (*bdb_getc)(FILE *) = fgetc;
#endif

/*---------------------------------------------------------------------*/
/*    static char                                                      */
/*    tty_bufname ...                                                  */
/*---------------------------------------------------------------------*/
static char tty_bufname[ 20 ];

/*---------------------------------------------------------------------*/
/*    The file descriptors of the tty                                  */
/*---------------------------------------------------------------------*/
static int tty_fd[ 2 ];

/*---------------------------------------------------------------------*/
/*    Control characters                                               */
/*---------------------------------------------------------------------*/
static char vintr, vquit;

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    open_tty ...                                                     */
/*---------------------------------------------------------------------*/
static int
open_tty( int fd[ 2 ] ) {
   int i, c;
   struct termios term;
   
   fd[ 0 ] = fd[ 1 ] = -1;
   
   for( c = 'p'; c <= 's'; c++ ) {
      for( i = 0; i < 16; i++ ) {
	 sprintf( tty_bufname, "/dev/pty%c%x", c, i );
	 
	 if( (fd[ 0 ] = open( tty_bufname, O_RDWR|O_NONBLOCK|O_NDELAY ) ) >= 0 ) {
	    
	    sprintf( tty_bufname, "/dev/tty%c%x", c, i );
	    
	    if( (fd[ 1 ] = open( tty_bufname, O_RDWR ) ) < 0 ) {
	       /* close the master because we cannot open the slave */
	       close( fd[ 0 ] );
	       fd[ 0 ] = -1;
	       continue;
	    }
	    /* ok we have found and opened a tty */
	    /* we change the terminal behavior to get ride of CR */
      
	    if( tcgetattr( fd[ 0 ], &term ) < 0 )
	       goto err;
	    
	    term.c_oflag &= ~ONLCR;
	    term.c_iflag |= IGNCR;

	    vintr = term.c_cc[ VINTR ];
	    vquit = term.c_cc[ VQUIT ];

	    if( tcsetattr( fd[ 0 ], TCSANOW, &term ) < 0 )
	       goto err;
	    
	    return 0;
	 }
      }
   }

 err:
   /* Error, we cannot open a tty */
  if( fd[ 0 ] != -1 ) close( fd[ 0 ] );
  if( fd[ 1 ] != -1 ) close( fd[ 1 ] );
  return -1;
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    tty_init ...                                                     */
/*    -------------------------------------------------------------    */
/*    Initializes a TTY and returns the name of the opened TTY.        */
/*    Returns the empty string if failure.                             */
/*---------------------------------------------------------------------*/
char *
tty_init() {
   if( open_tty( tty_fd ) )
      return "";
   else {
      return tty_bufname;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    get_tty0 ...                                                     */
/*---------------------------------------------------------------------*/
int
get_tty0() {
   return tty_fd[ 0 ];
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    tty_read ...                                                     */
/*    -------------------------------------------------------------    */
/*    !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  */
/*    -------------------------------------------------------------    */
/*    This function is left here for the example. It is actually       */
/*    _NEVER CALLED_ because the actual reading on a tty is only       */
/*    operated on by kbdb (see the Kbdb/ttyk.c file).                  */
/*    -------------------------------------------------------------    */
/*    Tries to read something on a tty. If there is something to       */
/*    be read, return it in a NULL terminated string. Otherwise,       */
/*    returns the empty string.                                        */
/*---------------------------------------------------------------------*/
char *
tty_read() {
   static int   read_buf_len = 1024;
   static char *read_buf     = 0;
   int n, m = 0;
   int raw = read_buf_len;

   if( !read_buf ) read_buf = (char *)malloc( read_buf_len + 1 );

   while( (n = read( tty_fd[ 0 ], read_buf, raw )) == raw ) {
      m += n;
      read_buf_len += raw;
      read_buf = (char *)realloc( read_buf, read_buf_len + 1 );
   }

   if( (n + m) <= 0 ) {
      return "";
   }
   else {
      read_buf[ m + n ] = 0;
      return read_buf;
   }
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    tty_write ...                                                    */
/*---------------------------------------------------------------------*/
int
tty_write( char *str, int len ) {
   return write( tty_fd[ 0 ], str, len );
}
   
