/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/crgc.c                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Sep 13 11:58:32 1998                          */
/*    Last change :  Sun May 30 16:50:29 2010 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Rgc runtime (mostly port handling).                              */
/*=====================================================================*/
#include <stdio.h>
#include <errno.h> 
#include <string.h>
#if defined( _MSC_VER) || defined( _MINGW_VER )
#   define _BGL_WIN32_VER
#endif
#ifndef _BGL_WIN32_VER
#   include <dirent.h>
#   include <sys/file.h>
#   include <sys/time.h>
#else
#   include <io.h>
#   include <windows.h>
#endif
#if( !(defined( NeXT ) && (defined( mc68000 ) || defined( i386 ))) )
#   if HAVE_TERMIO
#      include <termio.h>
#   endif
#endif
#if !defined( sony_news ) && \
    !(defined( NeXT ) && (defined( mc68000 ) || defined( i386 ))) && \
    !defined( _MSC_VER )
#   include <unistd.h>
#endif
#include <bigloo.h>
#if( defined( sony_news ) || (defined( NeXT ) && defined( mc68000 )) )
#   include <ctype.h>
#endif
#if POSIX_FILE_OPS
#   include <unistd.h>
#endif

/*---------------------------------------------------------------------*/
/*    isascii                                                          */
/*---------------------------------------------------------------------*/
#if( !defined( isascii ) )
#   define isascii( c ) (!((c) & ~0177))
#endif

#define RGC_DEBUG
#undef RGC_DEBUG

/*---------------------------------------------------------------------*/
/*    C imports                                                        */
/*---------------------------------------------------------------------*/
extern obj_t bigloo_case_sensitive;
extern obj_t string_to_keyword( char * );
extern obj_t make_string_sans_fill( int );
extern int bgl_debug();
extern obj_t bgl_escape_C_string( unsigned char *, long, long );
extern obj_t bgl_escape_scheme_string( unsigned char *, long, long );

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    rgc_double_buffer ...                                            */
/*    -------------------------------------------------------------    */
/*    This function augments the size of a port's buffer. An error is  */
/*    raised if there is not enough room for the allocation.           */
/*---------------------------------------------------------------------*/
static void
rgc_double_buffer( obj_t port ) {
   long bufsiz = BGL_INPUT_PORT_BUFSIZ( port );

   if( bufsiz == 2 ) {
      C_SYSTEM_FAILURE( BGL_IO_READ_ERROR,
			"read",
			"Can't enlarge buffer for non bufferized port (see the user manual for details)",
			port );
   } else {
      obj_t newbuf;
#if defined( RGC_DEBUG )
      if( bgl_debug() >= 1 ) {
	 printf( "  [0m[1;34mrgc_double_buffer[0m: old bufsiz=%d -> new=%d\n", bufsiz, bufsiz * 2 );
      }
#endif
      if( !STRINGP( INPUT_PORT( port ).buf ) )
	 C_SYSTEM_FAILURE( BGL_IO_READ_ERROR,
			   "read",
			   "Can't enlarge buffer",
			   port );

      newbuf = make_string_sans_fill( bufsiz * 2 );
      memmove( &STRING_REF( newbuf, 0 ), &RGC_BUFFER_REF( port, 0 ), bufsiz );
      INPUT_PORT( port ).buf = newbuf;
   }
}
  
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    shift_buffer ...                                                 */
/*---------------------------------------------------------------------*/
static void
shift_buffer( obj_t port ) {
   long bufsize = BGL_INPUT_PORT_BUFSIZ( port );
   long bufpos = INPUT_PORT( port ).bufpos;
   long matchstart = INPUT_PORT( port ).matchstart;
   unsigned char *buffer = &RGC_BUFFER_REF( port, 0 );

   assert( bufpos > 0 );

   INPUT_PORT( port ).lastchar   = RGC_BUFFER_REF( port, matchstart - 1 );

   /* we shift the buffer left */
   memmove( (char *)&RGC_BUFFER_REF( port, 0 ),
	    (char *)&RGC_BUFFER_REF( port, matchstart ),
	    bufpos - matchstart );

   INPUT_PORT( port ).bufpos    -= matchstart;
   INPUT_PORT( port ).matchstop -= matchstart;
   INPUT_PORT( port ).forward   -= matchstart;
   INPUT_PORT( port ).matchstart = 0;
}
 
/*---------------------------------------------------------------------*/
/*    static bool_t                                                    */
/*    rgc_size_fill_buffer ...                                         */
/*---------------------------------------------------------------------*/
static bool_t
rgc_size_fill_buffer( obj_t port, char *buf, int bufpos, int size ) {
   long r;
   int fb = INPUT_PORT( port ).fillbarrier;
   
#if defined( RGC_DEBUG )
   if( bgl_debug() >= 1 ) {
      assert( bufpos >= 1 );
      assert( (bufpos + size) == BGL_INPUT_PORT_BUFSIZ( port ) );
      printf( "  [0m[1;32mrgc_size_fill_buffer[0m: %p (%s)\n", port, BSTRING_TO_STRING( PORT( port ).name ) );
      printf( "    bufpos=%d size=%d fill-barrier=%d\n", bufpos, size, fb );
      assert( size > 0 );
   }
#endif

   if( fb == 0 ) {
#if defined( RGC_DEBUG )
      if( bgl_debug() >= 1 ) {
	 printf( "    fillarrier == 0\n" );
      }
#endif
      return 0;
   }
   
   /* we start reading at BUFPOS - 1 because we have */
   /* to remove the '\0' sentinel that ends the buffer */
   if( (fb > 0) && (size > fb) ) size = fb;

   if((r = INPUT_PORT( port ).sysread( port, &buf[ bufpos - 1 ], size )) <= 0) {
      if( r == 0 )
	 INPUT_PORT( port ).eof = 1;
      else {
	 C_SYSTEM_FAILURE( BGL_IO_READ_ERROR, "read", strerror( errno ), port );
      }
   }
   
   buf[ bufpos - 1 + r ] = 0;
#if defined( RGC_DEBUG )
   if( bgl_debug() >= 1 ) {
      printf( "    sysread res=%d\n", r );
      printf( "      -> [[0m[1;%dm%s[0m]\n", 35, &buf[ bufpos - 1 ] );
   }
#endif

   if( fb > 0 )
      INPUT_PORT( port ).fillbarrier = (fb - r);
   
   bufpos += r;

   INPUT_PORT( port ).bufpos = bufpos;

   assert( INPUT_PORT( port ).bufpos <= BGL_INPUT_PORT_BUFSIZ( port ) );

#if defined( RGC_DEBUG )
   if( bgl_debug() >= 1 ) {
      printf( "    ending with\n" );
      printf( "      size=%d asize=%d forward=%d mstart=%d mstop=%d\n",
	      BGL_INPUT_PORT_BUFSIZ( port ),
	      INPUT_PORT( port ).bufpos,
	      INPUT_PORT( port ).forward, 
	      INPUT_PORT( port ).matchstart, INPUT_PORT( port ).matchstop );
      printf( "      buffer: [[0m[1;%dm%s[0m]\n", 35, buf );
   }
#endif

   if( bufpos > 0 ) {
      buf[ bufpos - 1 ] = '\0';

#if defined( RGC_DEBUG )
      if( bgl_debug() >= 1 ) {
	 printf( "      return 1\n" );
      }
#endif
      return 1;
   } else {
#if defined( RGC_DEBUG )
      if( bgl_debug() >= 1 ) {
	 printf( "      return 0\n" );
      }
#endif
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_fill_buffer ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
rgc_fill_buffer( obj_t port ) {
   long bufsize = BGL_INPUT_PORT_BUFSIZ( port );
   long bufpos = INPUT_PORT( port ).bufpos;
   long matchstart = INPUT_PORT( port ).matchstart;
   unsigned char *buf = &RGC_BUFFER_REF( port, 0 );

   if( PORT( port ).kindof == KINDOF_CLOSED ) {
      C_SYSTEM_FAILURE( BGL_IO_READ_ERROR, "read", "input-port closed", port );
   }
   
#if defined( RGC_DEBUG )
   if( bgl_debug() >= 1 ) {
      printf( "[0m[1;33mrgc_fill_buffer[0m: %p (%s)\n", port, BSTRING_TO_STRING( PORT( port ).name ) );
      printf( "  bufsize=%d bufpos=%d forward=%d mstart=%d mstop=%d\n",
	      bufsize, bufpos, INPUT_PORT( port ).forward,
	      INPUT_PORT( port ).matchstart, INPUT_PORT( port ).matchstop );
      printf( "  eof=%d mstart=%d mstop=%d bufpos=%d\n",
	      INPUT_PORT( port ).eof,
	      INPUT_PORT( port ).matchstart,
	      INPUT_PORT( port ).matchstop,
	      INPUT_PORT( port ).bufpos );
   }
#endif
   /* In every case, forward has to be unwound  */
   /* because forward has reached the sentinel  */
   INPUT_PORT( port ).forward--;

   /* an input port that has seen its eof       */
   /* cannot be filled anymore                  */
   if( INPUT_PORT( port ).eof ) {
      return 0;
   } else {
      if( bufpos < bufsize )
	 /* the buffer is not full, we fill it */
	 return rgc_size_fill_buffer( port, buf, bufpos, bufsize - bufpos );
      else {
	 if( matchstart > 0 ) {
	    shift_buffer( port );
#if defined( RGC_DEBUG )
	    if( bgl_debug() >= 1 ) {
	       printf( "  buffer shifted [%s]\n", buf );
	    }
#endif
	    bufpos = INPUT_PORT( port ).bufpos;
	    
	    return rgc_size_fill_buffer( port, buf, bufpos, bufsize - bufpos );
	 } else {
	    /* the current token is too large for the buffer */
	    /* we have to enlarge it.                        */
	    /* Note: see rgc_size_fil_buffer for other       */
	    /* enlarge_buffer                                */
	    rgc_double_buffer( port );

	    /* undo forward--                                */
	    INPUT_PORT( port ).forward++;

	    return rgc_fill_buffer( port );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_substring ...                                         */
/*    -------------------------------------------------------------    */
/*    This function makes no bound checks because these tests have     */
/*    already been performed in the grammar.                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_substring( obj_t ip, long offset, long end ) {
   long start = INPUT_PORT( ip ).matchstart;
   long len = end - offset;

#if defined( RGC_DEBUG )
   if( bgl_debug() >= 1 ) {
      printf( "buffer-substring: start=%d  stop=%d  forward=%d  bufpos=%d\n",
	      start, INPUT_PORT( ip ).matchstop,
	      INPUT_PORT( ip ).forward, INPUT_PORT( ip ).bufpos );
   }
#endif

   return string_to_bstring_len( (char *)&RGC_BUFFER_REF( ip, start + offset ),
				 len );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_escape_substring ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_escape_substring( obj_t ip, long offset, long end, bool_t strict ) {
   long start = INPUT_PORT( ip ).matchstart;
   char *s = (char *)&RGC_BUFFER_REF( ip, start );
   
   if( strict )
      return bgl_escape_scheme_string( s, offset, end );
   else 
      return bgl_escape_C_string( s, offset, end );
}

/*---------------------------------------------------------------------*/
/*    CHEAT_BUFFER_AT                                                  */
/*---------------------------------------------------------------------*/
#define CHEAT_BUFFER_AT( s ) \
   long stop  = s; \
   char bck; \
   bck = RGC_BUFFER_REF( ip, stop ); \
   RGC_BUFFER_SET( ip, stop, '\0' );

/*---------------------------------------------------------------------*/
/*    CHEAT_BUFFER                                                     */
/*---------------------------------------------------------------------*/
#define CHEAT_BUFFER() \
   CHEAT_BUFFER_AT( INPUT_PORT( ip ).matchstop )

/*---------------------------------------------------------------------*/
/*    RESTORE_BUFFER                                                   */
/*---------------------------------------------------------------------*/
#define RESTORE_BUFFER() \
   RGC_BUFFER_SET( ip, stop, bck );

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    rgc_buffer_fixnum ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
long
rgc_buffer_fixnum( obj_t ip ) {
   long res;
   
   CHEAT_BUFFER();
   
   res = atol( (const char *)&RGC_BUFFER_REF( ip, INPUT_PORT(ip).matchstart ) );
   
   RESTORE_BUFFER();
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_bignum ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
rgc_buffer_bignum( obj_t ip ) {
   long start = INPUT_PORT( ip ).matchstart;
   obj_t res;
   
   CHEAT_BUFFER();
   
   res = bgl_string_to_bignum( &RGC_BUFFER_REF( ip, start ), 10 );
   
   RESTORE_BUFFER();
   
   return res;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_integer ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_integer( obj_t ip ) {
   /* code relies on the fact that maxvals are <= than -minvals        */
   /* generally we have maxval = -minval - 1 (for fx, elong and llong).*/
   long stop = INPUT_PORT( ip ).matchstop;
   long start = INPUT_PORT( ip ).matchstart;
   long res = 0;
   int sign = +1;
   long maxvalfx = LONG_MAX >> TAG_SHIFT;
   long maxvalelong = LONG_MAX;
   BGL_LONGLONG_T maxvalllong = BGL_LONGLONG_MAX;

   /* the sign */
   if( RGC_BUFFER_REF( ip, start ) == '+' ) {
      start++;
   } else {
      if( RGC_BUFFER_REF( ip, start ) == '-' ) {
	 start++;
	 sign = -1;
      }
   }

   /* skip the 0 padding */
   while( (start < stop) && RGC_BUFFER_REF( ip, start ) == '0' )
      start++;

   /* the real number */
   while( start < stop ) {
      char current = RGC_BUFFER_REF( ip, start ) - '0';

      /* a more accurate, but slower test would be:
       * if (res > (maxvalfx / 10 - current)) goto llong; */
      if ( res > (maxvalelong / 10 - 9) ) goto llong;

      res = (res * 10) + current;
      start++;
   }

   if( res <= maxvalfx )
      return BINT( res * sign );
   else
      return make_belong( res * sign );
      
llong:
   {
      BGL_LONGLONG_T lres = (BGL_LONGLONG_T)res;

      while( start < stop ) {
	 char current = RGC_BUFFER_REF( ip, start ) - '0';

	 /* a more accurate, but slower test would be:
	  * if (res > (maxvalllong / 10 - current)) return ...; */
	 if ( lres > (maxvalllong / 10 - 9) )
	    return rgc_buffer_bignum( ip );
	 
	 lres = lres * 10 + current;
	 start++;
      }
      return make_bllong( lres * sign );
   }
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    rgc_buffer_flonum ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
double
rgc_buffer_flonum( obj_t ip ) {
   double res;
   
   CHEAT_BUFFER();
  
   res = strtod( (const char *)&RGC_BUFFER_REF(ip, INPUT_PORT(ip).matchstart), 0 );
   
   RESTORE_BUFFER();
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_symbol ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_symbol( obj_t ip ) {
   unsigned char *aux;
   obj_t sym;
   long start = INPUT_PORT( ip ).matchstart;
   
   CHEAT_BUFFER();
   
   aux = &RGC_BUFFER_REF( ip, start );
   
   sym = string_to_symbol( (char *)aux );

   RESTORE_BUFFER();
   
   return sym;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_subsymbol ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_subsymbol( obj_t ip, long offset, long end ) {
   unsigned char *aux;
   obj_t sym;
   long start = INPUT_PORT( ip ).matchstart;
   long len = end - offset;
   
   CHEAT_BUFFER_AT( start + len );
   
   aux = &RGC_BUFFER_REF( ip, start + offset );
   
   sym = string_to_symbol( (char *)aux );

   RESTORE_BUFFER();
   
   return sym;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_upcase_symbol ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_upcase_symbol( obj_t ip ) {
   unsigned char *aux;
   obj_t sym;
   long start = INPUT_PORT( ip ).matchstart;
   unsigned char *walk;
   
   CHEAT_BUFFER();
   
   aux = &RGC_BUFFER_REF( ip, start );
   
   for( walk = aux; *walk; walk++ )
      if( isascii( *walk ) )
	 *walk = toupper( *walk );

   sym = string_to_symbol( (char *)aux );

   RESTORE_BUFFER();
   
   return sym;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_downcase_symbol ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_downcase_symbol( obj_t ip ) {
   unsigned char *aux;
   obj_t sym;
   long start = INPUT_PORT( ip ).matchstart;
   unsigned char *walk;
   
   CHEAT_BUFFER();
   
   aux = &RGC_BUFFER_REF( ip, start );
   
   for( walk = aux; *walk; walk++ )
      if( isascii( *walk ) )
	 *walk = tolower( *walk );

   sym = string_to_symbol( (char *)aux );

   RESTORE_BUFFER();
   
   return sym;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_keyword ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_keyword( obj_t ip ) {
   unsigned char *aux;
   obj_t key;
   long start = INPUT_PORT( ip ).matchstart;
   long cheat;
   
   aux = &RGC_BUFFER_REF( ip, start );

   if( *aux == ':' ) {
      aux++;
      cheat = INPUT_PORT( ip ).matchstop;
   } else {
      cheat = INPUT_PORT( ip ).matchstop - 1;
   }

   {
      CHEAT_BUFFER_AT( cheat );
      key = string_to_keyword( (char *)aux );
      RESTORE_BUFFER();
   }

   return key;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_downcase_keyword ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_downcase_keyword( obj_t ip ) {
   unsigned char *aux;
   obj_t key;
   long start = INPUT_PORT( ip ).matchstart;
   long cheat;
   unsigned char *walk;
   
   aux = &RGC_BUFFER_REF( ip, start );

   if( *aux == ':' ) {
      aux++;
      cheat = INPUT_PORT( ip ).matchstop;
   } else {
      cheat = INPUT_PORT( ip ).matchstop -1;
   }

   {
      CHEAT_BUFFER_AT( cheat );
      
      for( walk = aux; *walk; walk++ )
	 if( isascii( *walk ) ) *walk = tolower( *walk );

      key = string_to_keyword( (char *)aux );
      
      RESTORE_BUFFER();
   }

   return key;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_upcase_keyword ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
rgc_buffer_upcase_keyword( obj_t ip ) {
   unsigned char *aux;
   obj_t key;
   long start = INPUT_PORT( ip ).matchstart;
   long cheat;
   unsigned char *walk;
   
   aux = &RGC_BUFFER_REF( ip, start );

   if( *aux == ':' ) {
      aux++;
      cheat = INPUT_PORT( ip ).matchstop;
   } else {
      cheat = INPUT_PORT( ip ).matchstop -1;
   }

   {
      CHEAT_BUFFER_AT( cheat );
	 
      for( walk = aux; *walk; walk++ )
	 if( isascii( *walk ) ) *walk = toupper( *walk );

      key = string_to_keyword( (char *)aux );

      RESTORE_BUFFER();
   }

   return key;
}
 
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    rgc_buffer_unget_char ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
int
rgc_buffer_unget_char( obj_t ip, int c ) {
   INPUT_PORT( ip ).filepos--;
   
   if( INPUT_PORT( ip ).matchstop > 0 ) {
      INPUT_PORT( ip ).matchstop--;
   } else {
      RGC_BUFFER_REF( ip , 0 ) = c;
      if( INPUT_PORT( ip ).bufpos == 0 ) {
	 INPUT_PORT( ip ).bufpos = 1;
	 RGC_BUFFER_SET( ip, 1, '\0' );
      }
   }

   return c;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    reserve_space ...                                                */
/*---------------------------------------------------------------------*/
static void
reserve_space( obj_t port, long amount ) {
   long bufsize = BGL_INPUT_PORT_BUFSIZ( port );
   long bufpos = INPUT_PORT( port ).bufpos;
   long matchstop = INPUT_PORT( port ).matchstop;
   unsigned char *buffer = &RGC_BUFFER_REF( port, 0 );

   if( matchstop >= amount ) return;

   if( (matchstop+(bufsize-(bufpos-1))) >= amount ) {
      long diff = amount - matchstop;

      /* we shift the buffer to the right */
      memmove( (char *)&RGC_BUFFER_REF( port, amount ),
	       (char *)&RGC_BUFFER_REF( port, matchstop ),
	       bufpos-1 - matchstop );

      RGC_BUFFER_SET( port, bufpos+diff-1, 0 );

      INPUT_PORT( port ).bufpos    += diff;
      INPUT_PORT( port ).matchstop += diff;
   } else {
      rgc_double_buffer( port );
      reserve_space( port, amount );
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_insert_substring ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
rgc_buffer_insert_substring( obj_t ip, obj_t str, long from, long to ) {
   long len = to - from;
   long matchstop;

   if( BGL_INPUT_PORT_BUFSIZ( ip ) == 2 )
      /* unbuffered port */
      return 0;
   
   if( (long)PORT( ip ).kindof == (long)KINDOF_CLOSED ) return 0;

   if( from >= to ) return 1;

   reserve_space( ip, len );

   matchstop = INPUT_PORT( ip ).matchstop;

   /* we insert the given buffer */
   memmove( (char *)&RGC_BUFFER_REF( ip, (matchstop - len) ),
	    &STRING_REF(str, from),
	    len );
   
   if( INPUT_PORT( ip ).filepos >= len )
      INPUT_PORT( ip ).filepos -= len;
   else
      INPUT_PORT( ip ).filepos = 0;

   matchstop = INPUT_PORT( ip ).matchstop - len;
   INPUT_PORT( ip ).matchstop  = matchstop;
   INPUT_PORT( ip ).forward = matchstop;
   INPUT_PORT( ip ).matchstart = matchstop;

   return 1;
}   

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_insert_char ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
rgc_buffer_insert_char( obj_t ip, int c) {
   long matchstop;

   if( BGL_INPUT_PORT_BUFSIZ( ip ) == 2 )
      /* unbuffered port */
      return 0;
   
   if( (long)PORT( ip ).kindof == (long)KINDOF_CLOSED ) return 0;

   reserve_space( ip, 1 );

   matchstop = INPUT_PORT( ip ).matchstop;

   /* we insert the given buffer */
   RGC_BUFFER_SET( ip, matchstop - 1, c);

   if( INPUT_PORT( ip ).filepos >= 1 )
      INPUT_PORT( ip ).filepos--;
   else
      INPUT_PORT( ip ).filepos = 0;

   matchstop = INPUT_PORT( ip ).matchstop - 1;
   INPUT_PORT( ip ).matchstop  = matchstop;
   INPUT_PORT( ip ).forward = matchstop;
   INPUT_PORT( ip ).matchstart = matchstop;

   return 1;
}   

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_bol_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Is the matchstart position located at the beginning of a line?   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
rgc_buffer_bol_p( obj_t ip ) {
#if( defined( RGC_DEBUG ) )
   if( bgl_debug() >= 1 ) {
      printf( "RGC_BUFFER_BOL_P: mstart: %d  [mstart]: %d  lastchar: %d  --> %d\n",
	      INPUT_PORT( ip ).matchstart, 
	      RGC_BUFFER_REF( ip, INPUT_PORT( ip ).matchstart - 1 ),
	      INPUT_PORT( ip ).lastchar,
	      INPUT_PORT( ip ).lastchar == '\n' );
   }
#endif
   
   if( INPUT_PORT( ip ).matchstart > 0 )
      return RGC_BUFFER_REF( ip, INPUT_PORT( ip ).matchstart - 1 ) == '\n';
   else
      return INPUT_PORT( ip ).lastchar == '\n';
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_eol_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Does the buffer contain, at its first non match position, a `\n' */
/*    character?                                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
rgc_buffer_eol_p( obj_t ip ) {
   int c = RGC_BUFFER_GET_CHAR( ip );
#if( defined( RGC_DEBUG ) )   
   long f = INPUT_PORT( ip ).forward;
#endif
   
#if( defined( RGC_DEBUG ) )   
   if( bgl_debug() >= 1 ) {
      printf( "RGC_BUFFER_EOL_P: forward: %d %d", f, c );
   }
#endif
   
   if( !c ) {
      if( !RGC_BUFFER_EMPTY( ip ) ) {
	 INPUT_PORT( ip ).forward--;
	 
#if( defined( RGC_DEBUG ) )   
	 if( bgl_debug() >= 1 ) {
	    puts( "   not empty --> 0" );
	 }
#endif
	 return 0;
      }

      if( PORT( ip ).kindof == KINDOF_CONSOLE ) {
#if( defined( RGC_DEBUG ) )   
	 if( bgl_debug() >= 1 ) {
	    puts( "  kindof == CONSOLE --> 1" );
	 }
#endif
	 return 1;
      }
      if( rgc_fill_buffer( ip ) )
	 return rgc_buffer_eol_p( ip );
      else {
#if( defined( RGC_DEBUG ) )   
	 if( bgl_debug() >= 1 ) {
	    puts( "   not rgc_fill_buffer --> 0" );
	 }
#endif
	 return 0;
      }
   } else {
      INPUT_PORT( ip ).forward--;
#if( defined( RGC_DEBUG ) )   
      if( bgl_debug() >= 1 ) {
	 printf( "   --> %d\n", c == '\n' );
      }
#endif
      return c == '\n';
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_bof_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Is the match position at the beginning of the file?              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
rgc_buffer_bof_p( obj_t ip ) {
   return INPUT_PORT( ip ).filepos == 0;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_eof_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Is the input port at its end-of-file position?                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
rgc_buffer_eof_p( obj_t ip ) {
   long f = INPUT_PORT( ip ).forward;
   long p = INPUT_PORT( ip ).bufpos;
   long s = BGL_INPUT_PORT_BUFSIZ( ip );

#if defined( RGC_DEBUG )
   if( bgl_debug() >= 1 ) {
      printf( "rgc_bufffer_eof f=%d s=%d p=%d\n", f, s, p );
   }
#endif
   
   return (f >= s) || (RGC_BUFFER_PEEK_CHAR( ip ) == 0) && (f == (p - 1));
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    file_charready ...                                               */
/*---------------------------------------------------------------------*/
static int
file_charready( FILE *f ) {
#ifndef _BGL_WIN32_VER
#   if( BGL_HAVE_SELECT )
       fd_set readfds;
       struct timeval timeout;
       int fno = fileno( f ) + 1;

       FD_ZERO( &readfds );
       FD_SET( fileno( f ), &readfds );
       timeout.tv_sec = 0; timeout.tv_usec = 0;

       return (select( fno, &readfds, NULL, NULL, &timeout ) > 0);
#   else
       return 0;
#   endif
#else
    HANDLE hFile = (HANDLE)_get_osfhandle( _fileno( f ) );

    return ((WaitForSingleObject( hFile, 0) == WAIT_OBJECT_0) ? 1 : 0);
#endif
}

/*---------------------------------------------------------------------*/
/*    boot_t                                                           */
/*    bgl_rgc_charready ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_rgc_charready( obj_t port ) {
   switch( (long)PORT( port ).kindof ) {
      case (long)KINDOF_CLOSED:
	 return 0;
	 
      case (long)KINDOF_STRING:
	 return ((INPUT_PORT( port ).forward+1) < INPUT_PORT( port ).bufpos);
	 
      case (long)KINDOF_FILE:
	 return ((INPUT_PORT( port ).forward+1) < INPUT_PORT( port ).bufpos)
	    || (!feof( (FILE *)PORT( port ).stream )
		&& !INPUT_PORT( port ).eof);
	 
      case (long)KINDOF_PROCPIPE:
      case (long)KINDOF_PIPE:
      case (long)KINDOF_CONSOLE:
      case (long)KINDOF_SOCKET:
	 return ((INPUT_PORT( port ).forward+1) < INPUT_PORT( port ).bufpos)
	    || file_charready( PORT( port ).stream );
	 
      case (long)KINDOF_PROCEDURE:
      case (long)KINDOF_GZIP:
	 /* to know if a char is available we only could call the procedure */
	 /* this could block, so we just return true                        */
	 return 1;

      default:
	 return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_rgc_blit_string ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_rgc_blit_string( obj_t p, char *s, long o, long l ) {
   int bsz = BGL_INPUT_PORT_BUFSIZ( p );

   if( INPUT_PORT_CLOSEP( p ) ) {
      C_SYSTEM_FAILURE( BGL_IO_CLOSED_ERROR,
			"rgc-blit-string",
			"input-port closed",
			p );
   }
   
   RGC_START_MATCH( p );

   if( bsz == 2 ) {
      /* we are reading from a non bufferized port, we have to read */
      /* each character at a time. */
      int i;

#if defined( RGC_DEBUG )
      if( bgl_debug() >= 1 ) {
	 printf( "~~~~~ rgc_blit_string (%p) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n", p );
	 printf( "eof=%d  mstart=%d  mstop=%d bufpos=%d\n",
		 INPUT_PORT( p ).eof,
		 INPUT_PORT( p ).matchstart,
		 INPUT_PORT( p ).matchstop,
		 INPUT_PORT( p ).bufpos );
      }
#endif
      
      if( RGC_BUFFER_EMPTY( p ) && INPUT_PORT( p ).eof )
	 return 0;
      
      for( i = 0; i < l; i++ ) {
	 char c;

	 if( !(c = RGC_BUFFER_GET_CHAR( p )) ) {
	    rgc_fill_buffer( p );
	    if( !(c = RGC_BUFFER_GET_CHAR( p ) ) ) {
	       RGC_STOP_MATCH( p );
	       return i;
	    }
	 }

	 RGC_STOP_MATCH( p );
	 s[ o + i ] = c;
      }
      s[ o + i ] = 0;

      return l;
   } else {
      int o0 = o;
      int bufl = INPUT_PORT( p ).bufpos - INPUT_PORT( p ).matchstart - 1;
      int ml = ((l <= (bufl - o)) ? l : (bufl - o));

      if( ml > 0 ) {
	 memmove( &s[ o ],
		  &RGC_BUFFER_REF( p, INPUT_PORT( p ).matchstart ),
		  ml );
	 
	 INPUT_PORT( p ).forward = INPUT_PORT( p ).matchstart + ml;
	 RGC_STOP_MATCH( p );
	 RGC_SET_FILEPOS( p );
	 RGC_START_MATCH( p );
      }

      if( ml == l ) {
	 return l;
      } else {
	 if( (ml == 0) && (INPUT_PORT( p ).eof) ) {
	    return 0;
	 } else {
	    /* adjust the destination string position and length */
	    o += ml;
	    l -= ml;

	    while( (l > 0) && !(INPUT_PORT( p ).eof) ) {
	       int m = (bsz <= l ? bsz : l);
	       int r; 
	    
	       rgc_size_fill_buffer( p, &s[ o ], 1, m );
	       r = INPUT_PORT( p ).bufpos - 1;

	       INPUT_PORT( p ).filepos += r;
	    
	       l -= r;
	       o += r;

/* 	    {* we have failed to read m characters *}                  */
/* 	    if( r < m ) break;                                         */
	       /* MS 28 march 2006: we cannot read more */
	       if( r <= 0 ) break;
	    }

	    /* the following settings replace the call to RGC_STOP_MATCH */
	    INPUT_PORT( p ).matchstart = 0;
	    INPUT_PORT( p ).matchstop = 0;
	    INPUT_PORT( p ).bufpos = 1;
	    INPUT_PORT( p ).lastchar = '\n';
	    RGC_BUFFER_SET( p, 0, '\0' );

	    return o - o0;
	 }
      }
   }
}
 
