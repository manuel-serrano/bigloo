/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cwriter.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec 17 09:44:20 1991                          */
/*    Last change :  Tue Dec  4 00:26:10 2018 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Object (that have to be non recursives) printing.                */
/*=====================================================================*/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern obj_t c_constant_string_to_string( char *c_string );
extern obj_t llong_to_string( BGL_LONGLONG_T x, long radix );
extern obj_t bgl_bignum_to_string( obj_t x, int radix );
extern obj_t bgl_write_obj( obj_t, obj_t );
extern obj_t bgl_display_obj( obj_t, obj_t );
extern obj_t bgl_write( obj_t, unsigned char *, size_t );
extern obj_t bgl_output_flush_char( obj_t, char );
extern obj_t bgl_output_flush( obj_t, char *, size_t );
   
/*---------------------------------------------------------------------*/
/*    Les noms des caracateres                                         */
/*---------------------------------------------------------------------*/
static char *char_name[] = {
   "","","","","","","","",
   "",  "tab", "newline", "", "", "return", "", "",
   "", "","","","","","","",
   "", "", "","","", "", "", "",
   "space", "!", "\"","#","$","%","&","'",
   "(", ")", "*", "+", ",", "-", ".", "/",
   "0", "1", "2", "3", "4", "5", "6", "7",
   "8", "9", ":", ";", "<", "=", ">", "?",
   "@", "A", "B", "C", "D", "E", "F", "G",
   "H", "I", "J", "K", "L", "M", "N", "O",
   "P", "Q", "R", "S", "T", "U", "V", "W",
   "X", "Y", "Z", "[", "\\", "]", "^", "_",
   "`", "a", "b", "c", "d", "e", "f", "g",
   "h", "i", "j", "k", "l", "m", "n", "o",
   "p", "q", "r", "s", "t", "u", "v", "w",
   "x", "y", "z", "{", "|", "}", "~", ""
};

/*---------------------------------------------------------------------*/
/*    GCC branch prediction                                            */
/*---------------------------------------------------------------------*/
#if  __GNUC__ >= 3
#  define IF_EXPECT( expr ) if( __builtin_expect( (expr), 1 ) )
#  define IF_UNEXPECT( expr ) if( __builtin_expect( (expr), 0 ) )
#else
#  define IF_EXPECT( expr ) if( expr )
#  define IF_UNEXPECT( expr ) if( expr )
#endif

/*---------------------------------------------------------------------*/
/*    PUTC ...                                                         */
/*---------------------------------------------------------------------*/
#define PUTC( op, c ) {					               \
   IF_UNEXPECT( OUTPUT_PORT( op ).ptr >= OUTPUT_PORT( op ).end ) {     \
      bgl_output_flush_char( op, c );                                  \
   } else {                                                            \
      *OUTPUT_PORT( op ).ptr++ = c;				       \
   }                                                                   \
 }

/*---------------------------------------------------------------------*/
/*    PUTS ...                                                         */
/*    -------------------------------------------------------------    */
/*    This assumes than strings do not contain \n character.           */
/*---------------------------------------------------------------------*/
#define PUTS( op, s )						       \
   if( OUTPUT_PORT( op ).ptr + (sizeof(s)-1) < OUTPUT_PORT( op ).end) {\
      memcpy( OUTPUT_PORT( op ).ptr, s, (sizeof(s)-1) );               \
      OUTPUT_PORT( op ).ptr += (sizeof(s)-1);		               \
   } else {							       \
      bgl_output_flush( op, s, (sizeof(s)-1) );	              	       \
   } 

/*---------------------------------------------------------------------*/
/*    PRINTF ...                                                       */
/*---------------------------------------------------------------------*/
#ifdef __GNUC__
#  define _new( v, s ) v[ s ]
#else
#  define *_new = alloca( s )
#endif

#define PRINTF1( op, sz, fmt, arg0 ) \
   if( BGL_OUTPUT_PORT_CNT( op ) > sz ) { \
      int n = sprintf( OUTPUT_PORT( op ).ptr, fmt, arg0 ); \
      OUTPUT_PORT( op ).ptr += n; \
   } else { \
      char _new( __buf, sz  ); \
      int n = sprintf( __buf, fmt, arg0 ); \
      bgl_output_flush( op, __buf, n ); \
   }

#define PRINTF2( op, sz, fmt, arg0, arg1 ) \
   if( BGL_OUTPUT_PORT_CNT( op ) > sz ) { \
      int n = sprintf( OUTPUT_PORT( op ).ptr, fmt, arg0, arg1 ); \
      OUTPUT_PORT( op ).ptr += n; \
   } else { \
      char _new( __buf, sz ); \
      int n = sprintf( __buf, fmt, arg0, arg1 ); \
      bgl_output_flush( op, __buf, n ); \
   }

#define PRINTF3( op, sz, fmt, arg0, arg1, arg2 ) \
   if( BGL_OUTPUT_PORT_CNT( op ) > sz ) { \
      int n = sprintf( OUTPUT_PORT( op ).ptr, fmt, arg0, arg1, arg2 ); \
      OUTPUT_PORT( op ).ptr += n; \
   } else { \
      char _new( __buf, sz ); \
      int n = sprintf( __buf, fmt, arg0, arg1, arg2 ); \
      bgl_output_flush( op, __buf, n ); \
   }

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_substring ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_display_substring( obj_t o, long start, long end, obj_t op ) {
   obj_t res;
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   res = bgl_write( op, &STRING_REF( o, start ), end - start );
   BGL_MUTEX_UNLOCK( mutex );

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_string ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_display_string( obj_t o, obj_t op ) {
   obj_t res;
   obj_t mutex = OUTPUT_PORT( op ).mutex;

   BGL_MUTEX_LOCK( mutex );
   res = bgl_write( op, &STRING_REF( o, 0 ), STRING_LENGTH( o ) );
   BGL_MUTEX_UNLOCK( mutex );

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_string ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_string( obj_t o, bool_t esc, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   if( esc ) PUTC( op, '#' );
   
   PUTC( op, '"' );
   bgl_write( op, &STRING_REF( o, 0 ), STRING_LENGTH( o ) );
   PUTC( op, '"' );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_fixnum ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_display_fixnum( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 32, "%ld", CINT( o ) );
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_elong ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_elong( long o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 32, "%ld", o );
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_elong ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_elong( long o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 32, "#e%ld", o );
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_llong ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_llong( BGL_LONGLONG_T o, obj_t op ) {
   return bgl_display_string( llong_to_string( o, 10 ), op );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_llong ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_llong( BGL_LONGLONG_T o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   obj_t s = llong_to_string( o, 10 );
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#l" );
   bgl_write( op, &STRING_REF( s, 0 ), STRING_LENGTH( s ) );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_bignum ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_bignum( obj_t o, obj_t op ) {
   return bgl_display_string( bgl_bignum_to_string( o, 10 ), op );
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_bignum ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_bignum( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   obj_t s = bgl_bignum_to_string( o, 10 );
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#z" );
   bgl_write( op, &STRING_REF( s, 0 ), STRING_LENGTH( s ) );
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_char ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_display_char( char c, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;

   BGL_MUTEX_LOCK( mutex );
   
   PUTC( op, c );
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_char ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_char( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   int c = CCHAR( o );
   
   BGL_MUTEX_LOCK( mutex );
   
   if( (c > 0) && (c < 128) && char_name[ c ][ 0 ] ) {
      char *name = char_name[ c ];
	 
      PUTC( op, '#' );
      PUTC( op, '\\' );
      bgl_write( op, (unsigned char *)name, strlen( name ) );
   } else {
      PUTC( op, '#' );
      PUTC( op, '\\' );
      PUTC( op, 'x' );

      PRINTF1( op, 3, "%02x", (unsigned char)(c) );
   }
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_ucs2 ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_ucs2( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 7, "#u%04x", CUCS2( o ) );
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}   

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_ucs2 ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_ucs2( obj_t o, obj_t op ) {
   ucs2_t ch = CUCS2( o );
   
   if( ch < (ucs2_t)256 ) {
      obj_t mutex = OUTPUT_PORT( op ).mutex;
      BGL_MUTEX_LOCK( mutex );
      PUTC( op, ch );
      BGL_MUTEX_UNLOCK( mutex );
   
      return op;
   } else
      return bgl_write_ucs2( o, op );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_ucs2string ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_ucs2string( obj_t o, obj_t op ) {
   int len  = UCS2_STRING_LENGTH( o );
   ucs2_t *ucs2 = BUCS2_STRING_TO_UCS2_STRING( o );
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   int i;
   
   BGL_MUTEX_LOCK( mutex );
   
   for( i = 0; i < len; i++ ) {
      ucs2_t ch = ucs2[ i ];
	 
#if( UCS2_DISPLAYABLE )
#else
      if( ch < (ucs2_t)256 )
	 PUTC( op, (char)ch );
#endif
   }
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_utf8string ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_utf8string( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#u\"" );
   bgl_write( op, &STRING_REF( o, 0 ), STRING_LENGTH( o ) );
   PUTC( op, '"' );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_opaque ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_opaque( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF2( op, 40, "#<opaque:%ld:%08lx>", TYPE( o ), (unsigned long)o );
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_cnst ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_cnst( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 8, "#<%04x>", (int)CCNST( o ) );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_procedure ...                                          */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_procedure( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;

   BGL_MUTEX_LOCK( mutex );
   
   PRINTF2( op, 96,
	    "#<procedure:%lx.%ld>",
	    VA_PROCEDUREP( o ) ?
	    (unsigned long)PROCEDURE_VA_ENTRY( o ) :
	    (unsigned long)PROCEDURE_ENTRY( o ),
	    (long)PROCEDURE( o ).arity );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_output_port ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_output_port( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 20 + STRING_LENGTH( PORT( o ).name ),
	    "#<output_port:%s>",
	    BSTRING_TO_STRING( PORT( o ).name ) );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_input_port ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_input_port( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#<input_port:" );
   
   BGL_MUTEX_UNLOCK( mutex );
   bgl_display_obj( PORT( o ).name, op );
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 10, ".%ld>", (long)BGL_INPUT_PORT_BUFSIZ( o ) );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_binary_port ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_binary_port( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF2( op, 40 + STRING_LENGTH( BINARY_PORT( o ).name ),
	    "#<binary_%s_port:%s>",
	    BINARY_PORT_INP( o ) ? "input" : "output",
	    BSTRING_TO_STRING( BINARY_PORT( o ).name ) );
   
   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_foreign ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_foreign( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#<foreign:" );
   
   BGL_MUTEX_UNLOCK( mutex );
   bgl_display_obj( FOREIGN_ID( o ), op );
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 16, ":%lx>", (long)FOREIGN_TO_COBJ( o ) );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_dynamic_env ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_dynamic_env( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#<dynamic-env:" );
   PRINTF1( op, 16, ":%p>", o );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_process ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_process( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#<process:" );
   PRINTF1( op, 20, "%d>", PROCESS_PID( o ) );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_socket ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_socket( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   if( BGL_SOCKET_UNIXP( o ) ) {
      PRINTF1( op,
	       40 + (STRINGP( SOCKET( o ).hostname ) ?
		     STRING_LENGTH( SOCKET( o ).hostname ) :
		     sizeof( "localhost" )),
	       "#<unix-socket:%s>",
	       STRINGP( SOCKET( o ).hostname ) ?
	       BSTRING_TO_STRING( SOCKET( o ).hostname ) :
	       "localhost" );
   } else {
      PRINTF3( op,
	       48 + (STRINGP( SOCKET( o ).hostname ) ?
		     STRING_LENGTH( SOCKET( o ).hostname ) :
		     sizeof( "localhost" )),
	       "#<socket:%s.%d.%lx>",
	       STRINGP( SOCKET( o ).hostname ) ?
	       BSTRING_TO_STRING( SOCKET( o ).hostname ) :
	       "localhost",
	       SOCKET( o ).portnum,
	       (long)o );
   }

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_datagram_socket ...                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_datagram_socket( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF2( op,
	    40 + (STRINGP( BGL_DATAGRAM_SOCKET( o ).hostname ) ?
		  STRING_LENGTH( BGL_DATAGRAM_SOCKET( o ).hostname ) :
		  sizeof( "localhost" )),
	    "#<datagram-socket:%s.%d>",
	    STRINGP( BGL_DATAGRAM_SOCKET( o ).hostname ) ?
	    BSTRING_TO_STRING( BGL_DATAGRAM_SOCKET( o ).hostname ) :
	    "localhost",
	    BGL_DATAGRAM_SOCKET( o ).portnum );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_regexp ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_regexp( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op,
	    11 + STRING_LENGTH( BGL_REGEXP( o ).pat ),
	    "#<regexp:%s>",
	    BSTRING_TO_STRING( BGL_REGEXP( o ).pat ) );
   BGL_MUTEX_UNLOCK( mutex );
   
   return o;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_mmap ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_mmap( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#<mmap:" );
   
   BGL_MUTEX_UNLOCK( mutex );
   bgl_display_obj( BGL_MMAP( o ).name, op );
   BGL_MUTEX_LOCK( mutex );
   
   PRINTF1( op, 16, ":%ld>", (long)BGL_MMAP( o ).length );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_semaphore ...                                          */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_semaphore( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, "#<semaphore:" );
   
   BGL_MUTEX_UNLOCK( mutex );
   bgl_display_obj( BGL_SEMAPHORE( o ).name, op );
   BGL_MUTEX_LOCK( mutex );
   
   PUTS( op, ">" );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_custom ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_custom( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   CUSTOM_OUTPUT( o )( o, op );

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_unknown ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_unknown( obj_t o, obj_t op ) {
   obj_t mutex = OUTPUT_PORT( op ).mutex;
   
   BGL_MUTEX_LOCK( mutex );
   
   if( POINTERP( o ) ) {
      PRINTF2( op, 40, "#<???:%ld:%08lx>", TYPE( o ), (unsigned long)o );
   } else {
      PRINTF1( op, 40, "#<???:%08lx>", (unsigned long)o );
   }

   BGL_MUTEX_UNLOCK( mutex );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ill_char_rep ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_ill_char_rep( unsigned char c ) {
   char aux[ 10 ];

   sprintf( aux, "#a%03d", c );

   return c_constant_string_to_string( aux );
}



