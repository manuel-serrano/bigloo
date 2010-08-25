/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cwriter.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec 17 09:44:20 1991                          */
/*    Last change :  Wed Aug 25 10:20:27 2010 (serrano)                */
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
extern obj_t bgl_bignum_to_string( obj_t x, long radix );
extern obj_t bgl_write_obj( obj_t, obj_t );
extern obj_t bgl_display_obj( obj_t, obj_t );
extern obj_t bgl_write( obj_t, unsigned char *, size_t );

/*---------------------------------------------------------------------*/
/*    Les noms des caracateres                                         */
/*---------------------------------------------------------------------*/
static unsigned char *char_name[] = {
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
/*    PUTC ...                                                         */
/*---------------------------------------------------------------------*/
#define PUTC( op, c ) {						       \
   *OUTPUT_PORT( op ).ptr++ = c;				       \
   if( --OUTPUT_PORT( op ).cnt > 0 ) {				       \
      if( (c == '\n') && (OUTPUT_PORT( op ).bufmode == BGL_IOLBF) ) {  \
	 bgl_output_flush( op, 0, 0 );				       \
      }								       \
   } else {							       \
      bgl_output_flush( op, 0, 0 );				       \
   }								       \
 }								       \

/*---------------------------------------------------------------------*/
/*    PUTS ...                                                         */
/*    -------------------------------------------------------------    */
/*    This assumes than strings do not contain \n character.           */
/*---------------------------------------------------------------------*/
#define PUTS( op, str )						       \
   if( OUTPUT_PORT( op ).cnt >= (sizeof( str ) - 1) ) {		       \
      memcpy( OUTPUT_PORT( op ).ptr, str, (sizeof( str ) - 1) );       \
      OUTPUT_PORT( op ).ptr += (sizeof( str ) - 1);		       \
      OUTPUT_PORT( op ).cnt -= (sizeof( str ) - 1);		       \
   } else {							       \
      bgl_output_flush( op, str, (sizeof( str ) - 1) );		       \
   }								       \

/*---------------------------------------------------------------------*/
/*    PRINTF ...                                                       */
/*---------------------------------------------------------------------*/
#ifdef __GNUC__
#  define _new( v, s ) v[ s ]
#else
#  define *_new = alloca( s )
#endif

#define PRINTF1( op, sz, fmt, arg0 )			    \
   if( OUTPUT_PORT( op ).cnt > sz ) {			    \
      int n = sprintf( OUTPUT_PORT( op ).ptr, fmt, arg0 );  \
      OUTPUT_PORT( op ).ptr += n;			    \
      OUTPUT_PORT( op ).cnt -= n;			    \
   } else {						    \
      char _new( __buf, sz  );				    \
      int n = sprintf( __buf, fmt, arg0 );		    \
      bgl_output_flush( op, __buf, n );			    \
   }

#define PRINTF2( op, sz, fmt, arg0, arg1 )				\
   if( OUTPUT_PORT( op ).cnt > sz ) {					\
      int n = sprintf( OUTPUT_PORT( op ).ptr, fmt, arg0, arg1 );	\
      OUTPUT_PORT( op ).ptr += n;					\
      OUTPUT_PORT( op ).cnt -= n;					\
   } else {								\
      char _new( __buf, sz );						\
      int n = sprintf( __buf, fmt, arg0, arg1 );                  	\
      bgl_output_flush( op, __buf, n );					\
   }

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_substring ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_display_substring( obj_t o, long start, long end, obj_t op ) {
   return bgl_write( op, &STRING_REF( o, start ), end - start );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_string ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_display_string( obj_t o, obj_t op ) {
   return bgl_write( op, &STRING_REF( o, 0 ), STRING_LENGTH( o ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_string ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_string( obj_t o, bool_t esc, obj_t op ) {
   if( esc ) PUTC( op, '#' );
   
   PUTC( op, '"' );
   bgl_display_string( o, op );
   PUTC( op, '"' );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_fixnum ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_display_fixnum( obj_t o, obj_t op ) {
   void *ostream = PORT_STREAM( op );
   
   PRINTF1( op, 32, "%ld", CINT( o ) );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_elong ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_elong( long o, obj_t op ) {
   PRINTF1( op, 32, "%ld", o );
   
   return op;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_elong ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_elong( long o, obj_t op ) {
   PRINTF1( op, 32, "#e%ld", o );
   
   return op;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_llong ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_llong( BGL_LONGLONG_T o, obj_t op ) {
   bgl_display_string( llong_to_string( o, 10 ), op );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_llong ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_llong( BGL_LONGLONG_T o, obj_t op ) {
   PUTS( op, "#l" );
   bgl_display_string( llong_to_string( o, 10 ), op );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_bignum ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_bignum( obj_t o, obj_t op ) {
   bgl_display_string( bgl_bignum_to_string( o, 10 ), op );

   return op;
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_bignum ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_bignum( obj_t o, obj_t op ) {
   PUTS( op, "#z" );
   bgl_display_string( bgl_bignum_to_string( o, 10 ), op );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_char ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_display_char( char c, obj_t op ) {
   PUTC( op, c );
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_char ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_char( obj_t o, obj_t op ) {
   int c = CCHAR( o );
   void *ostream = PORT_STREAM( op );
   
   if( (c > 0) && (c < 128) && char_name[ c ][ 0 ] ) {
      unsigned char *name = char_name[ c ];
	 
      PUTC( op, '#' );
      PUTC( op, '\\' );
      bgl_write( op, name, strlen( name ) );
   } else {
      PUTC( op, '#' );
      PUTC( op, 'a' );

      PRINTF1( op, 4, "%03d", (unsigned char)(c) );
   }
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_ucs2 ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_ucs2( obj_t o, obj_t op ) {
   void *ostream = PORT_STREAM( op );
   
   PRINTF1( op, 7, "#u%04x", CUCS2( o ) );
   
   return op;
}   

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_display_ucs2 ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_display_ucs2( obj_t o, obj_t op ) {
   ucs2_t ch = CUCS2( o );
   
   if( UCS2_ISOLATIN1P( ch ) ) {
      PUTC( op, ch );
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
   int i;
   
   for( i = 0; i < len; i++ ) {
      ucs2_t ch = ucs2[ i ];
	 
#if( UCS2_DISPLAYABLE )
#else
      if( UCS2_ISOLATIN1P( ch ) )
	 PUTC( op, (char)ch );
#endif
   }
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_utf8string ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_utf8string( obj_t o, obj_t op ) {
   PUTS( op, "#u\"" );
   bgl_display_string( o, op );
   PUTC( op, '"' );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_opaque ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_opaque( obj_t o, obj_t op ) {
   PRINTF2( op, 40, "#<opaque:%ld:%08lx>", TYPE( o ), (unsigned long)o );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_cnst ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_cnst( obj_t o, obj_t op ) {
   PRINTF1( op, 8, "#<%04x>", (int)CCNST( o ) );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_procedure ...                                          */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_procedure( obj_t o, obj_t op ) {
   PRINTF2( op, 96,
	    "#<procedure:%lx.%ld>",
	    VA_PROCEDUREP( o ) ?
	    (unsigned long)PROCEDURE_VA_ENTRY( o ) :
	    (unsigned long)PROCEDURE_ENTRY( o ),
	    (long)PROCEDURE( o ).arity );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_output_port ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_output_port( obj_t o, obj_t op ) {
   PRINTF1( op, 20 + STRING_LENGTH( PORT( o ).name ),
	    "#<output_port:%s>",
	    BSTRING_TO_STRING( PORT( o ).name ) );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_input_port ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_input_port( obj_t o, obj_t op ) {
   PUTS( op, "#<input_port:" );
   bgl_display_obj( PORT( o ).name, op );
   PRINTF1( op, 10, ".%ld>", (long)BGL_INPUT_PORT_BUFSIZ( o ) );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_binary_port ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_binary_port( obj_t o, obj_t op ) {
   PRINTF2( op, 40 + STRING_LENGTH( BINARY_PORT( o ).name ),
	    "#<binary_%s_port:%s>",
	    BINARY_PORT_INP( o ) ? "input" : "output",
	    BSTRING_TO_STRING( BINARY_PORT( o ).name ) );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_foreign ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_foreign( obj_t o, obj_t op ) {
   PUTS( op, "#<foreign:" );
   bgl_display_obj( FOREIGN_ID( o ), op );
   PRINTF1( op, 16, ":%lx>", (long)FOREIGN_TO_COBJ( o ) );

   return op;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_dynamic_env ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_dynamic_env( obj_t o, obj_t op ) {
   PUTS( op, "#<dynamic-env:" );
   PRINTF1( op, 16, ":%p>", o );

   return op;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_process ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_process( obj_t o, obj_t op ) {
   PUTS( op, "#<process:" );
   PRINTF1( op, 20, "%d>", PROCESS_PID( o ) );

   return op;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_socket ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_socket( obj_t o, obj_t op ) {
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
      PRINTF2( op,
	       40 + (STRINGP( SOCKET( o ).hostname ) ?
		     STRING_LENGTH( SOCKET( o ).hostname ) :
		     sizeof( "localhost" )),
	       "#<socket:%s.%d>",
	       STRINGP( SOCKET( o ).hostname ) ?
	       BSTRING_TO_STRING( SOCKET( o ).hostname ) :
	       "localhost",
	       SOCKET( o ).portnum );
   }

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_mmap ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_mmap( obj_t o, obj_t op ) {
   PUTS( op, "#<mmap:" );
   bgl_display_obj( BGL_MMAP( o ).name, op );
   PRINTF1( op, 16, ":%ld>", (long)BGL_MMAP( o ).length );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_custom ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_custom( obj_t o, obj_t op ) {
   CUSTOM_OUTPUT( o )( o, op );

   return op;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write_unknown ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_write_unknown( obj_t o, obj_t op ) {
   if( POINTERP( o ) ) {
      PRINTF2( op, 40, "#<???:%ld:%08lx>", TYPE( o ), (unsigned long)o );
   } else {
      PRINTF1( op, 40, "#<???:%08lx>", (unsigned long)o );
   }

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



