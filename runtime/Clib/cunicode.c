/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cunicode.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon May 19 17:47:11 1997                          */
/*    Last change :  Tue Apr 17 08:00:21 2018 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Unicode strings handling                                         */
/*=====================================================================*/
#include <string.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Importations                                                     */
/*---------------------------------------------------------------------*/
extern bool_t ucs2_lower( ucs2_t );
extern obj_t integer_to_string( long, long );
extern obj_t make_string( long, unsigned char );
extern ucs2_t ucs2_tolower( ucs2_t );
extern obj_t bgl_real_to_string( double );

/*---------------------------------------------------------------------*/
/*    ucs2_string_t                                                    */
/*    make_ucs2_string ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_ucs2_string( int len, ucs2_t c ) {
   obj_t   string;
   ucs2_t *cstring;

   if( len < 0 ) {
      C_FAILURE( "make-ucs2-string", "Illegal string size", BINT( len ) );
   } else {
      int i;
      
      string  = GC_MALLOC_ATOMIC( UCS2_STRING_SIZE + (len * sizeof(ucs2_t)) );
      cstring = (&(string->ucs2_string.char0));

      string->ucs2_string.header = MAKE_HEADER( UCS2_STRING_TYPE, 0 );
      string->ucs2_string.length = len;

      for( i = 0; i < len; i++ )
	cstring[ i ] = c;
      cstring[ i ] = (ucs2_t)0;
		
      return BUCS2STRING( string );
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    ucs2cpy ...                                                      */
/*---------------------------------------------------------------------*/
static void
ucs2cpy( ucs2_t *u1, ucs2_t *u2, int len ) {
   for( len--; len >= 0; len-- )
      u1[ len ] = u2[ len ];
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_append ...                                           */
/*---------------------------------------------------------------------*/
obj_t
ucs2_string_append( obj_t s1, obj_t s2 ) {
   int    l1, l2;
   obj_t   ucs2_string;
   
   l1 = UCS2_STRING( s1 ).length;
   l2 = UCS2_STRING( s2 ).length;

   ucs2_string = GC_MALLOC_ATOMIC( UCS2_STRING_SIZE + ((l1 + l2) * sizeof( ucs2_t )) );

   ucs2_string->ucs2_string.header = MAKE_HEADER( UCS2_STRING_TYPE, 0 );
   ucs2_string->ucs2_string.length = l1 + l2;

   ucs2cpy( &(ucs2_string->ucs2_string.char0),
	    &UCS2_STRING_REF( s1, 0 ),
	    l1 );
   ucs2cpy( &((ucs2_t *)(&(ucs2_string->ucs2_string.char0)))[ l1 ],
	    &UCS2_STRING_REF( s2, 0 ),
	    l2 );
   ((ucs2_t *)(&(ucs2_string->ucs2_string.char0)))[ l1 + l2 ] = (ucs2_t)0;
	
   return BUCS2STRING( ucs2_string );
}
 
/*---------------------------------------------------------------------*/
/*    c_subucs2_string ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
c_subucs2_string( obj_t src_ucs2_string, int min, int max ) {
   int  len;
   obj_t dst_ucs2_string;
   
   len = max - min;

   dst_ucs2_string = GC_MALLOC_ATOMIC( UCS2_STRING_SIZE + (len * sizeof( ucs2_t ) ) );

   dst_ucs2_string->ucs2_string.header = MAKE_HEADER( UCS2_STRING_TYPE, 0 );
   dst_ucs2_string->ucs2_string.length = len;

   ucs2cpy( &(dst_ucs2_string->ucs2_string.char0),
	    &UCS2_STRING_REF( src_ucs2_string, min ),
            len );
   (&(dst_ucs2_string->ucs2_string.char0))[ len ] = (ucs2_t)0;

   return BUCS2STRING( dst_ucs2_string );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    c_ucs2_string_copy ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
c_ucs2_string_copy( obj_t src ) {
   int    len = UCS2_STRING_LENGTH( src );
   obj_t   string;
   ucs2_t *cstring, *cstr;
   int    i;

   string  = GC_MALLOC_ATOMIC( UCS2_STRING_SIZE + (len * sizeof( ucs2_t ) ) );
   cstring = (&(string->ucs2_string.char0));
   cstr    = &UCS2_STRING_REF( src, 0 );

   string->ucs2_string.header = MAKE_HEADER( UCS2_STRING_TYPE, 0 );
   string->ucs2_string.length = len;
   
   for( i = 0; i < len; i++ )
      cstring[ i ] = cstr[ i ];
   cstring[ i ] = (ucs2_t)0;
		
   return BUCS2STRING( string );
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_t                                                    */
/*    string_to_ucs2_string ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
string_to_ucs2_string( char *c ) {
   int     len = (int)strlen( c );
   obj_t   string;
   ucs2_t *cstring;
   int     i;

   string  = GC_MALLOC_ATOMIC( UCS2_STRING_SIZE + (len * sizeof( ucs2_t ) ) );
   cstring = (&(string->ucs2_string.char0));

   string->ucs2_string.header = MAKE_HEADER( UCS2_STRING_TYPE, 0 );
   string->ucs2_string.length = len;
   
   for( i = 0; i < len; i++ )
      cstring[ i ] = (ucs2_t)(c[ i ]);
   cstring[ i ] = (ucs2_t)0;
		
   return BUCS2STRING( string );
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_t                                                    */
/*    bstring_to_ucs2_string ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bstring_to_ucs2_string( obj_t src ) {
   int    len = STRING_LENGTH( src );
   obj_t   string;
   ucs2_t *cstring;
   int    i;
   char   *c = (char *)(&(src->string.char0));

   string  = GC_MALLOC_ATOMIC( UCS2_STRING_SIZE + (len * sizeof( ucs2_t ) ) );
   cstring = (&(string->ucs2_string.char0));

   string->ucs2_string.header = MAKE_HEADER( UCS2_STRING_TYPE, 0 );
   string->ucs2_string.length = len;
   
   for( i = 0; i < len; i++ )
      cstring[ i ] = (ucs2_t)(c[ i ]);
   cstring[ i ] = (ucs2_t)0;
		
   return BUCS2STRING( string );
}

/*---------------------------------------------------------------------*/
/*    integer_to_ucs2_string ...                                       */
/*---------------------------------------------------------------------*/
obj_t
integer_to_ucs2_string( long x, long radix ) {
   return string_to_ucs2_string( BSTRING_TO_STRING( integer_to_string( x, radix ) ) );
}

/*---------------------------------------------------------------------*/
/*    real_to_ucs2_string ...                                          */
/*---------------------------------------------------------------------*/
obj_t
real_to_ucs2_string( double x ) {
   return string_to_ucs2_string( BSTRING_TO_STRING( bgl_real_to_string( x ) ) );
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    ucs2_strcmp ...                                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t
ucs2_strcmp( obj_t o1, obj_t o2 ) {
   int l1, l2;
   
   l1 = UCS2_STRING_LENGTH( o1 );
   l2 = UCS2_STRING_LENGTH( o2 );

   if( l1 == l2 ) {
      ucs2_t *c1, *c2;
   
      c1 = BUCS2_STRING_TO_UCS2_STRING( o1 );
      c2 = BUCS2_STRING_TO_UCS2_STRING( o2 );
      
      for( l1--; l1 >= 0; l1-- )
	 if( c1[ l1 ] != c2[ l1 ] )
	    return 0;

      return 1;
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    ucs2_strcicmp ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
ucs2_strcicmp( obj_t bst1, obj_t bst2 ) {
   int l1, l2;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   if( l1 == l2 ) {
      ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
      ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
      int i;
   
      for( i = 0;
	   ucs2_tolower( *st1 ) == ucs2_tolower( *st2 );
	   i++, st1++, st2++ )
         if( i == l1 )
	    return 1;
   }

   return 0;
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_lt ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t       
ucs2_string_lt( obj_t bst1, obj_t bst2 )
{
   ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
   ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
   int l1, l2;
   int i, min;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   min = (l1 < l2) ? l1 : l2;

   for( i = 0; (*st1 == *st2) && (i < min); i++, st1++, st2++ );

   if( i < min )
      return *st1 < *st2;
   else
      return l1 < l2;
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_le ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
ucs2_string_le( obj_t bst1, obj_t bst2 ) {
   ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
   ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
   int l1, l2;
   int i, min;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   min = (l1 < l2) ? l1 : l2;

   for( i = 0; (*st1 == *st2) && (i < min); i++, st1++, st2++ );

   if( i < min )
      return *st1 <= *st2;
   else
      return l1 <= l2;
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_gt ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t       
ucs2_string_gt( obj_t bst1, obj_t bst2 ) {
   ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
   ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
   int l1, l2;
   int i, min;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   min = (l1 < l2) ? l1 : l2;

   for( i = 0; (*st1 == *st2) && (i < min); i++, st1++, st2++ );

   if( i < min )
      return *st1 > *st2;
   else
      return l1 > l2;
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_ge ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
ucs2_string_ge( obj_t bst1, obj_t bst2 ) {
   ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
   ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
   int l1, l2;
   int i, min;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   min = (l1 < l2) ? l1 : l2;

   for( i = 0; (*st1 == *st2) && (i < min); i++, st1++, st2++ );

   if( i < min )
      return *st1 >= *st2;
   else
      return l1 >= l2;
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_cilt ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t       
ucs2_string_cilt( obj_t bst1, obj_t bst2 ) {
   ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
   ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
   int l1, l2;
   int i, min;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   min = (l1 < l2) ? l1 : l2;

   for( i = 0;
	(ucs2_tolower( *st1 ) == ucs2_tolower( *st2 )) && (i < min);
	i++, st1++, st2++ );

   if( i < min )
      return ucs2_tolower( *st1 ) < ucs2_tolower( *st2 );
   else
      return l1 < l2;
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_cile ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
ucs2_string_cile( obj_t bst1, obj_t bst2 ) {
   ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
   ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
   int l1, l2;
   int i, min;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   min = (l1 < l2) ? l1 : l2;

   for( i = 0;
        (ucs2_tolower( *st1 ) == ucs2_tolower( *st2 )) && (i < min);
        i++, st1++, st2++ );

   if( i < min )
      return ucs2_tolower( *st1 ) <= ucs2_tolower( *st2 );
   else
      return l1 <= l2;
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_cigt ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t       
ucs2_string_cigt( obj_t bst1, obj_t bst2 ) {
   ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
   ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
   int l1, l2;
   int i, min;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   min = (l1 < l2) ? l1 : l2;

   for( i = 0;
        (ucs2_tolower( *st1 ) == ucs2_tolower( *st2 )) && (i < min);
        i++, st1++, st2++ );

   if( i < min )
      return ucs2_tolower( *st1 ) > ucs2_tolower( *st2 );
   else
      return l1 > l2;
}

/*---------------------------------------------------------------------*/
/*    ucs2_string_cige ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
bool_t        
ucs2_string_cige( obj_t bst1, obj_t bst2 ) {
   ucs2_t *st1 = BUCS2_STRING_TO_UCS2_STRING( bst1 );
   ucs2_t *st2 = BUCS2_STRING_TO_UCS2_STRING( bst2 );
   int l1, l2;
   int i, min;

   l1 = UCS2_STRING_LENGTH( bst1 );
   l2 = UCS2_STRING_LENGTH( bst2 );

   min = (l1 < l2) ? l1 : l2;

   for( i = 0;
        (ucs2_tolower( *st1 ) == ucs2_tolower( *st2 )) && (i < min);
        i++, st1++, st2++ );

   if( i < min )
      return ucs2_tolower( *st1 ) >= ucs2_tolower( *st2 );
   else
      return l1 >= l2;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    utf8_size ...                                                    */
/*---------------------------------------------------------------------*/
static int
utf8_size( ucs2_t ucs2 ) {
   if( ucs2 <= 0x7f )
      return 1;
   if( ucs2 <= 0x7ff )
      return 2;
   if( ucs2 <= 0xd7ff )
      return 3;
   if( ucs2 <= 0xdbff )
      /* utf16 extension, see http://en.wikipedia.org/wiki/UTF-16 */
      return 4;
   if( ucs2 <= 0xdfff )
      return 4;
   //C_FAILURE( "utf8_size", "Illegal ucs2 character", BUCS2( ucs2 ) );
   if( ucs2 <= 0xfffd )
      return 3;
   if( ucs2 <= 0xffff )
      /* see http://en.wikipedia.org/wiki/UTF-16 */
      return 3;
   
   C_FAILURE( "utf8_size", "Illegal ucs2 character", BUCS2( ucs2 ) );

   return -1;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    ucs2_string_to_utf8_string ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
ucs2_string_to_utf8_string( obj_t bucs2 ) {
   int len = UCS2_STRING_LENGTH( bucs2 );
   int utf8_len = 0;
   ucs2_t *cucs2 = BUCS2_STRING_TO_UCS2_STRING( bucs2 );
   int read, write;
   obj_t result;
   unsigned char *cresult;
   
   /* First, we compute the size of the futur utf8 string */
   for( read = 0, utf8_len = 0; read < len; read++ )
      utf8_len += utf8_size( cucs2[ read ] );

   /* now, we allocate the new string */
   result  = make_string( utf8_len, '0' );
   cresult = BSTRING_TO_USTRING( result );

   /* and we fill it */
   for( read = 0, write = 0; read < len; read++ ) {
      ucs2_t ucs2 = cucs2[ read ];
      int ulen  = utf8_size( ucs2 );

      switch( ulen ) {
	 case 1:
	    cresult[ write++ ] = (unsigned char)ucs2;
	    break;
	    
	 case 4: 
	    if( read < len - 1 ) {
	       /* www.ecma-international.org/ecma-262/5.1/#sec-15.1.2.4 */
	       ucs2_t nu = cucs2[ read + 1 ];

	       if( (nu >= 0xdc00) && (nu <= 0xdfff) ) {
		  read++;
		  int zzzzzz = (nu & 0x3f);
		  int yyyy = (nu & 0x3ff) >> 6;
		  int xx = ucs2 & 3;
		  int wwww = (ucs2 >> 2) & 0xf;
		  int vvvv = (ucs2 >> 6) & 0xf;
		  int uuuuu = vvvv + 1;

		  cresult[ write + 3 ] =
		     (unsigned char)(zzzzzz + 0x80);
		  cresult[ write + 2 ] =
		     (unsigned char)((xx << 4) | yyyy) + 0x80;
		  cresult[ write + 1 ] =
		     (unsigned char)(((uuuuu & 3) << 4) | wwww) + 0x80;
		  cresult[ write ] =
		     (unsigned char)(0xf0 | (uuuuu >> 2));

		  utf8_len -= 4;
		  write += ulen;
		  break;
	       }
	    } 

	    if( (ucs2 >= 0xd800) && (ucs2 <= 0xdbff) ) {
	       /* See http://en.wikipedia.org/wiki/UTF-8, section   */
	       /* Invalid code points.                              */
	       /* We encode, half utf16 #xdc00-#dbff with the       */
	       /* illegal code point #xf8, and 3 following bytes.   */
	       /* This allows an application to implement a correct */
	       /* utf8 string-append.                               */
	       int xx = ucs2 & 3;
	       int wwww = (ucs2 >> 2) & 0xf;
	       int vvvv = (ucs2 >> 6) & 0xf;
	       int uuuuu = vvvv + 1;

	       cresult[ write + 3 ] =
		  (unsigned char)(0x80 | (uuuuu >> 2));
	       cresult[ write + 2 ] =
		  (unsigned char)(xx << 4) + 0x80;
	       cresult[ write + 1 ] =
		  (unsigned char)(((uuuuu & 3) << 4) | wwww) + 0x80;
	       cresult[ write ] =
		  (unsigned char)0xf8;
	       
	       write += ulen;
	       break;
	    } else {
	       int nu = ucs2;
	       int zzzzzz = (nu & 0x3f);
	       int yyyy = (nu & 0x3ff) >> 6;

	       cresult[ write + 3 ] =
		  (unsigned char)(zzzzzz + 0x80);
	       cresult[ write + 2 ] =
		  (unsigned char)(yyyy + 0x80);
	       cresult[ write + 1 ] =
		  (unsigned char)0x80;
	       cresult[ write ] =
		  (unsigned char)0xfc;

	       write += ulen;
	       break;
	    }
	       
	 case 3:
	    cresult[ write + 2 ] = (unsigned char)(0x80 + (ucs2 & 0x3f));
	    ucs2 >>= 6;
	       
	 default:
	    cresult[ write + 1 ] = (unsigned char)(0x80 + (ucs2 & 0x3f));
	    ucs2 >>= 6;
	    cresult[ write ] = (unsigned char)(0xff - (0xff >> ulen) + ucs2);
	    write += ulen;
      }
   }

   result = bgl_string_shrink( result, utf8_len );
   
   return result;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    utf8_string_to_ucs2_string ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
utf8_string_to_ucs2_string( obj_t butf8 ) {
   int len = STRING_LENGTH( butf8 );
   ucs2_t *aux = (ucs2_t *)malloc( len * sizeof( ucs2_t ) );
   char *cutf8 = BSTRING_TO_STRING( butf8 );
   int read, write;
   obj_t string;
   ucs2_t *cstring;
   
   for( read = 0, write = 0; read < len; write++ ) {
      unsigned char byte = cutf8[ read++ ];

      if( byte <= 0x7f ) {
         aux[ write ] = (ucs2_t)byte;
      } else if( byte == 0xf8 ) {
	 char b1 = cutf8[ read++ ];
	 char b2 = cutf8[ read++ ];
	 char b3 = cutf8[ read++ ];
	 int u4u3 = (b3 & 0x3) << 2;
	 int xx = (b2 >> 4) & 0x3;
	 int wwww = b1 & 0xf;
	 int u2u1 = (b1 >> 4) & 0x3;
	 int uuuu = u4u3 | u2u1;
	 int vvvv = uuuu -1;
	 int hi = 0x36;
	 
	 aux[ write ] = (ucs2_t)( xx | (wwww << 2) | (vvvv << 6) | (hi << 10) );
      } else if( byte == 0xfc ) {
	 char b1 = cutf8[ read++ ];
	 char b2 = cutf8[ read++ ];
	 char b3 = cutf8[ read++ ];
	 int zzzzzz = b3 & 0x3f;
	 int yyyy = b2 & 0xf;
	 int hi = 0x37;
	 
	 aux[ write ] = (ucs2_t)( zzzzzz | (yyyy << 6) | (hi << 10) );
      } else if( (byte <= 0xbf) || (byte >= 0xfd) ) {
	 free( aux );
	 C_FAILURE( "utf8-string->ucs2-string",
		    "Illegal first byte",
		    BINT( byte ) );
      } else {
	 unsigned long ucs2 = (unsigned long)byte;
	 int bits = 6;

	 while( byte & 0x40 ) {
	    unsigned char next = cutf8[ read++ ];
               
	    if( (next <= 0x7f) || (next > 0xbf) ) {
	       free( aux );
	       C_FAILURE( "utf8-string->ucs2-string",
			  "Illegal following byte",
			  BINT( next ) );
	    }
               
	    ucs2 = (ucs2 << 6) + (next & 0x3f);
	    byte <<= 1;
	    bits += 5;
	 }

	 ucs2 &= (1<<bits) - 1;

/* 	 if( (ucs2 > 0xd7ff && ucs2 <= 0xdfff) ||                      */
/* 	     !(ucs2 & (~(unsigned long)0<<(bits - 5))) ) {             */
/* 	    // characters fffe and ffff are accepted, see:             */
/* 	    // http://www.unicode.org/versions/Unicode5.2.0/ch16.pdf#G19635 */
/* 	    C_FAILURE( "utf8-string->ucs2-string",                     */
/* 		       "Illegal utf8 character encoding",              */
/* 		       BINT( ucs2 ) );                                 */
/* 	 } else {                                                      */
	    if( ucs2 >= 0x10000 ) {
	       ucs2 -= 0x10000;
	       aux[ write++ ] = (ucs2_t)((ucs2 >> 10) + 0xd800);
	       aux[ write ] = (ucs2_t)((ucs2 & 0x3FF) + 0xdc00);
	    } else {
	       aux[ write ] = (ucs2_t)ucs2;
	    }
/* 	 }                                                             */
      }
   }
         
   string  = GC_MALLOC_ATOMIC( UCS2_STRING_SIZE + (len * sizeof( ucs2_t ) ) );
   cstring = (&(string->ucs2_string.char0));

   string->ucs2_string.header = MAKE_HEADER( UCS2_STRING_TYPE, 0 );
   string->ucs2_string.length = write;
   ucs2cpy( cstring, aux, write );
                
   free( aux );
   
   return BUCS2STRING( string );
}
