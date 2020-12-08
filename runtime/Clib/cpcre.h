/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cpcre.h          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jan 14 15:13:55 2019                          */
/*    Last change :  Thu Aug  8 13:35:02 2019 (serrano)                */
/*    Copyright   :  2019-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo PCRE binding.                                             */
/*=====================================================================*/
#include <pcre.h>

extern obj_t make_string( long, unsigned char );

static obj_t utf8_symbol = BUNSPEC;
static obj_t javascript_symbol = BUNSPEC;
static obj_t caseless_symbol = BUNSPEC;
static obj_t multiline_symbol = BUNSPEC;
static obj_t noraise_symbol = BUNSPEC;

#if( !defined( PCRE_JAVASCRIPT_COMPAT ) )
#  define PCRE_JAVASCRIPT_COMPAT 0
#endif	    

#if( !defined( PCRE_NEWLINE_ANY ) )
#  define PCRE_NEWLINE_ANY 0
#endif	    

#define BGL_REGEXP_PCRE( o ) (pcre *)(BGL_REGEXP_PREG( o ))
#define BGL_REGEXP_CHAR( o ) (char)(long)(BGL_REGEXP_PREG( o ))

#define PCRE_BGLNORAISE PCRE_DUPNAMES

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_pcre_options_init ...                                        */
/*---------------------------------------------------------------------*/
void
bgl_pcre_options_init() {
   if( utf8_symbol == BUNSPEC ) {
      utf8_symbol = string_to_symbol( "UTF8");
      javascript_symbol = string_to_symbol( "JAVASCRIPT_COMPAT" );
      caseless_symbol = string_to_symbol( "CASELESS" );
      multiline_symbol = string_to_symbol( "MULTILINE" );
      noraise_symbol = string_to_symbol( "NORAISE" );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_pcre_options ...                                             */
/*---------------------------------------------------------------------*/
static int
bgl_pcre_options( obj_t args ) {
   int options = 0;

   if( PAIRP( args ) ) {
      bgl_pcre_options_init();
   
      while( PAIRP( args ) ) {
	 if( CAR( args ) == utf8_symbol ) {
	    options |= PCRE_UTF8;
	 } else if( CAR( args ) == caseless_symbol ) {
	    options |= PCRE_CASELESS;
	 } else if( CAR( args ) == javascript_symbol ) {
	    options |= PCRE_JAVASCRIPT_COMPAT;
	 } else if( CAR( args ) == multiline_symbol ) {
	    options |= PCRE_MULTILINE | PCRE_NEWLINE_ANY;
	 } else if( CAR( args ) == noraise_symbol ) {
	    options |= PCRE_BGLNORAISE;
	 } else {
	    if( CAR( args ) != BFALSE ) {
	       C_SYSTEM_FAILURE( BGL_IO_PARSE_ERROR, "pregexp",
				 "Illegal PCRE option", CAR( args ) );
	       return 0;
	    }
	 }

	 args = CDR( args );
      }
   }

   return options;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regfree ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
bgl_regfree( obj_t re ) {
   pcre *rx = BGL_REGEXP_PCRE( re );

   if( rx ) {
      if( !pcre_refcount( rx, -1 ) ) {

#if( BGL_REGEXP_HAS_FREE_STUDY )      
	 if( BGL_REGEXP( ( re ) ).study ) {
	    pcre_free_study( BGL_REGEXP( ( re ) ).study );
	 }
#endif      
	 pcre_free( rx );
      }
      BGL_REGEXP_PREG( re ) = 0L;
   }
   
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regmatch ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
bgl_regmatch( obj_t re, char *string, bool_t stringp, int beg, int len ) {
   int oveccount = BGL_REGEXP( re ).capturecount + 1;
   int *ovect = alloca( sizeof( int ) * oveccount * 3 );
   int r;

   r = pcre_exec( BGL_REGEXP_PCRE( re ), BGL_REGEXP( re ).study,
		  string, len, beg, 0, ovect, oveccount * 3 );

   if( r < 0 ) {
      return BFALSE;
   } else {
      int i;
      obj_t tmp;
      obj_t res = MAKE_STACK_PAIR_TMP( BNIL, BNIL, tmp );
      obj_t tail = res;

      for( i = 0; i < oveccount * 2; i += 2 ) {
	 if( ovect[ i ] < 0 ) {
	    SET_CDR( tail, MAKE_PAIR( BFALSE, BNIL ) );
	 } else {
	    obj_t s = stringp
	       ? string_to_bstring_len(
		  &string[ ovect[ i ] ],
		  ovect[ i + 1 ] - ovect[ i ] )
	       : MAKE_PAIR(
		  BINT( ovect[ i ] ),
		  BINT( ovect[ i + 1 ] ) );
	    
	    SET_CDR( tail, MAKE_PAIR( s, BNIL ) );
	 }
	 tail = CDR( tail );
      }

      return CDR( res );
   }
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_regmatch_n ...                                               */
/*---------------------------------------------------------------------*/
static long
bgl_regmatch_n( obj_t re, char *string, obj_t vres, int beg, int len ) {
   int oveccount = BGL_REGEXP( re ).capturecount + 1;
   int *ovect = alloca( sizeof( int ) * oveccount * 3 );
   int r;

   r = pcre_exec( BGL_REGEXP_PCRE( re ), BGL_REGEXP( re ).study,
		  string, len, beg, 0, ovect, oveccount * 3 );

   if( r < 0 ) {
      return -1;
   } else {
      long i;
      long vlen = VECTOR_LENGTH( vres ) & ~1;
      long end = oveccount * 2 < vlen ? oveccount * 2 : vlen;

      for( i = 0; i < end; i += 2 ) {
	 VECTOR_SET( vres, i, BINT( ovect[ i ] ) );
	 VECTOR_SET( vres, i + 1, BINT( ovect[ i + 1 ] ) );
      }
      
      return i >> 1;
   }
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    char_compile ...                                                 */
/*---------------------------------------------------------------------*/
static long
char_compile( char *string, int options ) {
   return (long)(*string);
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_charmatch ...                                                */
/*---------------------------------------------------------------------*/
static obj_t
bgl_charmatch( obj_t re, char *string, bool_t stringp, int beg, int len ) {
   char c = BGL_REGEXP_CHAR( re );

   while( beg < len ) {
      if( string[ beg++ ] == c ) {
	 obj_t p = stringp ?
	    make_string( 1, c ) : MAKE_PAIR( BINT( beg - 1 ), BINT( beg ) );

	 return MAKE_PAIR( p, BNIL );
      }
   }

   return BFALSE;
}
   
/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_charmatch_n ...                                              */
/*---------------------------------------------------------------------*/
static long
bgl_charmatch_n( obj_t re, char *string, obj_t vres, int beg, int len ) {
   char c = BGL_REGEXP_CHAR( re );

   while( beg < len ) {
      if( string[ beg++ ] == c ) {
	 if( (VECTOR_LENGTH( vres ) & ~1) > 0 ) {
	    VECTOR_SET( vres, 0, BINT( beg - 1 ) );
	    VECTOR_SET( vres, 1, BINT( beg ) );

	    return 1;
	 } else {
	    return 0;
	 }
      }
   }

   return -1;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_charfree ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
bgl_charfree( obj_t re ) {
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_pcre_regcomp_finalize ...                                    */
/*---------------------------------------------------------------------*/
static void
bgl_pcre_regcomp_finalize( obj_t re, obj_t _ ) {
   BGL_REGEXP( BREF( re ) ).free( BREF( re ) );
}

/*---------------------------------------------------------------------*/
/*    CHAR_REGEXP ...                                                  */
/*---------------------------------------------------------------------*/
#define CHAR_REGEXP( pat, options ) \
   (STRING_LENGTH( pat ) == 1 \
    && !strchr( "$[*+?.(", STRING_REF( pat, 0 ) ) \
    && !(options & PCRE_CASELESS))

/*---------------------------------------------------------------------*/
/*    CHAR_ESCAPE_REGEXP ...                                           */
/*---------------------------------------------------------------------*/
#define CHAR_ESCAPE_REGEXP( pat, options ) \
   (STRING_LENGTH( pat ) == 2 \
    && STRING_REF( pat, 0 ) == '\\' \
    && !strchr( "\\-$[*+?.(", STRING_REF( pat, 0 ) ) \
    && !(options & PCRE_CASELESS))

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regcomp ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regcomp( obj_t pat, obj_t optargs, bool_t finalize ) {
   obj_t re = bgl_make_regexp( pat );
   const char *error;
   int erroffset;
   int options = bgl_pcre_options( optargs );
   static int init = 1000;

   if( CHAR_REGEXP( pat, options ) ) {
      BGL_REGEXP_PREG( re ) = (void *)char_compile( BSTRING_TO_STRING( pat ), options );
	   
      BGL_REGEXP( re ).match = bgl_charmatch;
      BGL_REGEXP( re ).match_n = bgl_charmatch_n;
      BGL_REGEXP( re ).free = bgl_charfree;
      BGL_REGEXP( re ).capturecount = 1;
      
      return re;
   } else if( CHAR_ESCAPE_REGEXP( pat, options ) ) {
      BGL_REGEXP_PREG( re ) = (void *)char_compile( BSTRING_TO_STRING( pat ) + 1, options );
	   
      BGL_REGEXP( re ).match = bgl_charmatch;
      BGL_REGEXP( re ).match_n = bgl_charmatch_n;
      BGL_REGEXP( re ).free = bgl_charfree;
      BGL_REGEXP( re ).capturecount = 1;
      
      return re;
   } else {
      if( finalize && !init-- ) { 
	 init = 1000;
	 /* force finalizers to free unused regexp */
	 GC_invoke_finalizers();
      }
   
#ifndef PCRE_STUDY_JIT_COMPILE
#define PCRE_STUDY_JIT_COMPILE 0
#endif
   
      if( (BGL_REGEXP_PREG( re ) =
	   pcre_compile( BSTRING_TO_STRING( pat ), options & ~PCRE_BGLNORAISE,
			 &error, &erroffset, NULL )) ) {
	 pcre_refcount( BGL_REGEXP_PCRE( re ), 1 );
	 BGL_REGEXP( re ).study = pcre_study( BGL_REGEXP_PCRE( re ),
					      PCRE_STUDY_JIT_COMPILE,
					      &error );

	 pcre_fullinfo( BGL_REGEXP_PCRE( re ),
			BGL_REGEXP( re ).study,
			PCRE_INFO_CAPTURECOUNT,
			&(BGL_REGEXP( re ).capturecount) );

	 if( finalize ) {
	    GC_register_finalizer( CREF( re ),
				   (GC_finalization_proc)&bgl_pcre_regcomp_finalize,
				   0, 0L, 0L );
	 }

	 BGL_REGEXP( re ).match = bgl_regmatch;
	 BGL_REGEXP( re ).match_n = bgl_regmatch_n;
	 BGL_REGEXP( re ).free = bgl_regfree;
      
	 return re;
      } else {
	 char *buf = alloca( 50 + strlen( error ) );

	 sprintf( buf, "PCRE compilation failed at offset %d: %s\n",
		  erroffset, error );

	 if( !options & PCRE_BGLNORAISE ) {
	    C_SYSTEM_FAILURE( BGL_IO_PARSE_ERROR, "pregexp", buf, pat );

	    return re;
	 } else {
	    return string_to_bstring( buf );
	 }
      }
   }
}

