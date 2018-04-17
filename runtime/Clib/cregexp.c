/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cregexp.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  6 15:44:28 2011                          */
/*    Last change :  Tue Apr 17 08:03:13 2018 (serrano)                */
/*    Copyright   :  2011-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Native posix regular expressions for Bigloo                      */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_regexp ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_regexp( obj_t pat ) {
   obj_t re = GC_MALLOC( BGL_REGEXP_SIZE );
   int err;
   
   re->regexp.header = MAKE_HEADER( REGEXP_TYPE, 0 );
   re->regexp.pat = pat;

   return BREF( re );
}

#if( BGL_REGEXP_TYPE == BGL_REGEXP_regex )

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regcomp ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regcomp( obj_t pat, obj_t _ ) {
   obj_t re = bgl_make_regexp( pat );
   int err;

   if( !(err = regcomp( &(BGL_REGEXP_PREG( re )),
			BSTRING_TO_STRING( pat ),
			REG_EXTENDED )) ) {
      return re;
   } else {
      char *buf;
      int n;
      n = regerror( err, &(BGL_REGEXP_PREG( re )), 0, 0);

      buf = alloca( n + 1 );
      regerror( err, &(BGL_REGEXP_PREG( re )), buf, n );

      C_SYSTEM_FAILURE( BGL_IO_PARSE_ERROR, "pregexp", buf, pat );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regfree ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regfree( obj_t o ) {
   regfree( &(BGL_REGEXP_PREG( o )) );
   return BUNSPEC;
}
	    
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regmatch ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_regmatch( obj_t re, char *string, bool_t stringp, int beg, int end ) {
   int nmatch = BGL_REGEXP_PREG( re ).re_nsub + 1;
   regmatch_t *pmatch = alloca( sizeof( *pmatch ) * nmatch );
   int r;

   if( end > 0 ) {
      char *tmp = alloca( end - beg + 1 );
      memcpy( tmp, &string[ beg ], (end - beg) );
      string = tmp;
   } else {
      if( beg > 0 ) {
	 string += beg;
      }
   }
   
   r = regexec( &(BGL_REGEXP_PREG( re )), string, nmatch, pmatch, 0 );

   if( r == REG_NOMATCH ) {
      return BFALSE;
   } else {
      int i;
      obj_t res = MAKE_PAIR( BNIL, BNIL );
      obj_t tail = res;

      for( i = 0; i < nmatch; i++ ) {
	 if( pmatch[ i ].rm_so == -1 ) {
	    SET_CDR( tail, MAKE_PAIR( BFALSE, BNIL ) );
	 } else {
	    obj_t s = stringp
	       ? string_to_bstring_len(
		  &string[ pmatch[ i ].rm_so ],
		  pmatch[ i ].rm_eo - pmatch[ i ].rm_so )
	       : MAKE_PAIR(
		  BINT( pmatch[ i ].rm_so + beg),
		  BINT( pmatch[ i ].rm_eo + beg) );
	    
	    SET_CDR( tail, MAKE_PAIR( s, BNIL ) );
	 }
	 tail = CDR( tail );
      }

      return CDR( res );
   }
}

#endif

#if( BGL_REGEXP_TYPE == BGL_REGEXP_pcre )
#include <pcre.h>

static obj_t utf8_symbol = BUNSPEC;
static obj_t javascript_symbol = BUNSPEC;
static obj_t caseless_symbol = BUNSPEC;
static obj_t multiline_symbol = BUNSPEC;

#if( !defined( PCRE_JAVASCRIPT_COMPAT ) )
#  define PCRE_JAVASCRIPT_COMPAT 0
#endif	    

#if( !defined( PCRE_NEWLINE_ANY ) )
#  define PCRE_NEWLINE_ANY 0
#endif	    

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
obj_t
bgl_regfree( obj_t re ) {
   pcre *pcre = BGL_REGEXP_PREG( re );

   if( pcre ) {
      if( !pcre_refcount( pcre, -1 ) ) {

#if( BGL_REGEXP_HAS_FREE_STUDY )      
	 if( BGL_REGEXP( ( re ) ).study ) {
	    pcre_free_study( BGL_REGEXP( ( re ) ).study );
	 }
#endif      
	 pcre_free( pcre );
      }
      BGL_REGEXP_PREG( re ) = 0L;
   }
   
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_pcre_regcomp_finalize ...                                    */
/*---------------------------------------------------------------------*/
static void
bgl_pcre_regcomp_finalize( obj_t re, obj_t _ ) {
   bgl_regfree( re );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regcomp ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regcomp( obj_t pat, obj_t optargs ) {
   obj_t re = bgl_make_regexp( pat );
   const char *error;
   int erroffset;
   int options = bgl_pcre_options( optargs );
   static int init = 1000;

   if( !init-- ) { 
      init = 1000;
      /* force finalizers to free unused regexp */
      GC_invoke_finalizers();
   }
   
#ifndef PCRE_STUDY_JIT_COMPILE
#define PCRE_STUDY_JIT_COMPILE 0
#endif
   
   if( BGL_REGEXP_PREG( re ) =
       pcre_compile( BSTRING_TO_STRING( pat ), options,
		     &error, &erroffset, NULL ) ) {
      pcre_refcount( BGL_REGEXP_PREG( re ), 1 );
      BGL_REGEXP( re ).study = pcre_study( BGL_REGEXP_PREG( re ),
					   PCRE_STUDY_JIT_COMPILE,
					   &error );

      pcre_fullinfo( BGL_REGEXP_PREG( re ),
		     BGL_REGEXP( re ).study,
		     PCRE_INFO_CAPTURECOUNT,
		     &(BGL_REGEXP( re ).capturecount) );

      GC_register_finalizer( re, (GC_finalization_proc)&bgl_pcre_regcomp_finalize,
			     0, 0L, 0L );
      return re;
   } else {
      char *buf = alloca( 50 + strlen( error ) );

      sprintf( buf, "PCRE compilation failed at offset %d: %s\n",
	       erroffset, error );

      C_SYSTEM_FAILURE( BGL_IO_PARSE_ERROR, "pregexp", buf, pat );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regmatch ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_regmatch( obj_t re, char *string, bool_t stringp, int beg, int len ) {
   int oveccount = BGL_REGEXP( re ).capturecount + 1;
   int *ovect = alloca( sizeof( int ) * oveccount * 3 );
   int r;

   r = pcre_exec( BGL_REGEXP_PREG( re ), BGL_REGEXP( re ).study,
		  string, len, beg, 0, ovect, oveccount * 3 );

   if( r < 0 ) {
      return BFALSE;
   } else {
      int i;
      obj_t res = MAKE_STACK_PAIR( BNIL, BNIL );
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
long
bgl_regmatch_n( obj_t re, char *string, obj_t vres, int beg, int len ) {
   int oveccount = BGL_REGEXP( re ).capturecount + 1;
   int *ovect = alloca( sizeof( int ) * oveccount * 3 );
   int r;

   r = pcre_exec( BGL_REGEXP_PREG( re ), BGL_REGEXP( re ).study,
		  string, len, beg, 0, ovect, oveccount * 3 );

   if( r < 0 ) {
      return -1;
   } else {
      long i;
      long len = VECTOR_LENGTH( vres ) & ~1;

      for( i = 0; i < oveccount * 2 && i < len; i += 2 ) {
	 VECTOR_SET( vres, i, BINT( ovect[ i ] ) );
	 VECTOR_SET( vres, i + 1, BINT( ovect[ i + 1 ] ) );
      }
      
      return i >> 1;
   }
}

#endif

