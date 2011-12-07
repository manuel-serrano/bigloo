/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cregex.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  6 15:44:28 2011                          */
/*    Last change :  Wed Dec  7 13:39:00 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
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
   
   re->regexp_t.header = MAKE_HEADER( REGEXP_TYPE, 0 );
   re->regexp_t.pat = pat;

   return BREF( re );
}

#if( BGL_REGEXP_TYPE == BGL_REGEXP_REGEX )

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regcomp ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regcomp( obj_t pat ) {
   obj_t re = bgl_make_regexp( pat );
   int err;
   
   if( !(err = regcomp( &(BGL_REGEXP_PREG( re )),
			BSTRING_TO_STRING( regex ),
			REG_EXTENDED )) ) {
      return BREF( re );
   } else {
      char *buf;
      int n;
      n = regerror( err, &(BGL_REGEXP_PREG( re )), 0, 0);

      buf = alloca( n + 1 );
      regerror( err, &(BGL_REGEXP_PREG( re )), buf, n );

      C_SYSTEM_FAILURE( BGL_IO_PARSE_ERROR, "pregexp", buf, regex );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regfree ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regfree( obj_t o ) {
   regfree( &(BGL_REGEXP_PREF( o )) );
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
