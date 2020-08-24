/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cregex.h         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jan 14 15:06:49 2019                          */
/*    Last change :  Thu Aug  8 13:59:30 2019 (serrano)                */
/*    Copyright   :  2019-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Basic C regex support.                                           */
/*=====================================================================*/
#include <regex.h>

#define BGL_REGEXP_REGEX( o ) ((regex_t *)BGL_REGEXP_PREG( o ))

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regfree ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regfree( obj_t o ) {
   regfree( BGL_REGEXP_REGEX( o ) );
   return BUNSPEC;
}
	    
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regmatch ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_regmatch( obj_t re, char *string, bool_t stringp, int beg, int end ) {
   int nmatch = BGL_REGEXP_REGEX( re )->re_nsub + 1;
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
   
   r = regexec( BGL_REGEXP_REGEX( re ), string, nmatch, pmatch, 0 );

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

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regmatch_n ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_regmatch_n( obj_t re, char *string, obj_t vres, int beg, int end ) {
   C_SYSTEM_FAILURE( BGL_IO_PARSE_ERROR, "match_d", "not implemented" );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_regcomp ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_regcomp( obj_t pat, obj_t _, bool_t finalize ) {
   int err;
   obj_t re = bgl_make_regexp( pat );

   BGL_REGEXP_PREG( re ) = (void *)GC_MALLOC_ATOMIC( sizeof( regex_t ) );

   if( !(err = regcomp( BGL_REGEXP_REGEX( re ),
			BSTRING_TO_STRING( pat ),
			REG_EXTENDED )) ) {
      
      BGL_REGEXP( re ).match = bgl_regmatch;
      BGL_REGEXP( re ).match_n = bgl_regmatch_n;
      BGL_REGEXP( re ).free = bgl_regfree;
      BGL_REGEXP( re ).capturecount = -1;
      
      return re;
   } else {
      char *buf;
      int n;
      n = regerror( err, BGL_REGEXP_REGEX( re ), 0, 0);

      buf = alloca( n + 1 );
      regerror( err, BGL_REGEXP_REGEX( re ), buf, n );

      C_SYSTEM_FAILURE( BGL_IO_PARSE_ERROR, "pregexp", buf, pat );
   }
}


