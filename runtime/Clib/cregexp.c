/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cregexp.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  6 15:44:28 2011                          */
/*    Last change :  Thu Apr  8 10:16:04 2021 (serrano)                */
/*    Copyright   :  2011-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Native posix regular expressions for Bigloo                      */
/*=====================================================================*/
#include <bigloo.h>

#if( BGL_REGEXP_TYPE == BGL_REGEXP_regex )
#  include "cregex.h"
#elif( BGL_REGEXP_TYPE == BGL_REGEXP_pcre2 )
#  include "cpcre2.h"
#elif( BGL_REGEXP_TYPE == BGL_REGEXP_pcre )
#  include "cpcre.h"
#endif

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
   re->regexp.capturecount = -1;

   return BREF( re );
}


