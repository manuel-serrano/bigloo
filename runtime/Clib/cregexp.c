/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cregexp.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  6 15:44:28 2011                          */
/*    Last change :  Mon Jan 14 15:03:02 2019 (serrano)                */
/*    Copyright   :  2011-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Native posix regular expressions for Bigloo                      */
/*=====================================================================*/
#include <bigloo.h>

#if( BGL_REGEXP_TYPE == BGL_REGEXP_regex )
#  include "cregex.h"
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


