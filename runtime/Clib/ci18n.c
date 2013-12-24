/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/ci18n.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec 19 08:16:32 2013                          */
/*    Last change :  Thu Dec 19 08:16:43 2013 (serrano)                */
/*    Copyright   :  2013 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    C i18n implementation                                            */
/*=====================================================================*/
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <bigloo.h>

#if( BGL_HAVE_UNISTRING )
#  include <unistr.h>
#  include <uninorm.h>
#endif

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_strcoll ...                                                  */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_UNISTRING )
#include <locale.h>

int
bgl_strcoll( obj_t left, obj_t right ) {
   int res;

/*    if( -1 == u8_normcoll( BSTRING_TO_STRING( left ), STRING_LENGTH( left ), */
/* 			  BSTRING_TO_STRING( right ), STRING_LENGTH( right ), */
/* 			  UNINORM_NFC, &res ) ) {                      */
/*       C_FAILURE( "utf8-locale-compare3", strerror( errno ), left ); */
/*       return 0;                                                     */
/*    } else {                                                         */
/*       fprintf( stderr, "res=%d\n", res );                           */
/*       return res;                                                   */
/*    }
 */
   res = u8_strcoll( BSTRING_TO_STRING( left ), BSTRING_TO_STRING( right ) );
   
   return res;
}
#endif
