/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/ci18n.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec 19 08:16:32 2013                          */
/*    Last change :  Tue Dec 24 13:20:43 2013 (serrano)                */
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
#  include <locale.h>
#endif

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_strcoll ...                                                  */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_UNISTRING )
BGL_RUNTIME_DEF
int
bgl_strcoll( obj_t left, obj_t right ) {
   return u8_strcoll( BSTRING_TO_STRING( left ), BSTRING_TO_STRING( right ) );
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_utf8_string_locale_upcase ...                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_utf8_string_locale_upcase( obj_t str ) {
   size_t len = STRING_LENGTH( str );
   
#if( BGL_HAVE_UNISTRING )
   uint8_t *src = BSTRING_TO_STRING( str );
   obj_t res = make_string_sans_fill( len );
   uint8_t *buf = BSTRING_TO_STRING( res );
   
   u8_toupper( src, len, uc_locale_language(), NULL, buf, len );
#else
   obj_t res = string_locale_to_bstring_locale_len( BSTRING_TO_STRING( str ), len );
   offset_t i;

   for( i = 0; i < len; i++ ) {
      res[ i ] = toupper( res[ i ] );
   }
#endif   

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_utf8_string_locale_downcase ...                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_utf8_string_locale_downcase( obj_t str ) {
   size_t len = STRING_LENGTH( str );
   
#if( BGL_HAVE_UNISTRING )
   uint8_t *src = BSTRING_TO_STRING( str );
   obj_t res = make_string_sans_fill( len );
   uint8_t *buf = BSTRING_TO_STRING( res );
   
   u8_tolower( src, len, uc_locale_language(), NULL, buf, len );
#else
   obj_t res = string_locale_to_bstring_locale_len( BSTRING_TO_STRING( str ), len );
   offset_t i;

   for( i = 0; i < len; i++ ) {
      res[ i ] = tolower( res[ i ] );
   }
#endif   

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_utf8_string_locale_capitalize ...                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_utf8_string_locale_capitalize( obj_t str ) {
   size_t len = STRING_LENGTH( str );
   
#if( BGL_HAVE_UNISTRING )
   uint8_t *src = BSTRING_TO_STRING( str );
   obj_t res = make_string_sans_fill( len );
   uint8_t *buf = BSTRING_TO_STRING( res );
   
   u8_totitle( src, len, uc_locale_language(), NULL, buf, len );

   return res;
#else
   return string_locale_to_bstring_locale_len( BSTRING_TO_STRING( str ), len );
#endif   
}
