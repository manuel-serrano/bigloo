/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/mpg123/src/Clib/bglmpg123.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 21 08:20:23 2011                          */
/*    Last change :  Tue Jan 26 13:44:06 2016 (serrano)                */
/*    Copyright   :  2011-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    mpg123 C prototypes                                              */
/*=====================================================================*/
#ifndef BGL_LIBMPG123
#define BGL_LIBMPG123

extern int bgl_mpg123_error( char *, char *, obj_t );
extern obj_t bgl_mpg123_decoders();
extern int bgl_mpg123_decode( obj_t, char *, long, long, char *, long );
extern mpg123_handle *bgl_mpg123_new( const char * );
extern long bgl_mpg123_getformat( mpg123_handle * );
extern long bgl_mpg123_info( obj_t );
extern double bgl_mpg123_getvolume( mpg123_handle * );
extern obj_t bgl_mpg123_getparam( obj_t, enum mpg123_parms );

#endif
