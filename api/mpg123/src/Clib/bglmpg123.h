/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/mpg123/src/Clib/bglmpg123.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 21 08:20:23 2011                          */
/*    Last change :  Sat May 12 16:18:32 2012 (serrano)                */
/*    Copyright   :  2011-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    mpg123 C prototypes                                              */
/*=====================================================================*/
#ifndef BGL_LIBMPG123
#define BGL_LIBMPG123

extern obj_t bgl_mpg123_decoders();
extern int bgl_mpg123_decode( obj_t, char *, long, long, char *, long );
extern mpg123_handle *bgl_mpg123_new( const char * );
extern long bgl_mpg123_getformat( mpg123_handle * );
extern long bgl_mpg123_info( mpg123_handle * );
extern double bgl_mpg123_getvolume( mpg123_handle * );

#endif
