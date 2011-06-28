/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/mpg123/src/Clib/bglmpg123.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 21 08:20:23 2011                          */
/*    Last change :  Tue Jun 28 08:51:46 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    mpg123 C prototypes                                              */
/*=====================================================================*/
#ifndef BGL_LIBMPG123
#define BGL_LIBMPG123

extern obj_t bgl_mpg123_decoders();
extern int bgl_mpg123_decode( mpg123_handle *, char *, long, char *, long );
extern mpg123_handle *bgl_mpg123_new( const char * );
extern long bgl_mpg123_getformat( mpg123_handle * );
extern long bgl_mpg123_info( mpg123_handle * );
extern long bgl_mpg123_getvolume( mpg123_handle * );

#endif
