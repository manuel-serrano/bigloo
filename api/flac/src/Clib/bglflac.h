/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/flac/src/Clib/bglflac.h          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 21 08:20:23 2011                          */
/*    Last change :  Mon Sep 19 09:43:15 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    flac C prototypes                                                */
/*=====================================================================*/
#ifndef BGL_LIBFLAC
#define BGL_LIBFLAC

extern FLAC__StreamDecoderInitStatus
bgl_FLAC__stream_decoder_init_stream( FLAC__StreamDecoder *, obj_t );

#define BGL_FLAC_BLIT_STRING( s1, o1, s2, o2, len ) \
   memcpy( ((char *)s2) + o2, ((char *)s1) + o1, len )

#endif
